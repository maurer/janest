(** A module for reading files.  *)

open Std_internal

(* This is here to fix a bug in SMP kernels when using lseek and LargeFile.  LargeFile use
   is therefore disabled *)

module LargeFile = Pervasives
type int64 = int
module Int64 = struct let add = (+) let sub = (-) let zero = 0 let one = 1 end

let max_null_retries = 20

(** The type of a linebuf. *)
type t = { mutable file: in_channel;        (** The channel we maintain. *)
           mutable pos: int64;              (** Current file position. *) 
           mutable line: int;               (** Current line position. *)
           mutable closed: bool;            (** Closed or open. *)
           mutable inode: int;              (** The inode number of the current file *)
           name: string;                    (** File name. *)
           buf: Buffer.t;
           follow_deletes: bool;            (** close and reopen the file if it is 
                                                deleted and recreated. *)
           close_on_eof: bool;              (** whether to close the file on EOF.
                                                Useful for NFS filesystems that
                                                don't promptly notify you when files
                                                change unless you close and reopen *)
           null_hack: bool;                 (** Close and reopen file when you read
                                                nulls from the file *)
           mutable null_retries: int;       (** number of times we've tried to
                                                reread this file in response to a
                                                given null reading. *)
         }

type error_type = Null_retry | Too_many_nulls | Exception of string * exn

type result = 
    Success of int * string
  | Nothing_available
  | Error of error_type
  | Fatal_error of string * exn

(** Open a linebuffer from the passed filename. *)
let open_linebuf ?(close_on_eof=false) ?(null_hack=false) ?(follow_deletes=false) fname =
  let {Unix.st_ino=inode} = Unix.stat fname in
  { file = open_in_bin fname;
    pos = Int64.zero;
    line = 1;
    name = fname;
    closed = false;
    buf = Buffer.create 0;
    close_on_eof = close_on_eof;
    follow_deletes = follow_deletes;
    inode = inode;
    null_hack = null_hack;    
    null_retries = 0;
  }

(** Close the linebuf. *)
let close_linebuf lbuf =
  if not lbuf.closed then
    close_in lbuf.file;
  lbuf.closed <- true

let closed_linebuf t = t.closed

let possibly_reopen lbuf =
  if lbuf.closed then
    begin
      lbuf.file <- open_in_bin lbuf.name;
      lbuf.closed <- false;
      LargeFile.seek_in lbuf.file lbuf.pos;
    end
;;

let reopen_if_deleted lbuf =    
  try
    let {Unix.st_ino=inode} = Unix.stat lbuf.name in
    if lbuf.inode <> inode then begin
      try
        close_linebuf lbuf;
        lbuf.file <- open_in_bin lbuf.name;
        lbuf.inode <- inode;
        lbuf.closed <- false;
        lbuf.pos <- Int64.zero;
        `Success
      with exn -> 
        `Error ("reopen_if_deleted: closing and reopening the file failed", exn)
    end else
      `Success
  with 
    Unix.Unix_error (Unix.ENOENT, _, _) -> `Success
  | exn -> `Error ("reopen_if_deleted: stat failed", exn)

let has_nulls s = 
  try
    for i = 0 to String.length s - 1 do
      if s.[i] = '\000' then raise Exit
    done;
    false
  with Exit -> true

exception Null_found
exception Too_many_null_retries

(** returns [None] if no full line is available, [Some line] otherwise. *)
let try_read_lnum_verbose lbuf =
  try
    possibly_reopen lbuf;
    let line = input_line lbuf.file in
    if lbuf.null_hack then
      if has_nulls line then 
        (if lbuf.null_retries > max_null_retries then raise Too_many_null_retries;
         close_linebuf lbuf; 
         lbuf.null_retries <- lbuf.null_retries + 1;
         raise Null_found)
      else
        lbuf.null_retries <- 0;
    
    lbuf.pos <- LargeFile.pos_in lbuf.file;
    let last_char =
      try LargeFile.seek_in lbuf.file (Int64.sub lbuf.pos Int64.one); input_char lbuf.file
      with End_of_file -> 
        failwith "Linebuf.try_read_lnum: unexpected EOF, file may have been truncated"
    in
    LargeFile.seek_in lbuf.file lbuf.pos;
    if last_char = '\n'
    then (* we're at the end of the line *)
      let lnum = lbuf.line in
      lbuf.line <- lbuf.line + 1;
      let line =
        if String.length line > 0 && String.nget line (-1) = '\r'
        then String.slice line 0 (-1) else line
      in
      if Buffer.length lbuf.buf > 0 then
        let oldline = Buffer.contents lbuf.buf in
        Buffer.clear lbuf.buf;
        Success (lnum,oldline ^ line)
      else
        Success (lnum,line)
    else
      begin
        Buffer.add_string lbuf.buf line;
        Nothing_available
      end
  with
  | Null_found -> Error Null_retry
  | Too_many_null_retries -> Error Too_many_nulls
  | End_of_file ->
      begin
        try
          if lbuf.close_on_eof then close_linebuf lbuf;
          if lbuf.follow_deletes then 
            match reopen_if_deleted lbuf with
              `Success -> Nothing_available
            | `Error (s, e) -> Error (Exception (s, e))
          else
            Nothing_available
        with exn -> Fatal_error ("error while handling end of file", exn)
      end
  | exn -> Fatal_error ("main loop", exn)

let try_read_lnum lbuf = 
  match try_read_lnum_verbose lbuf with
  | Nothing_available -> None
  | Error Null_retry -> 
      eprintf "<<<<NULLFOUND:%s>>>>\n%!" lbuf.name;
      None
  | Success (lnum, line) -> Some (lnum, line)
  | Error Too_many_nulls -> failwith "Too many null retries"
  | Error (Exception (e, exn)) ->
      failwith (sprintf "error in linebuf: %s (exn: %s)" e (exn_to_string exn))
  | Fatal_error (e, exn) ->
      failwith (sprintf "fatal error in linebuf: %s (exn: %s)" e (exn_to_string exn))

let try_read lbuf =
  opt_map snd (try_read_lnum lbuf)

let read_frequency = Time.Span.of_sec 0.01    

let rec read lbuf =
  match try_read lbuf with
  | Some line -> line
  | None ->
      Time.pause read_frequency;
      read lbuf

let tail lbuf =
  let file_size = LargeFile.in_channel_length lbuf.file in
  if file_size = Int64.zero then () else
    begin
      lbuf.pos <- Int64.sub file_size Int64.one;
      LargeFile.seek_in lbuf.file lbuf.pos;
      ignore (read lbuf)
    end

let unsafe_tail lbuf =
  lbuf.pos <- LargeFile.in_channel_length lbuf.file;
  LargeFile.seek_in lbuf.file lbuf.pos;
  ignore (try_read lbuf)

