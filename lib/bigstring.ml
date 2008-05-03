(*pp cpp *)

open Printf
open Unix
open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

exception IOError of int * exn

external init : unit -> unit = "bigstring_init_stub"

let () =
  Callback.register_exception "Bigstring.End_of_file" End_of_file;
  Callback.register_exception "Bigstring.IOError" (IOError (0, Exit));
  init ()

let create n = Array1.create Bigarray.char c_layout n
let length bstr = Array1.dim bstr
external is_mmapped : t -> bool = "bigstring_is_mmapped_stub" "noalloc"

let check_args ~loc bstr ~pos ~len =
  if pos < 0 then invalid_arg (loc ^ ": pos < 0");
  if len < 0 then invalid_arg (loc ^ ": len < 0");
  let bstr_len = length bstr in
  if bstr_len < pos + len then
    invalid_arg (sprintf "Bigstring.%s: length(bstr) < pos + len" loc)

let get_opt_pos ~loc ~var = function
  | Some pos ->
      if pos < 0 then invalid_arg (sprintf "Bigstring.%s: %s < 0" loc var);
      pos
  | None -> 0

let get_opt_len bstr ~pos = function
  | Some len -> len
  | None -> length bstr - pos

let check_min_len ~loc ~len = function
  | None -> 0
  | Some min_len ->
      if min_len > len then (
        let msg = sprintf "%s: min_len (%d) > len (%d)" loc min_len len in
        invalid_arg msg);
      if min_len < 0 then (
        let msg = sprintf "%s: min_len (%d) < 0" loc min_len in
        invalid_arg msg);
      min_len

let sub ?(pos = 0) ?len bstr =
  let len =
    match len with
    | None -> length bstr - pos
    | Some len -> len
  in
  Array1.sub bstr pos len


(* Blitting *)

external unsafe_blit :
  src_pos : int -> src : t -> dst_pos : int -> dst : t -> len : int -> unit
  = "bigstring_blit_stub"

let blit ?src_pos ~src ?dst_pos ~dst len =
  let loc = "blit" in
  let src_pos = get_opt_pos ~loc ~var:"src_pos" src_pos in
  let dst_pos = get_opt_pos ~loc ~var:"dst_pos" dst_pos in
  if len < 0 then invalid_arg "Bigstring.blit: len < 0"
  else if len = 0 then (
    if src_pos > length src then
      invalid_arg "Bigstring.blit: src_pos > src_len";
    if dst_pos > length dst then
      invalid_arg "Bigstring.blit: dst_pos > dst_len")
  else (
    if src_pos + len > length src then
      invalid_arg "Bigstring.blit: src_pos + len > src_len"
    else if dst_pos + len > length dst then
      invalid_arg "Bigstring.blit: dst_pos + len > dst_len"
    else unsafe_blit ~src_pos ~src ~dst_pos ~dst ~len)

external unsafe_blit_string_bigstring :
  src_pos : int -> string -> dst_pos : int -> t -> len : int -> unit
  = "bigstring_blit_string_bigstring_stub" "noalloc"

let blit_string_bigstring ?src_pos str ?dst_pos bstr ~len =
  let loc = "blit_string_bigstring" in
  let src_pos = get_opt_pos ~loc ~var:"src_pos" src_pos in
  let dst_pos = get_opt_pos ~loc ~var:"dst_pos" dst_pos in
  if len < 0 then invalid_arg "Bigstring.blit_string_bigstring: len < 0"
  else if len = 0 then (
    if src_pos > String.length str then
      invalid_arg "Bigstring.blit_string_bigstring: src_pos > str_len";
    if dst_pos > length bstr then
      invalid_arg "Bigstring.blit_string_bigstring: src_pos > bstr_len")
  else (
    if src_pos + len > String.length str then
      invalid_arg "Bigstring.blit_string_bigstring: src_pos + len > str_len"
    else if dst_pos + len > length bstr then
      invalid_arg "Bigstring.blit_string_bigstring: src_pos + len > bstr_len"
    else unsafe_blit_string_bigstring ~src_pos str ~dst_pos bstr ~len)

let of_string ?(pos = 0) ?len str =
  let len =
    match len with
    | Some len -> len
    | None -> String.length str - pos
  in
  let bstr = create len in
  blit_string_bigstring ~src_pos:pos str bstr ~len;
  bstr

external unsafe_blit_bigstring_string :
  src_pos : int -> t -> dst_pos : int -> string -> len : int -> unit
  = "bigstring_blit_bigstring_string_stub" "noalloc"

let blit_bigstring_string ?src_pos bstr ?dst_pos str ~len =
  let loc = "blit_bigstring_string" in
  let src_pos = get_opt_pos ~loc ~var:"src_pos" src_pos in
  let dst_pos = get_opt_pos ~loc ~var:"dst_pos" dst_pos in
  if len < 0 then invalid_arg "Bigstring.blit_bigstring_string: len < 0"
  else if len = 0 then (
    if src_pos > length bstr then
      invalid_arg "Bigstring.blit_bigstring_string: src_pos > bstr_len";
    if dst_pos > String.length str then
      invalid_arg "Bigstring.blit_bigstring_string: src_pos > str_len")
  else (
    if src_pos + len > length bstr then
      invalid_arg "Bigstring.blit_bigstring_string: src_pos + len > bstr_len"
    else if dst_pos + len > String.length str then
      invalid_arg "Bigstring.blit_bigstring_string: src_pos + len > str_len"
    else unsafe_blit_bigstring_string ~src_pos bstr ~dst_pos str ~len)

let to_string ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"to_string" bstr ~pos ~len;
  let str = String.create len in
  blit_bigstring_string ~src_pos:pos bstr str ~len;
  str


(* Input functions *)

external unsafe_read :
  min_len : int -> file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_stub"

let read ?min_len fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "read" in
  check_args ~loc bstr ~pos ~len;
  let min_len = check_min_len ~loc ~len min_len in
  unsafe_read ~min_len fd ~pos ~len bstr

let really_read fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  ignore (read ~min_len:len fd ~pos ~len bstr)

external unsafe_really_recv :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_recv_stub"

let really_recv sock ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_recv" bstr ~pos ~len;
  unsafe_really_recv sock ~pos ~len bstr

external unsafe_read_assume_nonblocking :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_assume_nonblocking_stub"

let read_assume_nonblocking fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"read_assume_nonblocking" bstr ~pos ~len;
  unsafe_read_assume_nonblocking fd ~pos ~len bstr

external unsafe_input :
  min_len : int -> in_channel -> pos : int -> len : int -> t -> int
  = "bigstring_input_stub"

let input ?min_len ic ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "input" in
  check_args ~loc bstr ~pos ~len;
  let min_len = check_min_len ~loc ~len min_len in
  unsafe_input ~min_len ic ~pos ~len bstr

let really_input ic ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_input" bstr ~pos ~len;
  ignore (unsafe_input ~min_len:len ic ~pos ~len bstr)

external unsafe_output :
  min_len : int -> out_channel -> pos : int -> len : int -> t -> int
  = "bigstring_output_stub"

let output ?min_len oc ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "output" in
  check_args ~loc bstr ~pos ~len;
  let min_len = check_min_len ~loc ~len min_len in
  unsafe_output oc ~min_len ~pos ~len bstr

let really_output oc ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_output" bstr ~pos ~len;
  ignore (unsafe_output oc ~min_len:len ~pos ~len bstr)


(* Output functions *)

external unsafe_really_write :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_write_stub"

let really_write fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_write" bstr ~pos ~len;
  unsafe_really_write fd ~pos ~len bstr

#if defined(MSG_NOSIGNAL) || defined(__linux__)
external unsafe_really_send_no_sigpipe :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_send_no_sigpipe_stub"

let really_send_no_sigpipe fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_send_no_sigpipe" bstr ~pos ~len;
  unsafe_really_send_no_sigpipe fd ~pos ~len bstr
#else
#warning "MSG_NOSIGNAL not defined; really_send_no_sigpipe"
#warning "not implemented."
#warning "Try compiling on Linux?"
#endif

#if defined(MSG_NOSIGNAL) || defined(__linux__)
(* CRv2 SW: It seems complex to have the underyling C code return the option.
   Why not leave the C code simple (returning an int) and translate to an
   option on the ML side?
   CRv2 mmottl: oh well, small deal.  The current way saves an
   intermediate function on the ML side and only adds two short lines
   to the C-code.
 *)
external unsafe_send_nonblocking_no_sigpipe :
  file_descr -> pos : int -> len : int -> t -> int option
  = "bigstring_send_nonblocking_no_sigpipe_stub"

let send_nonblocking_no_sigpipe fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"send_nonblocking_no_sigpipe" bstr ~pos ~len;
  unsafe_send_nonblocking_no_sigpipe fd ~pos ~len bstr
#else
#warning "MSG_NOSIGNAL not defined; really_send_no_sigpipe"
#warning "not implemented."
#warning "Try compiling on Linux?"
#endif

external unsafe_write :
  file_descr -> pos : int -> len : int -> t -> int = "bigstring_write_stub"

let write fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"write" bstr ~pos ~len;
  unsafe_write fd ~pos ~len bstr

external unsafe_write_assume_nonblocking :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_write_assume_nonblocking_stub"

let write_assume_nonblocking fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"write_assume_nonblocking" bstr ~pos ~len;
  unsafe_write_assume_nonblocking fd ~pos ~len bstr

external unsafe_writev :
  file_descr -> t Unix_ext.IOVec.t array -> int -> int
  = "bigstring_writev_stub"

let get_iovec_count loc iovecs = function
  | None -> Array.length iovecs
  | Some count ->
      if count < 0 then invalid_arg (loc ^ ": count < 0");
      let n_iovecs = Array.length iovecs in
      if count > n_iovecs then invalid_arg (loc ^ ": count > n_iovecs");
      count

let writev fd ?count iovecs =
  let count = get_iovec_count "writev" iovecs count in
  unsafe_writev fd iovecs count

external unsafe_writev_assume_nonblocking :
  file_descr -> t Unix_ext.IOVec.t array -> int -> int
  = "bigstring_writev_assume_nonblocking_stub"

let writev_assume_nonblocking fd ?count iovecs =
  let count = get_iovec_count "writev_nonblocking" iovecs count in
  unsafe_writev_assume_nonblocking fd iovecs count


(* Memory mapping *)

let map_file ~shared fd n = Array1.map_file fd Bigarray.char c_layout shared n


(* File I/O *)

let close_on_exc fd exc =
  begin try close fd with _ -> () end;
  raise exc

let load_file ?(pos = 0) ?len fname =
  if pos < 0 then invalid_arg "Bigstring.load_file: pos < 0";
  let fd = openfile fname [O_RDONLY] 0o600 in
  try
    let len =
      let file_size = (fstat fd).st_size in
      match len with
      | None ->
          let len = file_size - pos in
          if len < 0 then invalid_arg "Bigstring.load_file: pos > file size";
          len
      | Some len ->
          if len < 0 then invalid_arg "Bigstring.load_file: len < 0";
          if pos + len > file_size then
            invalid_arg "Bigstring.load_file: pos + len > file size";
          len
    in
    ignore (lseek fd pos SEEK_SET);
    let bstr = create len in
    really_read fd ~len bstr;
    bstr
  with exc -> close_on_exc fd exc

let store_file
      ?(create = true) ?(exclusive = true) ?(append = true) ?(perm = 0o600)
      fname ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"store_file" bstr ~pos ~len;
  let flags = [O_WRONLY] in
  let flags = if create then O_CREAT :: flags else flags in
  let flags = if exclusive then O_EXCL :: flags else flags in
  let flags = if append then O_APPEND :: flags else flags in
  let fd = openfile fname flags perm in
  try really_write fd ~pos ~len bstr
  with exc -> close_on_exc fd exc


(* input and output, linux only *)
#if defined(MSG_NOSIGNAL) || defined(__linux__)

external unsafe_sendmsg_nonblocking_no_sigpipe :
  file_descr -> t Unix_ext.IOVec.t array -> int -> int
  = "bigstring_sendmsg_nonblocking_no_sigpipe_stub"

let sendmsg_nonblocking_no_sigpipe fd ?count iovecs =
  let count = get_iovec_count "sendmsg_nonblocking_no_sigpipe" iovecs count in
  unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count
#else
#warning "MSG_NOSIGNAL not defined; bigstring_send{,msg}_noblocking_no_sigpipe"
#warning "not implemented."
#warning "Try compiling on Linux?"
#endif
