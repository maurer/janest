(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(* WARNING!  This module is undergoing some big dev work internally, use with caution *)

TYPE_CONV_PATH "Shell"
open Core.Std

module Ansi = struct
    (** Color printing in terminals  *)

  let capable = lazy (Unix.isatty Unix.stdout && Sys.getenv "TERM" = Some "xterm")

  type style = [ `Underline | `Red   | `Green ]
  let style_to_string : style -> string = function
    | `Underline -> "\027[4m"
    | `Red       -> "\027[31m"
    | `Green     -> "\027[32m"

  let fprintf style channel fmt =
    if Lazy.force capable then
      fprintf
        channel
        ( "%s" ^^ fmt ^^ "\027[0m%!")
        (style_to_string style)
    else
      fprintf
        channel
        (fmt ^^ "%!")

  let eprintf style fmt = fprintf style stderr fmt
  let printf  style fmt = fprintf style stdout fmt

  let output style chan s pos len =
    let capable = Lazy.force capable in
    if capable then
      output_string chan (style_to_string style);
    output chan s pos len;
    if capable then
      output_string chan "\027[0m";
    flush chan

  let is_color_tty () = Lazy.force capable

end

(* set cwd to [dir] while running [f] *)
(* merge with Process*)
let pushd dir ~f =
  let owd = Sys.getcwd () in
  protectx
    ~finally: (fun _ -> Sys.chdir owd)
    ~f:(fun x ->
          Sys.chdir dir;
          f x
       )

module Process = struct

  type status =  [ `Timeout of Time.Span.t | Unix.Process_status.t ] with sexp_of

  type t = {
      program   : string;
      arguments : string list;
  } with sexp_of

  type result = {
    command : t;
    status  : [ `Timeout of Time.Span.t | Unix.Process_status.t ];
    stdout  : string;
    stderr  : string
  } with sexp_of

  exception Failed of result with sexp

  let to_string {program=prog; arguments=args} =
    let f s =
      if not (String.contains s ' ') &&
        not (String.contains s '"') then
        s
      else
        sprintf "%S" s
    in
    String.concat ~sep:" " (List.map ~f (prog::args))

  let status_to_string = function
  | #Unix.Process_status.t as s ->
      Unix.Process_status.to_string_hum s
  | `Timeout _ -> "Timed out"

  let format_failed c =
    String.concat ~sep:" " ["Command failed:";
                            to_string c.command;
                            "Exit status:";
                            status_to_string c.status;
                            "stderr:";
                            c.stderr]

  module Defaults = struct
    let timeout = ref None
    let verbose = ref false
    let echo = ref false
  end

  let set_defaults ?timeout ?verbose ?echo () =
    Option.iter ~f:(fun v -> Defaults.verbose := v) verbose;
    Option.iter ~f:(fun v -> Defaults.timeout := v) timeout;
    Option.iter ~f:(fun v -> Defaults.echo := v)    echo


  let cmd program arguments = {
    program   = program;
    arguments = arguments;
  }

  let shell s =
    {
      program   = "/bin/sh";
      arguments = [ "-c" ; s ]
    }

  (* Passes the command to bash... *)
  let remote ?user ~host cmd   =
    let url = match user with
      | None      -> host
      | Some user -> user ^"@"^host
    in
    { program   = "ssh";
      arguments =
      (* magic invocation to avoid asking for password. Otherwise ssh will hunt you down*)
        "-o"::"PreferredAuthentications=hostbased,publickey,gssapi-with-mic"
        :: "-o"::"StrictHostKeyChecking=no"
        :: url
        :: "--"
        :: cmd.program::cmd.arguments
    }


  type 'res acc =
      {
        add   : string -> int -> unit;
        flush : unit -> 'res
      }

  type 'res reader = unit -> 'res acc

  let run' ?timeout ?working_dir ?verbose ?echo ?input ~f cmd  =
    let verbose = Option.value verbose ~default:(!Defaults.verbose) in
    let timeout = Option.value timeout ~default:(!Defaults.timeout) in
    let echo = Option.value echo       ~default:(!Defaults.echo)    in
    if echo then
      Ansi.printf `Underline "Shell: %s\n" (to_string cmd);
    let stderrf =
      if verbose then
        (fun s len -> Ansi.output `Red stderr s 0 len)
      else
        (fun _s _len -> ())
    and stdoutf =
      if verbose then
        (fun s len ->
          Ansi.output `Green stderr s 0 len;
          f s len
        )
      else f
    in
    try
      Process.run ?timeout ?input ?working_dir
        ~stdoutf
        ~stderrf
        ~prog:cmd.program
        ~args:cmd.arguments
        ()
    with Process.Timeout (stderr,stdout) ->
      raise (Failed
                {
                  command = cmd;
                  status  = `Timeout (Option.value_exn timeout);
                  stdout  = stdout;
                  stderr  = stderr
                }
      )

  let run ?timeout ?working_dir ?expect ?verbose ?echo ?input cmd (reader:_ reader) =
    let expect = Option.value expect ~default:[0] in
    let acc = reader () in
    try
      let r = run' ?timeout ?working_dir ?verbose ?echo ?input cmd ~f:acc.add in
      let module Res = Process.Command_result in
      match r.Res.status with
      | `Exited i when List.mem i ~set:expect -> acc.flush ()
      | status -> raise (Failed
                            {
                              command = cmd;
                              status  = (status :> status);
                              stderr  = r.Res.stdout_tail;
                              stdout  = r.Res.stderr_tail
                            }
        )
    with
     (* Support for early exit *)
    | Exit -> acc.flush ()

  let test ?timeout ?working_dir ?verbose ?echo ?input ?(true_v=[0]) ?(false_v=[1]) cmd  =
    let r = run' ?timeout ?working_dir ?verbose ?echo ?input cmd ~f:(fun _ _ -> ()) in
    let module Res = Process.Command_result in
    match r.Res.status with
    | `Exited i when List.mem i ~set:true_v -> true
    | `Exited i when List.mem i ~set:false_v -> false
    | status -> raise (Failed
                          {
                            command = cmd;
                            status  = (status :> status);
                            stderr  = r.Res.stdout_tail;
                            stdout  = r.Res.stderr_tail
                          }
      )

  (*
  let test ?timeout ?working_dir ?(expect_true=[0]) ?expect_false ?verbose
      ?echo ?input prog args =
    try
      really_run ~timeout ~working_dir ~expect:(Some expect_true) ~verbose
        ~echo ~input ~prog ~args ~stdout:drop;
      true
    with Failed_command {status=`Exited exit_code} as exn ->
      match expect_false with
      | None   -> false
      | Some l -> if List.mem exit_code ~set:l then false else raise exn 
  *)

  let discard () = {
    add   = (fun _ _ -> ());
    flush = (fun () -> ());
  }

  let content () =
    let buffer = Buffer.create 16 in
    {
      add   = (fun s len -> Buffer.add_substring buffer s 0 len);
      flush = (fun () -> Buffer.contents buffer)
    }

  let lines () =
    let strip_trailing_nl str =
      if str <> "" && str.[String.length str - 1] = '\n'
      then String.drop_suffix str 1
      else str in
    let buffer = Buffer.create 16 in
    {
      add   = (fun s len -> Buffer.add_substring buffer s 0 len);
      flush = (fun () -> String.split ~on:'\n'
        (strip_trailing_nl (Buffer.contents buffer)))
    }

  let head () =
    let buffer = Buffer.create 16 in
    {
      add   = (fun s len ->
        begin try
          let e = String.index s '\n' in
          if e < len then begin
            Buffer.add_substring buffer s 0 e;
            raise Exit
          end
          with
          | Not_found -> ()
        end;
        Buffer.add_substring buffer s 0 len
      );
      flush = (fun () -> Buffer.contents buffer)
    }
end

type 'a with_process_flags =
    ?timeout:Time.Span.t option
  -> ?working_dir:string (* rename to run_in? *)
  -> ?verbose:bool
  -> ?echo:bool
  -> ?input:string
  -> 'a

type 'a with_run_flags =
     (** Defaults to [0]*)
    (?expect:int list -> 'a) with_process_flags

type 'a with_test_flags =
    (?true_v:int list -> ?false_v:int list ->'a ) with_process_flags

type 'a cmd = string -> string list -> 'a

type ('a,'ret) sh_cmd = (('a, unit, string,'ret) format4 -> 'a)

let run_gen reader ~timeout ~working_dir ~expect ~verbose ~echo ~input cmd args =
  Process.run
    ?timeout
    ?working_dir
    ?expect
    ?verbose
    ?echo
    ?input
    (Process.cmd cmd args)
    reader

let run ?timeout ?working_dir ?verbose ?echo ?input ?expect cmd args =
  run_gen Process.discard ~timeout ~working_dir ~expect ~verbose ~echo ~input cmd args

let run_lines ?timeout ?working_dir ?verbose ?echo ?input ?expect cmd args =
  run_gen Process.lines ~timeout ~working_dir ~expect ~verbose ~echo ~input cmd args

let run_one ?timeout ?working_dir ?verbose ?echo ?input ?expect cmd args =
  run_gen Process.head ~timeout ~working_dir ~expect ~verbose ~echo ~input cmd args

let run_full ?timeout ?working_dir ?verbose ?echo ?input ?expect cmd args =
  run_gen Process.content ~timeout ~working_dir ~expect ~verbose ~echo ~input cmd args

let test ?timeout ?working_dir ?verbose ?echo ?input ?true_v ?false_v cmd args =
  Process.test
    ?timeout
    ?working_dir
    ?verbose
    ?echo
    ?input
    ?true_v
    ?false_v
    (Process.cmd cmd args)

let sh_gen reader ~timeout ~working_dir ~expect ~verbose ~echo ~input fmt =
  ksprintf (fun s -> Process.run
    ?timeout
    ?working_dir
    ?expect
    ?verbose
    ?echo
    ?input
    (Process.shell s)
    reader
  )
    fmt

let sh ?timeout ?working_dir ?verbose ?echo ?input ?expect fmt =
  sh_gen Process.discard ~timeout ~working_dir ~expect ~verbose ~echo ~input fmt

let sh_one ?timeout ?working_dir ?verbose ?echo ?input ?expect fmt =
  sh_gen Process.head ~timeout ~working_dir ~expect ~verbose ~echo ~input fmt

let sh_lines ?timeout ?working_dir ?verbose ?echo ?input ?expect fmt =
  sh_gen Process.lines ~timeout ~working_dir ~expect ~verbose ~echo ~input fmt

let sh_full ?timeout ?working_dir ?verbose ?echo ?input ?expect fmt =
  sh_gen Process.content ~timeout ~working_dir ~expect ~verbose ~echo ~input fmt

let sh_test ?timeout ?working_dir ?verbose ?echo ?input ?true_v ?false_v fmt =
  ksprintf (fun s -> Process.test
    ?timeout
    ?working_dir
    ?verbose
    ?echo
    ?input
    ?true_v
    ?false_v
    (Process.shell s)
  ) fmt


(* "real" switches between real and effective uids. sudo sets both real and
   effective uids, so this will not work, though you should be able to use
   $SUDO_UID *)
let whoami ?(real=false) () =
  let uid = if real then Unix.getuid () else Unix.geteuid () in
  try (Unix.getpwuid uid).Unix.pw_name
  with Not_found -> failwith "unable to determine username"


let on_error ~f ~finally =
  try
    f ()
  with e ->
    List.iter ~f:(fun f -> try f() with _ -> ()) finally;
    raise e

(*We are overriding protectx to discard its second exception. This is the
  correct behaviour for this library because both exception are usually
  linked and this does nothing to help the user of the library. *)
let protectx ~f x ~(finally : 'a -> unit) =
  let res =
    try f x
    with exn ->
      (try finally x with _ -> ());
      raise exn
  in
  finally x;
  res

(* TODO: export the env variable...*)
let with_popen s ~f =
  let ps = Unix.open_process_full ~env:[||] s in
  let res =
    on_error ~f:(fun () -> f ps)
      ~finally:[fun () -> ignore (Unix.close_process_full ps)]
  in
  res,Unix.close_process_full ps

let get_path () =
  Sys.getenv "PATH"
|! Option.map ~f:(String.split ~on:':')
|! Option.value  ~default:[]


let quote = Filename.quote

let (^/) x y = Filename.normalize (x ^/ y)


let rm = Sys.remove
let cp src tgt = run "cp" [src;tgt]

let is_executable  path =
  try
    let stat = Unix.stat path in
    stat.Unix.st_kind = Unix.S_REG (* Is file *)
    && (stat.Unix.st_perm land 0o111 > 0) (* Is executable*)
  with
  | Unix.Unix_error ((Unix.ENOENT|Unix.ENOTDIR), _, _) -> false (* File not found *)

let which bin =
  if not (String.contains bin '/') then
    let rec loop = function
      | [] -> None
      | h::t ->
          let file = h ^/ bin in
          if is_executable file then
            Some file
          else
            loop t
   in loop (get_path ())
  else begin
    if not (is_executable bin) then
      None
    else
      Some bin
  end

(* editors list approximately ordered by ascending new-user-hostility *)
let get_editor () =
  let editors = [Option.value (Sys.getenv "EDITOR") ~default:"nano";
                 "emacs"; "vim"; "vi"; "ed"] in
  let rec first_valid = function
    | [] -> None
    | ed::eds -> match which ed with
      | Some _ as e -> e
      | None -> first_valid eds
  in first_valid editors

let home () =
  try
    (Unix.getpwuid (Unix.geteuid ())).Unix.pw_dir
  with Not_found -> failwith "unable to determine home directory"

let get_group_names () =
  Unix.getgroups () |! Array.to_list |!
      List.map ~f:(fun gid -> (Unix.getgrgid gid).Unix.gr_name)

let in_group group_name =
  List.exists (get_group_names ()) ~f:((=) group_name)

let hostname = Unix.gethostname

let file_kind f = (Unix.lstat f).Unix.st_kind

(* Sys.file_exists fails on dead links*)
let file_exists f =
  try
    ignore (Unix.lstat f:Unix.stats);
    true
  with
  | Unix.Unix_error ((Unix.ENOENT|Unix.ENOTDIR), _, _) -> false (* File not found *)

let is_directory ?(unlink=false) path =
  try
    let stat = if unlink then
      Unix.stat path
    else
      Unix.lstat path
    in
    stat.Unix.st_kind = Unix.S_DIR
  with
  | Unix.Unix_error ((Unix.ENOENT|Unix.ENOTDIR), _, _) -> false (* File not found *)

let is_file ?(unlink=false) path =
  try
    let stat = if unlink then
      Unix.stat path
    else
      Unix.lstat path
    in
    stat.Unix.st_kind = Unix.S_REG
  with
  | Unix.Unix_error ((Unix.ENOENT|Unix.ENOTDIR), _, _) -> false (* File not found *)


let ls dir = Sys.readdir dir
  |! Array.to_list
  |! List.sort ~cmp:Extended_string.collate

let rec mkdir_p ?(perm=0o777) = function
    (** Filename.dirname will converge towards "." or "/". If those don't exist
        we are in a pickle...*)
  | "/"
  | "." -> ()
  | d   ->
      try
        Unix.mkdir ~perm d
      with
      | Unix.Unix_error (Unix.EEXIST,_,_) when is_directory ~unlink:true d -> ()
      | Unix.Unix_error (Unix.ENOENT,_,_) ->
          mkdir_p ~perm (Filename.dirname d);
          (* Avoid really weird errors with concurrent runs...*)
          mkdir_p ~perm d;
      | Unix.Unix_error (err,_,arg) ->
          raise (Unix.Unix_error (err,"mkdir_p",arg))

(* Deal with atomicity  *)
let copy_file ?(overwrite=false) ?perm ~src ~dst =
  let perm = match perm with
    | Some p -> p
    | None -> (Unix.lstat src).Unix.st_perm
  in
  let out_mode =
    if overwrite then
      [ Unix.O_WRONLY; Unix.O_NOCTTY; Unix.O_CREAT; Unix.O_TRUNC ]
    else
      [ Unix.O_WRONLY; Unix.O_NOCTTY; Unix.O_CREAT; Unix.O_EXCL ]
  in
  protectx (Unix.openfile src ~mode:[ Unix.O_RDONLY; Unix.O_NOCTTY ] ~perm:0)
    ~f:(fun infh ->
          protectx (Unix.openfile dst ~mode:out_mode ~perm)
            ~f:(fun outfh ->
                  let buflen = 4096 in
                  let buf = String.create buflen in
                  let rec loop () =
                    let rlen = Unix.read infh ~buf ~pos:0 ~len:buflen in
                    if rlen <> 0 then
                      let wlen = Unix.write outfh ~buf ~pos:0 ~len:rlen in
                      if rlen <> wlen then
                        failwithf "Short write: tried to write %d bytes, \
                                   only wrote %d bytes" rlen wlen ();
                      loop ()
                  in
                  loop ();
               )
            ~finally:Unix.close
       )
    ~finally:Unix.close

(** CR till for till: delete... *)
let remote ?timeout ?working_dir ?verbose ?echo ?input ?expect ?(user=whoami ())
    ~host command args
    =
  run ?timeout  ?working_dir ?expect ?verbose ?echo ?input "ssh"
    (* magic invocation to avoid asking for password. Otherwise ssh will hunt you down*)
    ("-o"::"PreferredAuthentications=hostbased,publickey,gssapi-with-mic"
      :: "-o"::"StrictHostKeyChecking=no"
      ::(user^"@"^host)
      ::command::args)

let ssh ~user ~host command = run "ssh" [user^"@"^host;command]

let scp ?(recurse=false) ~user ~host f t =
  let args = [f;user^"@"^host^":"^t] in
  let args = if recurse then "-r"::args else args in
  run "scp" args

let echo fmt = printf (fmt ^^ "\n%!")

let warnf fmt =
  ksprintf
    (fun s ->
       String.split s ~on:'\n'
     |!List.iter ~f:(eprintf "warning: %s\n");
     flush stderr
    ) fmt

let email to_address cc_address_lst subject body =
  let cc_string =
    if List.is_empty cc_address_lst then
      ""
    else
      let cc_addresses = String.concat ~sep:"," cc_address_lst in
      sprintf "-c %s " cc_addresses
  in
  sh ~input:body "mail %s -s '%s' %s" cc_string subject to_address
