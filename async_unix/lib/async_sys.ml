open Core.Std
open Import

module Unix = Unix_syscalls

let argv = Sys.argv
let executable_name = Sys.executable_name

let wrap1 f x1    = In_thread.run (fun () -> f x1)
let wrap2 f x1 x2 = In_thread.run (fun () -> f x1 x2)

let file_exists     = wrap1 Sys.file_exists
let file_exists_exn = wrap1 Sys.file_exists_exn

let when_file_exists ?(poll_delay = sec 0.5) file =
  Deferred.create (fun i ->
    let rec loop () =
      file_exists file >>> function
        | `Yes -> Ivar.fill i ()
        | `No -> upon (after poll_delay) loop
        | `Unknown ->
          failwiths "when_file_exists can not check file" file <:sexp_of< string >>
    in
    loop ())
;;

let when_file_changes ?(poll_delay = sec 0.5) file =
  let last_reported_mtime = ref Time.epoch in
  let (reader, writer) = Pipe.create () in
  let rec loop () =
    Monitor.try_with (fun () -> Unix.stat file)
    >>> fun stat_result ->
    if not (Pipe.is_closed writer) then begin
      begin match stat_result with
      | Error _ -> ()
      | Ok st ->
        let mtime = st.Unix.Stats.mtime in
        if mtime <> !last_reported_mtime then begin
          last_reported_mtime := mtime;
          Pipe.write_without_pushback writer mtime;
        end
      end;
      after poll_delay >>> loop
    end
  in
  loop ();
  reader
;;

let chdir            = wrap1 Sys.chdir
let command          = wrap1 Sys.command
let command_exn      = wrap1 Sys.command_exn
let getcwd           = wrap1 Sys.getcwd
let ls_dir           = wrap1 Sys.ls_dir
let readdir          = wrap1 Sys.readdir
let remove           = wrap1 Sys.remove
let rename           = wrap2 Sys.rename

let wrap_is f =
  fun ?follow_symlinks path -> In_thread.run (fun () -> f ?follow_symlinks path)
;;

let is_directory     = wrap_is Sys.is_directory
let is_directory_exn = wrap_is Sys.is_directory_exn
let is_file          = wrap_is Sys.is_file
let is_file_exn      = wrap_is Sys.is_file_exn

let c_int_size     = Sys.c_int_size
let execution_mode = Sys.execution_mode
let getenv         = Sys.getenv
let getenv_exn     = Sys.getenv_exn
let interactive    = Sys.interactive
let ocaml_version  = Sys.ocaml_version
let os_type        = Sys.os_type
let word_size      = Sys.word_size
