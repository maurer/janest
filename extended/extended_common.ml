open Core.Std

let run_main f =
  try
    f ();
    exit 0
  with e ->
    eprintf "Uncaught exception:\n%s" (Extended_exn.to_string e);
    if Caml.Printexc.backtrace_status () then begin
      prerr_newline ();
      Caml.Printexc.print_backtrace stderr;
    end;
    exit 1

let run_highlighted f =
  if Shell.Ansi.is_color_tty () then begin
    let opened = ref [] in
    try
      if Shell.which "redcat.exe" = None then failwith "executable not in the path";
      let pipe_read,pipe_write = Unix.pipe () in
      opened := [pipe_read;pipe_write];
      let old_stderr = Unix.dup Unix.stderr in
      opened := old_stderr :: !opened;
      Unix.set_close_on_exec pipe_write;
      let (pid:int) = Extended_unix.unsafe_create_process
        ~prog:"redcat.exe"
        ~args:[]
        ~stdin:pipe_read
        ~stdout:Unix.stderr
        ~stderr:Unix.stderr
      in
      Unix.close pipe_read;
      Unix.dup2 ~src:pipe_write ~dst:Unix.stderr;
      Unix.close pipe_write;
      at_exit (fun () ->
        Unix.dup2 ~src:old_stderr ~dst:Unix.stderr;
        ignore (Unix.waitpid ~mode:[] pid :  int*Unix.Process_status.t)
      );
    with e ->
      eprintf "\027[31mFailed to dispatch redcat (%s)!\027[0m\n%!"
        (Extended_exn.to_string_hum e);
      List.iter ~f:(fun fd -> try Unix.close fd with _ -> ()) !opened;
  end;
  run_main f
