open Core.Std
open Unix
external unsafe_create_process :
  prog : string -> args : string array -> stdin : file_descr
  -> stdout : file_descr -> stderr : file_descr
  -> int
  = "extended_ml_create_process"

let unsafe_create_process ~prog ~args ~stdin ~stdout ~stderr =
  unsafe_create_process ~prog ~args:(Array.of_list args) ~stdin ~stdout ~stderr

(* In case we ever need it here is a create_process implemented on top of
   [unsafe_create_process]
*)
(*
let create_process ~prog ~args =
  let close_on_err = ref [] in
  try
    let (in_read, in_write) = pipe() in
    close_on_err := in_read :: in_write :: !close_on_err;
    let (out_read, out_write) = pipe() in
    close_on_err := out_read :: out_write :: !close_on_err;
    let (err_read, err_write) = pipe() in
    close_on_err := err_read :: err_write :: !close_on_err;
    set_close_on_exec in_write;
    set_close_on_exec out_read;
    set_close_on_exec err_read;
    let pid = unsafe_create_process
      ~prog
      ~args
      ~stdin:in_read
      ~stdout:out_write
      ~stderr:err_write
    in
    close in_read;
    close out_write;
    close err_write;
    {
      Process_info.pid = pid;
      stdin = in_write;
      stdout = out_read;
      stderr = err_read
    }
  with e ->
    List.iter ~f:(fun fd -> try close fd with _ -> ()) !close_on_err;
    raise e
*)
