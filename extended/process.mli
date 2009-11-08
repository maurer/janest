(**
   Low-level process handling
*)

open Core.Std

exception Timeout of string*string (* stdout * stderr *)

module Command_result : sig
  type t= {
    status: Unix.Process_status.t;
    stdout_tail : string;
    stderr_tail : string
  }
end

val run : ?timeout:Time.Span.t
  -> ?working_dir:string
  -> ?input:string
  -> ?stdoutf:(string -> int -> unit)
  -> ?stderrf:(string -> int -> unit)
  -> ?tail_len:int
  -> prog:string
  -> args:string list
  -> unit
  -> Command_result.t
