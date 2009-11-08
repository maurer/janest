

(** Utility functions concerning the OCaml-runtime

    @author Markus Mottl <mmottl\@janestcapital.com>
*)

val running_byte_code : unit -> bool
(** [running_byte_code ()] @return [true] when the program is being run
    in byte code, [false] when it is being executed as native code. *)
