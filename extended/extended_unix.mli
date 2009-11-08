open Core.Std
(** Extensions to [Core.Unix]. *)

val unsafe_create_process :
  prog : string
  -> args : string list
  -> stdin : Unix.file_descr
  -> stdout : Unix.file_descr
  -> stderr : Unix.file_descr
  -> int
(** [unsafe_create_process ~prog ~args ~stdin ~stdout ~stderr]
   forks a new process that executes the program
   in file [prog], with arguments [args]. The pid of the new
   process is returned immediately; the new process executes
   concurrently with the current process.
   The standard input and outputs of the new process are connected
   to the descriptors [new_stdin], [new_stdout] and [new_stderr].
   Passing e.g. [stdout] for [new_stdout] prevents the redirection
   and causes the new process to have the same standard output
   as the current process. The executable file [prog] is searched in
   the path. The new process has the same environment as the current process.

   Please refrain from using this function if you do not understand how to
   use [set_close_on_exec] or the kernel's reference counting of open file
   descriptors.
*)
