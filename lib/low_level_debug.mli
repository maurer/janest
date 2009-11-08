(* Useful debugging functions working at a low level.

   NOT FOR USE IN PRODUCTION CODE.

   Functions here that stop programs with SIGSTOP are designed to be used
   in conjunction with "gdb -p". *)

(* Stop the calling process or any forked children (with SIGSTOP) upon
   reception of SIGBUS.  This overrides any other Caml-installed handler. *)
val stop_upon_sigbus : unit -> unit

(* Stop the calling process or any forked children (with SIGSTOP) upon
   reception of SIGSEGV.  This overrides any other Caml-installed handler. *)
val stop_upon_sigsegv : unit -> unit

(* Stop the calling process or any forked children (with SIGSTOP) upon
   reception of SIGPIPE.  This overrides any other Caml-installed handler. *)
val stop_upon_sigpipe : unit -> unit

(* Stop the calling process or any forked children upon normal process
   termination. *)
val stop_upon_exit : unit -> unit

(* Stop the calling process right away.

   This is effectively the same as setting a gdb breakpoint, but is likely
   easier to use, especially if the program should only be stopped under
   certain circumstances.

   One example of the use of this function is to replace "raise" by a call
   to [stop_me_now]; this enables you to see the stack trace leading up to
   the raise.  (This might be useful if many functions can call a single
   raise point, and you don't know which caller is triggering it.)  You can
   of course view backtraces using OCAMLRUNPARAM=b, but that can be misleading:
   for example if an exception is re-raised then the backtrace will show it
   as raised only at the most recent raise, and previous frames (including the
   original raise) won't be named.  Using [stop_me_now] on the original raise
   also has the advantage that you don't need to adjust any "with" clauses
   between the raise point and the top level, which would have to be removed
   to see a backtrace with OCAMLRUNPARAM=b. *)
val stop_me_now : unit -> unit
