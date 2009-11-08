(* @author Markus Mottl <mmottl\@janestcapital.com> *)

(* Agnostic mutexes do not remember the thread by which they got locked.
   Therefore anybody can unlock them, even if they have not locked them.
   This also means that they will work in child processes: they are
   fork-safe.  The only error-check is that unlocked agnostic mutexes
   cannot be unlocked again, this will raise a [Failure]-exception.

   Though standard OCaml-mutexes (they do not check for errors)
   could in principle be used, too, on Linux systems, because their
   semantics happens to be almost equivalent, this is not recommended:

   The POSIX-standard explicitly specifies that the behavior of unlocking
   a mutex from within a different thread than the one that locked it
   is undefined.  This approach would therefore not be guaranteed to be
   future-proof, and would not be portable to other platforms either.

   Furthermore, standard mutexes can be unlocked without error even
   though they were already unlocked.  Agnostic mutexes, however, do
   check for this error.

   Hence the need for this module.
*)

type t

val create : unit -> t
val equal : t -> t -> bool
val lock : t -> unit
val try_lock : t -> bool
val unlock : t -> unit
val critical_section : t -> f:(unit -> 'a) -> 'a
