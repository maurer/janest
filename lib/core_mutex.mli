(** A fast_mutex is just like a mutex, except that it uses try_lock to acquire
    locks, and is hence faster when it acquires the lock. *)

(* CRv2 sweeks: There's no reason why the standard Mutex should be used.  Replace
   all uses in Core with Core_mutex, and rebind Mutex in Common. *)

(* CRv2 sweeks: Would it be possible to add

     val hold_mutex : t -> bool

   to test if I hold the mutex?  Or at least

     val is_locked : t -> bool

   to check if it is locked?  That would make it possible to write useful
   assertions in mutex-ridden code.

*)

type t = Mutex.t

val create : unit -> t
val lock : t -> unit
val try_lock : t -> bool
val unlock : t -> unit
val critical_section : t -> f:(unit -> 'a) -> 'a
