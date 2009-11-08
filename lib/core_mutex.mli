(*pp $(pwd)/pp.sh *)
(*
#include <unistd.h>
end-pp-include*)
(* Error-checking mutexes. *)

type t = Mutex.t

val create : unit -> t
val equal : t -> t -> bool

(** [lock mtx] locks [mtx], possibly waiting for it to be released
    first by another thread.

    @raise Unix_error if [lock] attempts to acquire [mtx] recursively.
*)
val lock : t -> unit

(** [try_lock mtx] like [lock], but returns immediately with [false]
    if the mutex is already being held by another thread, or acquires
    the mutex and returns [true] otherwise.

    @raise Unix_error if [try_lock] attempts to acquire [mtx] recursively.
*)
val try_lock : t -> bool


#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)
(** [timedlock mtx timeout] like [lock], but takes a [timeout] parameter.
    @return [true] if the mutex was acquired, or [false] when [timeout]
    expires otherwise.

    @raise Unix_error if [timedlock] attempts to acquire [mtx] recursively.
*)
val timedlock : t -> Time.t -> bool
#else
#warning "POSIX TMO not present; Core_mutex.timedlock unavailable"
#endif

(** [unlock mtx] unlocks [mtx].

    @raise Unix_error if [unlock] attempts to release an unacquired
    mutex or a mutex held by another thread.
*)
val unlock : t -> unit

val am_holding_mutex : t -> bool

val critical_section : t -> f:(unit -> 'a) -> 'a

(** [update_signal mtx cnd ~f] updates some state within a critical
    section protected by mutex [mtx] using function [f] and signals
    condition variable [cnd] after finishing.  If [f] raises an exception,
    the condition will NOT be signaled! *)
val update_signal : t -> Condition.t -> f:(unit -> 'a) -> 'a

(** [update_broadcast mtx cnd ~f] updates some state within a critical
    section protected by mutex [mtx] using function [f] and broadcasts
    condition variable [cnd] after finishing.  If [f] raises an exception,
    the condition will NOT be broadcast! *)
val update_broadcast : t -> Condition.t -> f:(unit -> 'a) -> 'a
