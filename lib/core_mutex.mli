(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

INCLUDE "config.mlh"
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

IFDEF MUTEX_TIMED_LOCK THEN
(** [timedlock mtx timeout] like [lock], but takes a [timeout] parameter.
    @return [true] if the mutex was acquired, or [false] when [timeout]
    expires otherwise.

    @raise Unix_error if [timedlock] attempts to acquire [mtx] recursively.
*)
val timedlock : t -> Time.t -> bool
ENDIF

(** [unlock mtx] unlocks [mtx].

    @raise Unix_error if [unlock] attempts to release an unacquired
    mutex or a mutex held by another thread.
*)
val unlock : t -> unit

val am_holding_mutex : t -> bool

val critical_section : t -> f:(unit -> 'a) -> 'a

(* [sychronize f] returns a new function that is identical except that at most one thread
   can execute in it at a time. *)
val synchronize : ('a -> 'b) -> ('a -> 'b)

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
