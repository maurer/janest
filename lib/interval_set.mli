(* This module defines interval sets ("1 through 10 and 22 through 24") that can be
   unioned, intersected, etc.
   
   The intervals are closed.
   
   Polymorphic comparison is assumed to work.

   Efficiency is bad for large numbers of intervals.

   There is a more complete and somewhat more efficient version in Qbase.
*)
open Std_internal

type 'a t
val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t

(* Creates an interval set containing values between each pair of values.  The
   pairs have to be sorted in ascending order, and they can't overlap. *)
val create : ('a * 'a) list -> 'a t

val contains : 'a t -> 'a -> bool

val contains_set : container:('a t) -> contained:('a t) -> bool

(* The largest and smallest element of the interval set, respectively.  Raises Invalid_arg
   on empty sets. *)
val ubound : 'a t -> 'a
val lbound : 'a t -> 'a
