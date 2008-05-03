(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
open Std_internal

(** Module for simple closed intervals over arbitrary types that are ordered
    correctly using polymorphic compare. *)
(* CRv2 sweeks: It would be nice to make this a functor that takes a Comparable.
*)

type 'a t with bin_io, sexp
val make : 'a -> 'a -> 'a t
val empty : 'a t

val intersect : 'a t -> 'a t -> 'a t

val is_empty : 'a t -> bool

val is_empty_or_singleton : 'a t -> bool

(* CRv2 sweeks: Jane convention would argue for renaming [lbound] as
   [lbound_exn] and [lbound_opt] as [lbound] *)

(* CRv2 sweeks: Add [val bounds: 'a t -> ('a * 'a) option] *)

val lbound_opt : 'a t -> 'a option

val ubound_opt : 'a t -> 'a option

val lbound : 'a t -> 'a

val ubound : 'a t -> 'a

val contains : 'a t -> 'a -> bool

(* CRv2 RG: add a test for checking whether something is contained in a list of intervals *)
(* CRv2 sweeks: It might be worth having an abstract type, "Intervals" that
   maintains the invariant "ascending list of nonempty disjoint intervals".  This
   property is preserved by [list_intersect]. *)

(* Specialized fast version for Time.t *)
val time_contains : Time.t t -> Time.t -> bool

(** [contains_interval i1 i2] is whether i1 contains i2.  The empty
    interval is contained in every interval. *)
val contains_interval : 'a t -> 'a t -> bool

val map : f : ('a -> 'b) -> 'a t -> 'b t

(** Returns true iff a given set of intervals are disjoint *)
val are_disjoint : 'a t list -> bool

(** Returns true iff a given set of intervals would be disjoint if considered as open
    intervals.  i.e.,  (3,4) and (4,5) would count as disjoint. *)
val are_disjoint_as_open_intervals : 'a t list -> bool

(** Assuming that [ilist1] and [ilist2] are lists of (disjoint) intervals,
  [list_intersect ilist1 ilist2] returns the list of disjoint intervals that
  correspond to the intersection of [ilist1] with [ilist2].
*)
val list_intersect : 'a t list -> 'a t list -> 'a t list
