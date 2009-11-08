(** Mutable finite sets of non-negative integers.

    Testing membership and adding an element are O(log(N)), where N is the
    largest element in the set.

    The representation uses a compressed form of a complete d-ary tree, and so
    is space efficient when the integers in the set form contiguous ranges.
    More precisely, the space used is O(log(N) * R), where R is the number of
    contiguous ranges in the set.
*)

open Core.Std;;

type t

include Sexpable with type sexpable = t

(** Container functions. *)
val length : t -> int
val is_empty : t -> bool

val invariant : t -> unit

val to_string : t -> string

(** [create ?log2_degree ()] returns an empty set.  [log2_degree] is the degree
    of each node in the tree that represents the set.  Must have
    [log2_degree >= 1].  The default is [4].
*)
val create : ?log2_degree:int -> unit -> t

(** [mem t i] checks if [i] is in [t]. *)
val mem : t -> int -> bool

(** [add t i] adds [i] to the [t], or does nothing if [i] is in [t] *)
val add : t -> int -> unit

(** [add_range t ~lo ~hi] adds all the integers in the range [lo, hi].
    [add_range] runs in time proportional to O(log(N)) -- it does not depend
    on the size of the range.

    Must have [0 <= lo <= hi].
*)
val add_range : t -> lo:int -> hi:int -> unit
  
(** [{min,max}_element t] return the smallest and largest elements in [t]. *)
val min_element : t -> int option
val max_element : t -> int option
