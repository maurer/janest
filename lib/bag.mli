(** A bag is a data structure like a set, except that:

    * It doesn't require anything (hashable, comparable) of elements in the bag.
    * Duplicates are allowed.
    * Addition and removal are (amortized) constant time.
*)
open Std_internal

module Elt : sig
  type 'a t

  val equal : 'a t -> 'a t -> bool
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  val value : 'a t -> 'a
end

type 'a t

include Container.S1 with type 'a container = 'a t

val invariant : 'a t -> unit

(** [create ()] returns an empty bag. *)
val create : unit -> 'a t

(* CRv2 tvaroquaux: If we do not check for any invariants then [add] and
   [remove] should run in constant time. I think the "amortized" part
   of the comment puts us unnecessarily on our guards.
*)
(** [add t v] adds [v] to the bag [t], returning an element that can later be
    removed from the bag.  [add] runs in (amortized) constant time.
*)
val add : 'a t -> 'a -> 'a Elt.t

(** [remove t elt] removes [elt] from the bag [t], raising an exception if [elt]
    is not in the bag.  [remove] runs in (amortized) constant time.
*)
val remove : 'a t -> 'a Elt.t -> unit

(** [some_elt t] returns some element in the bag. *)
val some_elt : 'a t -> 'a Elt.t option

(** [remove_one t] removes some element from the bag, and returns its value.
    [remove_one] runs in (amortized) constant time.
*)
val remove_one : 'a t -> 'a option

(** [clear t] removes all elements from the bag.  [clear] runs in O([length t])
    time. *)
val clear : 'a t -> unit

(** [find_elt t ~f] looks at elements in the bag one-by-one until it finds one
    [elt] such that [f (Elt.value elt)], in which case it returns [Some elt].
    If there is no element satisfying [f], then [find_elt] returns [None]. *)
val find_elt : 'a t -> f:('a -> bool) -> 'a Elt.t option

(** [until_empty t f] repeatedly removes a value [v] from [t] and runs [f v],
    continuing until [t] is empty.  Running [f] may add elements to [t] if it
    wants.
*)
val until_empty : 'a t -> ('a -> unit) -> unit

val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
