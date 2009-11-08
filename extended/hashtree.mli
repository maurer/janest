open Core.Std




(** This module implements a normal hashtbl, with one tweak. Instead
    of using linked lists for buckets it uses (mutable) AVL
    trees. This property means that the worst case time complexity of
    find, add, and remove is O(log(N)), instead of O(N) as with the
    list version. As with the standard Hashtbl, the average case time
    complexity is O(1) with a good hash function, and the constants
    appear to be comparable.

    You must pay for this guarentee by specifying a comparison
    function instead of an equality function (the polymorphic version
    uses Pervasives.compare). This means that this version of hashtbl
    does not work with certian classes of keys (E.G. keys where
    physical equality is the only method of comparison). *)
  
type ('k, 'v) t

include Sexpable.S2 with type ('k, 'v) sexpable = ('k, 'v) t
include Binable.S2 with type ('k, 'v) binable = ('k, 'v) t

(** [create i] create a new table with initial size [i].

    Note that this implementation DOES implement table growth when the
    buckets get too crowded, however, the cutoff is much higher than
    in hashtbl. When there are 10 times more elements than buckets it
    will grow by a factor of 10. Using this larger factor seems to
    improve performance. Probably because searching through buckets is
    much faster than in a standard table (3 comparisons instead of
    10).
*)
val create : int -> ('k, 'v) t

(** [add t ~key ~data] add a new binding of [key] to [data] to the table.

    Note that the semantics of [add] are the same as those of
    [Hashtbl.replace].  This implementation does NOT support multiple
    bindings.

    worst case: O(log(N)), average case: amortized O(1)
*)
val add : ('k, 'v) t -> key:'k -> data:'v -> unit

(** [remove t key] remove the binding of [key] from the table, if no
    binding exists do nothing.

    worst case: O(log(N)), average case: amortized O(1)
*)
val remove : ('k, 'v) t -> 'k -> unit

(** [find t key] look up the binding of [key], if no binding exists
    return [None]

    worst case: O(log(N)), average case: amortized O(1)
*)
val find : ('k, 'v) t -> 'k -> 'v option

(** [length t] return the number of bindings in the table.

    O(1)
*)
val length : ('k, 'v) t -> int

(** [fold t ~init:z ~f] fold through the table. NO guarentee is made
    about the order in which you will see elements.

    O(N)
*)
val fold : ('k, 'v) t -> init:'b -> f:(key:'k -> data:'v -> 'b -> 'b) -> 'b

module type Key = sig
  type t
  include Sexpable with type sexpable = t

  val hash : t -> int
  val compare : t -> t -> int
end

(** The functorized version *)  
module type S = sig
  module Key : Key

  type 'a t

  include Sexpable.S1 with type 'a sexpable = 'a t

  val create : int -> 'a t
  val add : 'a t -> key:Key.t -> data:'a -> unit
  val remove : 'a t -> Key.t -> unit
  val find : 'a t -> Key.t -> 'a option
  val length : 'a t -> int
  val fold : 'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
end

module Make (Key : Key) : S with module Key = Key
