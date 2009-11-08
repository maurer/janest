open Core.Std

(** This module implements an imperative map based on a mutable AVL
    tree. It is much faster than the OCaml standard map. In terms of
    constants, it is about 3 times slower than Hashtbl, however it has
    better guarenteed time complexity. Most people wanting a hashtbl
    replacement will want to use [Hashtree], which has the same
    guarentee and much better constants (they are comparable to
    [Hashtbl]). This module is exposed to make it easy to build
    derived data structures more than to be directly used.

    see [Hashtree] for function descriptions and individual time
    complexity.
*)


type ('k, 'v) t

include Sexpable.S2 with type ('k, 'v) sexpable = ('k, 'v) t
include Binable.S2 with type ('k, 'v) binable = ('k, 'v) t

val create : unit -> ('k, 'v) t
val invariant : ('k, 'v) t -> unit

val add : ('k, 'v) t -> key:'k -> data:'v -> unit
val remove : ('k, 'v) t -> 'k -> unit
val find : ('k, 'v) t -> 'k -> 'v option
val fold : ('k, 'v) t -> init:'a -> f:(key:'k -> data:'v -> 'a -> 'a) -> 'a
val iter : ('k, 'v) t -> f:(key:'k -> data:'v -> unit) -> unit

module type Key = sig
  type t
  include Sexpable.S with type sexpable = t

  val compare : t -> t -> int
end

module type S = sig
  type key
  type 'a t

  include Sexpable.S1 with type 'a sexpable = 'a t

  val create : unit -> 'a t
  val invariant : 'a t -> unit

  val add : 'a t -> key:key -> data:'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a option
  val fold : 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b
  val iter : 'a t -> f:(key:key -> data:'a -> unit) -> unit
end

module Make (Key : Key) : S with type key = Key.t
