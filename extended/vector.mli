(* A Vector is a functional array implemented as a tree of arrays.  Each array in the tree
 * can be up to length 32, and the resulting tree is traversed by examining 5 bit chunks
 * of a given index to get sub-indices into a given node of the tree.  This means that
 * lookups are log 32 n, and should be fairly cache friendly.  Setting does involve some
 * array blitting, but because the maximum array size in this case is 32 it remains fairly
 * quick.  The current tail of the array is kept separately until it reaches length 32
 * before placing it in the tree to make the common append 1 element to the end of the
 * array action faster. *)

open Core.Std

type 'a t

include Binable.S1 with type 'a binable = 'a t
include Container.S1 with type 'a container = 'a t
include Sexpable.S1 with type 'a sexpable = 'a t

val empty : 'a t
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> 'a t

(* [snoc t v] returns a new t with v appended to the end.  This is not called append
 * because the signature of Array.append is: 'a t -> 'a t -> 'a t *)
val snoc : 'a t -> 'a -> 'a t
