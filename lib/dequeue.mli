(** An array that can shrink and expand on both ends - the minimum index needs not be 0.
    can easily be used as a double-ended queue
    unlike cbuffer, an index refers to the same element after Dequeue.push_front
    the "front" is the smallest valid index, while the "back" is the largest
    all operations are amortized O(1) with a small constant *)

open Std_internal

type 'a t

val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t

(* if never_shrink is true, the physical array will never shrink; only expand *)
(* initial_index is the index at which the first push_back operation will insert *)
(* a dummy element is required to satisfy the type-checker and will never be returned *)
val create : ?never_shrink:bool -> ?initial_index:int -> dummy:'a -> unit -> 'a t

(* number of elements in the dequeue, i.e. back_index - front_index + 1 *)
val length : 'a t -> int
(* same as Dequeue.length = 0 *)
val is_empty : 'a t -> bool

(* length of physical array used to implement the dequeue -- will change automatically
   over time, doubling and halving *)
val phys_length : 'a t -> int

(* minimum and maximum valid indices (inclusive) *)
val front_index : 'a t -> int
val back_index : 'a t -> int

(* returns an element, and leaves it in the dequeue *)
(* [get q i] raises Invalid_argument unless front_index <= i <= back_index *)
val get : 'a t -> int -> 'a

(* raises Invalid_argument iff dequeue is empty *)
val get_front : 'a t -> 'a
val get_back : 'a t -> 'a

(* mutates the indexed element *)
val set : 'a t -> int -> 'a -> unit

(* same as Array.iteri (iterates passing the index) *)
val iteri : f:(int -> 'a -> unit) -> 'a t -> unit

(* same as iteri but don't pass the index *)
val iter : f:('a -> unit) -> 'a t -> unit

(* fold across the index element pairs of the dequeue *)
val foldi : f:('a -> int -> 'b -> 'a) -> init:'a -> 'b t -> 'a

(* fold across just the elements of the dequeue *)
val fold : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a

(* decreases front_index by one, and places the new element at the new front_index *)
val push_front : 'a t -> 'a -> unit

(* increases back_index by one, and places the new element at the new back_index *)
val push_back : 'a t -> 'a -> unit

(* drop functions raise Invalid_argument if asked to drop more than Dequeue.length
   elements *)
  
(* drops one element at front, increasing front_index *)
val drop_front : 'a t -> unit
(* drops one element at back, decreasing back_index *)
val drop_back : 'a t -> unit

(* drop the front and return it *)
val take_front : 'a t -> 'a

(* drop the back and return it *)
val take_back : 'a t -> 'a

(* drops n elements from front or back *)
val drop_n_front : 'a t -> int -> unit
val drop_n_back : 'a t -> int -> unit

(* drops index j iff j < i *)
val drop_indices_less_than : 'a t -> int -> unit
(* drops index j iff j > i *)
val drop_indices_greater_than : 'a t -> int -> unit
