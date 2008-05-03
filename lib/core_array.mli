(** A better Array module.  If you open Core.Std, you get this as your Array module
    in place of the standard Array module. *)

type 'a t = 'a array

include Binable.S1 with type 'a binable = 'a t
include Container.S1 with type 'a container = 'a t
include Sexpable.S1 with type 'a sexpable = 'a t

(* CRv2 sweeks: replace ['a array] with ['a t] *)

(* From the standard ArrayLabels. *)
external get : 'a array -> int -> 'a = "%array_safe_get"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external create : int -> 'a -> 'a array = "caml_make_vect"
val init : int -> f:(int -> 'a) -> 'a array
val make_matrix : dimx:int -> dimy:int -> 'a -> 'a array t
val append : 'a array -> 'a array -> 'a array
val concat : 'a array list -> 'a array
val sub : 'a array -> pos:int -> len:int -> 'a array
val copy : 'a array -> 'a array
val fill : 'a array -> pos:int -> len:int -> 'a -> unit
val blit :
  src:'a array -> src_pos:int -> dst:'a array -> dst_pos:int -> len:int -> unit
val of_list : 'a list -> 'a array
val map : f:('a -> 'b) -> 'a array -> 'b t
val iteri : f:(int -> 'a -> unit) -> 'a array -> unit
val mapi : f:(int -> 'a -> 'b) -> 'a array -> 'b t
val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a
val fold_right : f:('b -> 'a -> 'a) -> 'b t -> init:'a -> 'a
val sort : cmp:('a -> 'a -> int) -> 'a array -> unit
val stable_sort : cmp:('a -> 'a -> int) -> 'a array -> unit
val fast_sort : cmp:('a -> 'a -> int) -> 'a array -> unit

(**
   ----------------------------------------------------------------------
   Extensions
   ----------------------------------------------------------------------
*)

val cartesian_product : 'a array -> 'b array -> ('a * 'b) array

(** [normalize array index] returns a new index into the array such that if index is less
    than zero, the returned index will "wrap around" -- i.e. array.(normalize array (-1))
   returns the last element of the array. *)
val normalize : 'a array -> int -> int

(** [slice array start stop] returns a fresh array including elements [array.(start)] through
    [array.(stop-1)] with the small tweak that the start and stop positions are normalized
    and a stop index of 0 means the same thing a stop index of [Array.length array].  In
    summary, it's like the slicing in Python or Matlab. *)
val slice : 'a array -> int -> int -> 'a array

(** Array access with [normalize]d index. *)
val nget : 'a array -> int -> 'a

(** Array modification with [normalize]d index. *)
val nset : 'a array -> int -> 'a -> unit

(** [filter_opt array] returns a new array where [None] entries are omitted and [Some x]
    entries are replaced with [x]. Note that this changes the index at which elements
    will appear. *)
val filter_opt : 'a option array -> 'a array

(** [filter_map ~f array] maps [f] over [array] and filters [None] out of the results. *)
val filter_map : f:('a -> 'b option) -> 'a array -> 'b array

(** Same as [filter_map] but uses {!Array.mapi}. *)
val filter_mapi : f:(int -> 'a -> 'b option) -> 'a array -> 'b array

val map2 : f:('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c t

(** [filter ~f array] removes the elements for which [f] returns false.  *)
val filter : f:('a -> bool) -> 'a array -> 'a array

(** Like [filter] except [f] also receives the index. *)
val filteri : f:(int -> 'a -> bool) -> 'a array -> 'a array

(** [swap arr i j] swaps the value at index [i] with that at index [j]. *)
val swap : 'a array -> int -> int -> unit

(** [mem el arr] returns true iff [arr.(i) = el] for some i *)
val mem : 'a -> 'a array -> bool

(* CR sweeks:  rev should be called rev_inplace or something, and add
     val rev : 'a t -> 'a t
   like List.rev.
*)
(** [rev ar] reverses [ar] in place *)
val rev : 'a array -> unit

(** [replace t i ~f] = [t.(i) <- f (t.(i))]. *)
val replace : 'a t -> int -> f:('a -> 'a) -> unit

(** modifies an array in place -- [ar.(i)] will be set to [f(ar.(i))] *)
val replace_all : 'a t -> f:('a -> 'a) -> unit

(** [find_exn f t] returns in first [a] in [t] for which [f t.(i)] is true.
    It raises [Not_found] if there is no such [a].
*)
val find_exn : 'a array -> f:('a -> bool) -> 'a

(** [findi f ar] returns in first index [i] of [ar] for which [f ar.(i)] is true *)
val findi : 'a array -> f:('a -> bool) -> int option

(** [findi_exn f ar] returns in first index [i] of [ar] for which [f ar.(i)] is
    true.  It raises [Not_found] if there is no such element. *)
val findi_exn : 'a array -> f:('a -> bool) -> int

(** [reduce f [a1; ...; an]] is [f (... (f (f a1 a2) a3) ...) an]. *)
val reduce : f:('a -> 'a -> 'a) -> 'a array -> 'a

(** [best] is an alias for [reduce]. *)
val best : f:('a -> 'a -> 'a) -> 'a array -> 'a

(** [permute ar] randomly permutes [ar] in place *)
val permute : ?random_state:Random.State.t -> 'a array -> unit

(** [combine ar] combines two arrays to an array of pairs. *)
val combine : 'a array -> 'b array -> ('a * 'b) array

(** [sorted_copy ar cmp] returns a shallow copy of [ar] that is sorted. Similar to
    List.sort *)
val sorted_copy : 'a array -> cmp:('a -> 'a -> int) -> 'a array

val last : 'a array -> 'a

module Infix : sig
  val ( <|> ) : 'a array -> int * int -> 'a array
end
