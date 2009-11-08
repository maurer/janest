 (** A better Array module.  If you open Core.Std, you get this as your Array module
    in place of the standard Array module. *)

type 'a t = 'a array

include Binable.S1 with type 'a binable = 'a t
include Container.S1 with type 'a container = 'a t
include Sexpable.S1 with type 'a sexpable = 'a t

(* From the standard ArrayLabels. *)
external get : 'a t -> int -> 'a = "%array_safe_get"
external set : 'a t -> int -> 'a -> unit = "%array_safe_set"
external unsafe_get : 'a t -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a t -> int -> 'a -> unit = "%array_unsafe_set"

val create : int -> 'a -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val make_matrix : dimx:int -> dimy:int -> 'a -> 'a t t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val sub : 'a t -> pos:int -> len:int -> 'a t
val copy : 'a t -> 'a t
val fill : 'a t -> pos:int -> len:int -> 'a -> unit
val blit : src:'a t -> src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> unit
val of_list : 'a list -> 'a t
val map : f:('a -> 'b) -> 'a t -> 'b t
val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a
val fold_right : f:('b -> 'a -> 'a) -> 'b t -> init:'a -> 'a
val sort : cmp:('a -> 'a -> int) -> 'a t -> unit
val stable_sort : cmp:('a -> 'a -> int) -> 'a t -> unit
val fast_sort : cmp:('a -> 'a -> int) -> 'a t -> unit

(**
   ----------------------------------------------------------------------
   Extensions
   ----------------------------------------------------------------------
*)





(** Array lengths [l] satisfy [0 <= l < max_length]. *)
val max_length : int

val cartesian_product : 'a t -> 'b t -> ('a * 'b) t

(** [normalize array index] returns a new index into the array such that if index is less
    than zero, the returned index will "wrap around" -- i.e. array.(normalize array (-1))
   returns the last element of the array. *)
val normalize : 'a t -> int -> int

(** [slice array start stop] returns a fresh array including elements [array.(start)] through
    [array.(stop-1)] with the small tweak that the start and stop positions are normalized
    and a stop index of 0 means the same thing a stop index of [Array.length array].  In
    summary, it's like the slicing in Python or Matlab. *)
val slice : 'a t -> int -> int -> 'a t

(** Array access with [normalize]d index. *)
val nget : 'a t -> int -> 'a

(** Array modification with [normalize]d index. *)
val nset : 'a t -> int -> 'a -> unit

(** [filter_opt array] returns a new array where [None] entries are omitted and [Some x]
    entries are replaced with [x]. Note that this changes the index at which elements
    will appear. *)
val filter_opt : 'a option t -> 'a t

(** [filter_map ~f array] maps [f] over [array] and filters [None] out of the results. *)
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

(** Same as [filter_map] but uses {!Array.mapi}. *)
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t

val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

(** [filter ~f array] removes the elements for which [f] returns false.  *)
val filter : f:('a -> bool) -> 'a t -> 'a t

(** Like [filter] except [f] also receives the index. *)
val filteri : f:(int -> 'a -> bool) -> 'a t -> 'a t

(** [swap arr i j] swaps the value at index [i] with that at index [j]. *)
val swap : 'a t -> int -> int -> unit


(** [mem el arr] returns true iff [arr.(i) = el] for some i *)
val mem : 'a -> 'a t -> bool


(** [rev ar] reverses [ar] in place *)
val rev : 'a t -> unit

(** [of_list_rev l] converts from list then reverses in place *)
val of_list_rev : 'a list -> 'a t

(** [replace t i ~f] = [t.(i) <- f (t.(i))]. *)
val replace : 'a t -> int -> f:('a -> 'a) -> unit


(** modifies an array in place -- [ar.(i)] will be set to [f(ar.(i))] *)
val replace_all : 'a t -> f:('a -> 'a) -> unit

(** [find_exn f t] returns the first [a] in [t] for which [f t.(i)] is true.
    It raises [Not_found] if there is no such [a].
*)
val find_exn : 'a t -> f:('a -> bool) -> 'a

(** [findi f ar] returns the first index [i] of [ar] for which [f ar.(i)] is true *)

val findi : 'a t -> f:('a -> bool) -> int option

(** [findi_exn f ar] returns the first index [i] of [ar] for which [f ar.(i)] is
    true.  It raises [Not_found] if there is no such element. *)
val findi_exn : 'a t -> f:('a -> bool) -> int

(** [reduce f [a1; ...; an]] is [f (... (f (f a1 a2) a3) ...) an]. *)
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option
val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a

(** [permute ar] randomly permutes [ar] in place *)
val permute : ?random_state:Random.State.t -> 'a t -> unit

(** [combine ar] combines two arrays to an array of pairs. *)
val combine : 'a t -> 'b t -> ('a * 'b) t

(** [split ar] splits an array of pairs into two arrays of single elements. *)
val split : ('a * 'b) t -> 'a t * 'b t


(** [sorted_copy ar cmp] returns a shallow copy of [ar] that is sorted. Similar to
    List.sort *)
val sorted_copy : 'a t -> cmp:('a -> 'a -> int) -> 'a t

val last : 'a t -> 'a

(** [empty ()] creates an empty array *)
val empty : unit -> 'a t

module Infix : sig
  val ( <|> ) : 'a t -> int * int -> 'a t
end
