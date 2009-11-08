(** Tail recursive version of standard List functions, plus
    additional operations.
 *)

type 'a t = 'a list

include Binable.S1 with type 'a binable = 'a t
include Container.S1 with type 'a container = 'a t
include Sexpable.S1 with type 'a sexpable = 'a t

include Monad.S with type 'a monad = 'a t


val nth : 'a t -> int -> 'a option
(** Return the [n]-th element of the given list.
   The first element (head of the list) is at position 0.
   Raise [Failure "nth"] if the list is too short.
   Raise [Invalid_argument "List.nth"] if [n] is negative. *)
val nth_exn : 'a t -> int -> 'a
(** List reversal. *)
val rev : 'a t -> 'a t
(** [List.rev_append l1 l2] reverses [l1] and concatenates it to [l2].
   This is equivalent to {!List.rev}[ l1 @ l2], but [rev_append] is
   tail-recursive and more efficient. *)
val rev_append : 'a t -> 'a t -> 'a t

(** [List.rev_map f l] gives the same result as
   {!List.rev}[ (]{!ListLabels.map}[ f l)], but is more efficient. *)
val rev_map : f:('a -> 'b) -> 'a t -> 'b t

(** [List.fold_left f a [b1; ...; bn]] is
   [f (... (f (f a b1) b2) ...) bn]. *)
val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a

(** [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
   [f a1 b1; ...; f an bn].
   Raise [Invalid_argument] if the two lists have
   different lengths. *)
val iter2 : f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [List.rev_map2 f l1 l2] gives the same result as
   {!List.rev}[ (]{!List.map2}[ f l1 l2)], but is more efficient. *)
val rev_map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
   [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
   Raise [Invalid_argument] if the two lists have
   different lengths. *)
val fold_left2 :
  f:('a -> 'b -> 'c -> 'a) -> init:'a -> 'b t -> 'c t -> 'a
(** Same as {!List.for_all}, but for a two-argument predicate.
   Raise [Invalid_argument] if the two lists have
   different lengths. *)
val for_all2 : f:('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Same as {!List.exists}, but for a two-argument predicate.  Raise [Invalid_argument] if
    the end of one list is reached before the end of the other. *)
val exists2 : f:('a -> 'b -> bool) -> 'a t -> 'b t -> bool

(** [mem a l] is true if and only if [a] is equal
   to an element of [l]. *)
val mem : 'a -> set:'a t -> bool
(** Same as {!List.mem}, but uses physical equality instead of structural
   equality to compare list elements. *)
val memq : 'a -> set:'a t -> bool

(** [filter p l] returns all the elements of the list [l]
   that satisfy the predicate [p].  The order of the elements
   in the input list is preserved.  *)
val filter : f:('a -> bool) -> 'a t -> 'a t
val filteri : 'a t -> f: (int -> 'a -> bool) -> 'a list
(** [find_all] is another name for {!List.filter}. *)
val find_all : f:('a -> bool) -> 'a t -> 'a t

(** [partition p l] returns a pair of lists [(l1, l2)], where
    [l1] is the list of all the elements of [l] that
    satisfy the predicate [p], and [l2] is the list of all the
    elements of [l] that do not satisfy [p].
    The order of the elements in the input list is preserved. *)
val partition : 'a t -> f:('a -> bool) -> 'a t * 'a t



val assoc' : ('a * 'b) t -> 'a -> equal:('a -> 'a -> bool) -> 'b option
val assoc_exn' : ('a * 'b) t -> 'a -> equal:('a -> 'a -> bool) -> 'b
val mem_assoc' : ('a * 'b) t -> 'a -> equal:('a -> 'a -> bool) -> bool
val remove_assoc' : ('a * 'b) t -> 'a -> equal:('a -> 'a -> bool) -> ('a * 'b) t

val assoc : 'a -> ('a * 'b) t -> 'b
val mem_assoc : 'a -> map:('a * 'b) t -> bool

(* Removes all occurrences. Not tail recursive! *)
val remove_assoc : 'a -> ('a * 'b) t -> ('a * 'b) t


val sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
val stable_sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
val fast_sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t

(** Merge two lists:
    Assuming that [l1] and [l2] are sorted according to the
    comparison function [cmp], [merge cmp l1 l2] will return a
    sorted list containting all the elements of [l1] and [l2].
    If several elements compare equal, the elements of [l1] will be
    before the elements of [l2].
*)
val merge : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> 'a t

val hd : 'a t -> 'a option
val tl : 'a t -> 'a t option
(** Return the first element of the given list. Raise
   [Failure "hd"] if the list is empty. *)
val hd_exn : 'a t -> 'a
(** Return the given list without its first element. Raise
   [Failure "tl"] if the list is empty. *)
val tl_exn : 'a t -> 'a t

val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option

(** [find_map t f] applies [f] to each element of [t] until it finds
 * [f a = Some b], at which point it returns [Some b].  If there is no such
 * element, [find_map] returns [None].
 *)
val find_map : 'a t -> f:('a -> 'b option) -> 'b option

(** [find_exn t ~f] returns the first element of [t] that satisfies [f].  It
    raises [Not_found] if there is no such element.
*)
val find_exn : 'a t -> f:('a -> bool) -> 'a

(** {6 Tail-recursive implementations of standard List operations} *)

(** E.g. [append [1; 2] [3; 4; 5]] is [[1; 2; 3; 4; 5]] *)
val append : 'a t -> 'a t -> 'a t

(** [List.map f [a1; ...; an]] applies function [f] to [a1, ..., an],
   and builds the list [[f a1; ...; f an]]
   with the results returned by [f]. *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(** [concat_map t ~f] is [concat (map t ~f)], except that there
 * is no guarantee about the order in which [f] is applied to the elements of
 * [t].
 *)
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t

(** [List.map2 f [a1; ...; an] [b1; ...; bn]] is
   [[f a1 b1; ...; f an bn]].
   Raise [Invalid_argument] if the two lists have
   different lengths. *)
val map2 :'a t -> 'b t ->  f:('a -> 'b -> 'c) -> 'c t

val rev_map3 :
  'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
val map3 :
  'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t

(** [rev_map_append ~f l1 l2] reverses [l1] mapping [f] over each
    element, and appends the result to the front of [l2]. *)
val rev_map_append : 'a t -> 'b t -> f:('a -> 'b) -> 'b t


(** [List.fold_right f [a1; ...; an] b] is
    [f a1 (f a2 (... (f an b) ...))]. *)
val fold_right : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b

(** [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
   [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
   Raise [Invalid_argument] if the two lists have
   different lengths. *)
val fold_right2 : 'a t -> 'b t -> f:('a -> 'b -> 'c -> 'c) -> init:'c -> 'c



(** Transform a list of pairs into a pair of lists:
   [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
*)
val split : ('a * 'b) t -> 'a t * 'b t

(** Transform a pair of lists into a list of pairs:
   [combine [a1; ...; an] [b1; ...; bn]] is
   [[(a1,b1); ...; (an,bn)]].
   Raise [Invalid_argument] if the two lists
   have different lengths. *)
val combine : 'a t -> 'b t -> ('a * 'b) t

(** mapi is just like map, but it also passes in the index of each
    element as the first argument to the mapped function. Tail-recursive. *)
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t

(** iteri is just like iter, but it also passes in the index of each
    element as the first argument to the iter'd function. Tail-recursive. *)
val iteri : 'a t ->  f:(int -> 'a -> unit) -> unit


(** fold_lefti is just like fold_left, but it also passes in the index of each
    element as the first argument to the folded function.  Tail-recursive. *)
val fold_lefti : 'a t -> f:(int -> 'b -> 'a -> 'b) -> init:'b -> 'b

(** [reduce f [a1; ...; an]] is [f (... (f (f a1 a2) a3) ...) an].
    It fails on the empty list.  Tail recursive. *)
val reduce_exn : 'a t -> f:('a -> 'a -> 'a) -> 'a
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option

(** [group l ~break] returns a list of lists (i.e., groups) whose concatenation is
    equal to the original list.  Each group is broken where break returns true on
    a pair of successive elements.

    Example

    group ~break:(<>) ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i'] ->

    [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']]
*)
val group : 'a t -> break:('a -> 'a -> bool) -> 'a t t

(** This is just like group, except that you get the index in the original list of the
    current element along with the two elements.

    Example, group the chars of Mississippi into triples

    groupi ~break:(fun i _ _ -> i mod 3 = 0)
    ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i'] ->

    [['M'; 'i'; 's']; ['s'; 'i'; 's']; ['s'; 'i'; 'p']; ['p'; 'i']]
*)
val groupi : 'a t -> break:(int -> 'a -> 'a -> bool) -> 'a t t

(** min_combine combines two lists, possibly of different length, and returns
    a list the length of the shorter list.  Tail recursive.  *)
val min_combine : 'a t -> 'b t -> ('a * 'b) t

(** The final element of a list.  The _exn version raises Invalid_argument on the empty
    list. *)
val last : 'a t -> 'a option
val last_exn : 'a t -> 'a




(** [dedup] (de-duplicate).  The same list with duplicates removed, but the
    order is not guaranteed. *)
val dedup : ?compare:('a -> 'a -> int) -> 'a t -> 'a t

(** [stable_dedup] Same as [dedup] but maintains the order of the list and doesn't allow
    compare function to be specified (uses set membership). *)
val stable_dedup : 'a t -> 'a t

(** [contains_dup] True if there are any two elements in the list which are the same. *)
val contains_dup : 'a t -> bool

(** [find_a_dup] returns a duplicate from the list (no guarantees about which
  duplicate you get), or None if there are no dups. *)
val find_a_dup : 'a t -> 'a option

(** [count f l] is the number of elements in [l] that satisfy the
    predicate [f].  *)
val count : 'a t -> f:('a -> bool) -> int

(** [range stride low high] is the list of integers from [low](inclusive)
    to [high](exclusive), stepping by [stride].  If unspecified, [stride]
    defaults to 1. *)
val range : ?stride:int -> int -> int -> int t

(** [frange] is similar to [range], but for floats. *)
val frange : ?stride:float -> float -> float -> float t

(** [init f n] is [[(f 0); (f 1); ...; (f (n-1))]].
    It is an error if [n < 0].
*)
val init : int -> f:(int -> 'a) -> 'a t

(** [rev_filter_map f l] is the reversed sublist of [l] containing
    only elements for which [f] returns [Some e]. *)
val rev_filter_map : 'a t -> f:('a -> 'b option) -> 'b t

(** [filter_map f l] is the sublist of [l] containing only elements
    for which [f] returns [Some e].  *)
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

(** [filter_opt l] is the sublist of [l] containing only elements
    which are [Some e].  In other words, [filter_opt l] = [filter_map ~f:ident l]. *)
val filter_opt : 'a option t -> 'a t

(** [partition_map t ~f] partitions [t] according to [f]. *)
val partition_map :
  'a t -> f:('a -> [ `Fst of 'b | `Snd of 'c ]) -> 'b t * 'c t


(** [reverse_pairs l] reverses each tuple inside an association list. *)
val reverse_pairs : ('a * 'b) t -> ('b * 'a) t

(** [split_n n \[e1; ...; em\]] is [(\[e1; ...; en\], \[en+1; ...; em\])].
    If [n > m], [(\[e1; ...; em\], \[\])] is returned.  If [n < 0],
    Invalid_argument is raised. *)
val split_n : 'a t -> int -> ('a t * 'a t)

(** [sub pos len l] is the [len]-element sublist of [l], starting at [pos]. *)
val sub : 'a t -> pos:int -> len:int -> 'a t


(** [slice l start stop] returns a new list including elements [l.(start)] through
    [l.(stop-1)], normalized python-style. *)
val slice : 'a t -> int -> int -> 'a t


(** [take l n] is [fst (split_n n l)].
    [drop l n] is [snd (split_n n l)]. *)
val take : 'a t -> int -> 'a t
val drop : 'a t -> int -> 'a t

(** [drop_while l ~f] keeps dropping elements while [f el] is [true]
    and returns the rest in order. *)
val drop_while : 'a t -> f : ('a -> bool) -> 'a t

(** Concatenate a list of lists.  The elements of the argument are all
   concatenated together (in the same order) to give the result.
   Tail recursive over outer and inner lists. *)
val concat : 'a t t -> 'a t


(** Same as [concat]. *)
val flatten : 'a t t -> 'a t

(** Same as [flatten] but faster and without preserving any ordering (ie
    for lists that are essentially viewed as multi-sets. *)
val flatten_no_order : 'a t t -> 'a t

(* Deprecated.  Use [assoc']. *)
(** [assoc_opt a l] returns the value associated with key [a] in the list of pairs [l]
 * as an option. *)
val assoc_opt : 'a -> ('a * 'b) t -> 'b option

val cons : 'a -> 'a t -> 'a t

(* Returns a list with all possible pairs -- if input lists have length len1 and len2,
   resulting list will have length len1*len2. *)
val cartesian_product : 'a t -> 'b t -> ('a * 'b) t


val to_string : ('a -> string) -> 'a t -> string

(** [permute l] shuffles [l], using Random.int *)
val permute : ?random_state:Random.State.t -> 'a t -> 'a t

val is_sorted : 'a t -> compare:('a -> 'a -> int) -> bool

(** lexicographic *)
val compare : 'a t -> 'a t -> cmp:('a -> 'a -> int) -> int


module Infix : sig
  val ( @ ) : 'a t -> 'a t -> 'a t
end
