(** Tail recursive version of standard List functions, plus
    additional operations.
 *)

type 'a t = 'a list

include Binable.S1 with type 'a binable = 'a t
include Container.S1 with type 'a container = 'a t
include Sexpable.S1 with type 'a sexpable = 'a t

include Monad.S with type 'a monad = 'a list

(* CRv2 sweeks: replace ['a list] with ['a t] *)

(* From the standard ListLabels *)

val nth : 'a list -> int -> 'a option
val nth_exn : 'a list -> int -> 'a
val rev : 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val rev_map : f:('a -> 'b) -> 'a list -> 'b t
val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a
val iter2 : f:('a -> 'b -> unit) -> 'a list -> 'b t -> unit
val rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b t -> 'c t
val fold_left2 :
  f:('a -> 'b -> 'c -> 'a) -> init:'a -> 'b t -> 'c t -> 'a
val for_all2 : f:('a -> 'b -> bool) -> 'a list -> 'b t -> bool
val exists2 : f:('a -> 'b -> bool) -> 'a list -> 'b t -> bool
val mem : 'a -> set:'a list -> bool
val memq : 'a -> set:'a list -> bool
val filter : f:('a -> bool) -> 'a list -> 'a list
val find_all : f:('a -> bool) -> 'a list -> 'a list
(* CR sweeks: I can never remember whether true or false is on the left.
   What do people think about changing partition to
     val partition : 'a t -> f:('a -> [ `Fst | `Snd ]) -> 'a t * 'a t
   Similarly for partition_map
     val partition_map :
       'a t -> ~f:('a -> [ `Fst of 'b | `Snd of 'c ]) -> 'b list * 'c list
*)
val partition : f:('a -> bool) -> 'a list -> 'a list * 'a list

(* CR sweeks: I propose to change [assoc] and friends to remove the implicit use
   of = and ==.  Such implicit use makes it too easy to violate abstraction
   boundaries and miss when the notion of equality when a type changes.  How
   about the following?

      val assoc : ('a * 'b) t -> ('a -> bool) -> 'b option
      val assoc_exn : ('a * 'b) t -> ('a -> bool) -> 'b 
      val mem_assoc : ('a * 'b) t -> ('a -> bool) -> bool
      val remove_assoc : ('a * 'b) t -> ('a -> bool) -> ('a * 'b) t 

  I've also made the argument order consistent and dropped the inconsistent
  "map" label.

  Then one can write

    assoc alist ((=) x)

  or

    assoc alist ((==) x)

  This is just as concise and makes explicit the fact that a notion of
  equality is being used.  Furthermore, one should really write

    assoc alist (X.(=) x)

  which will remain correct as the notion of equality on X's changes.

  I've added these functions below each suffixed with a prime.  Someday, I'd
  like to make the primed be the default and suffix the unprimed with
  _deprecated.
*)
  
val assoc : 'a -> ('a * 'b) t -> 'b
val mem_assoc : 'a -> map:('a * 'b) t -> bool
val remove_assoc : 'a -> ('a * 'b) t -> ('a * 'b) t

val assoc' : ('a * 'b) t -> ('a -> bool) -> 'b option
val assoc_exn' : ('a * 'b) t -> ('a -> bool) -> 'b 
val mem_assoc' : ('a * 'b) t -> ('a -> bool) -> bool
val remove_assoc' : ('a * 'b) t -> ('a -> bool) -> ('a * 'b) t 
  
val sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
val stable_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
val fast_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
val merge : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list

(* CR sweeks: do we really want [hd] and [tl]?  Wouldn't it always be clearer
   to just use pattern matching?
*)
val hd : 'a list -> 'a option
val tl : 'a list -> ('a list) option
val hd_exn : 'a list -> 'a
val tl_exn : 'a list -> 'a list

(** [find_exn t ~f] returns the first element of [t] that satisfies [f].  It
    raises [Not_found] if there is no such element.
*)
val find_exn : 'a list -> f:('a -> bool) -> 'a
  
(** {6 Tail-recursive implementations of standard List operations} *)

(** Tail-recursive append. *)
val append : 'a list -> 'a list -> 'a list

(** Tail-recursive map. *)
val map : 'a list -> f:('a -> 'b) -> 'b list

(** Tail-recursive map2. *)
val map2 :'a list -> 'b list ->  f:('a -> 'b -> 'c) -> 'c list

val rev_map3 :
  'a list -> 'b list -> 'c list -> f:('a -> 'b -> 'c -> 'd) -> 'd list
val map3 :
  'a list -> 'b list -> 'c list -> f:('a -> 'b -> 'c -> 'd) -> 'd list

val rev_map_append :  'a list -> f:('a -> 'b) -> 'b list -> 'b list
(** [rev_map_append ~f l1 l2] reverses [l1] mapping [f] over each
    element, and appends the result to the front of [l2].  Very efficient,
    and tail-recursive! *)

(** Tail-recursive fold_right. *)
val fold_right : 'a list -> f:('a -> 'b -> 'b) -> init:'b -> 'b

(** Tail-recursive fold_right2. *)
val fold_right2 : 'a list -> 'b list -> f:('a -> 'b -> 'c -> 'c) -> init:'c -> 'c

(** Tail-recursive split. *)
val split : ('a * 'b) list -> 'a list * 'b list

(** Tail-recursive combine. *)
val combine : 'a list -> 'b list -> ('a * 'b) list


(** {6 Additional list operations} *)

(** mapi is just like map, but it also passes in the index of each
    element as the first argument to the mapped function. Tail-recursive. *)
val mapi : 'a list -> f:(int -> 'a -> 'b) -> 'b list

(** iteri is just like iter, but it also passes in the index of each
    element as the first argument to the iter'd function. Tail-recursive. *)
val iteri : 'a list ->  f:(int -> 'a -> unit) -> unit

(** fold_lefti is just like fold_left, but it also passes in the index of each
    element as the first argument to the folded function.  Tail-recursive. *)
val fold_lefti :  'a list -> f:(int -> 'b -> 'a -> 'b) -> init:'b -> 'b

(** fold_left_term is like fold_left, except that you can halt early.  The function to be
    folded should return a bool along with the new accumulator.  True indicates that it
    should continue, false means it should halt *)
val fold_left_term :'a list -> f:('b -> 'a -> bool * 'b) -> init:'b -> 'b

(** [reduce f [a1; ...; an]] is [f (... (f (f a1 a2) a3) ...) an].
    It fails on the empty list.  Tail recursive. *)
val reduce : 'a list -> f:('a -> 'a -> 'a) -> 'a

(** [best] is an alias for [reduce]. *)
val best : 'a list -> f:('a -> 'a -> 'a) -> 'a

(** min_combine combines two lists, possibly of different length, and returns
    a list the length of the shorter list.  Tail recursive.  *)
val min_combine : 'a list -> 'b list -> ('a * 'b) list

(** The final element of a list.  Fails on the empty list.  *)
val last : 'a list -> 'a

(** [dedup] (de-duplicate).  The same list with duplicates removed, but the
    order is not guaranteed. *)
val dedup : 'a list -> 'a list

(** [stable_dedup] Same as [dedup] but maintains the order of the list. *)
val stable_dedup : 'a list -> 'a list

(** [contains_dup] True if there are any two elements in the list which are the same. *)
val contains_dup : 'a list -> bool

(** [find_a_dup] returns a duplicate from the list (no guarantees about which
  duplicate you get), or None if there are no dups. *)
val find_a_dup : 'a list -> 'a option

(** [count f l] is the number of elements in [l] that satisfy the
    predicate [f].  *)
val count : 'a list -> f:('a -> bool) -> int

(** [range stride low high] is the list of integers from [low](inclusive)
    to [high](exclusive), stepping by [stride].  If unspecified, [stride]
    defaults to 1. *)
val range : ?stride:int -> int -> int -> int t

(** [frange] is similar to [range], but for floats. *)
val frange : ?stride:float -> float -> float -> float t

(** [init f n] is [[(f 0); (f 1); ...; (f (n-1))]].
    It is an error if [n < 0].
*)
val init : int -> f:(int -> 'a) -> 'a list

(* CR sweeks: I think this is a simpler and more natural type for unfold:

   val unfold : 'a -> ('a -> ('b * 'a) option) -> 'b list

  Here's the intended semantics.
   
  let unfold a f =
    let rec loop a l =
      match f a with
      | None -> l
      | Some (b, a) -> loop a (b :: l)
    in
    loop a []
  ;;

  Personally, I wouldn't have both unfold_left and unfold_right.  I'd just
  have unfold, and people can do a List.reverse if they want.

*)
(* (\** *)
(*    [unfold_right ~f ~stop ~next v] *)
(*    map f over the list built up of subsequent calls to next v until (stop x = true) *)
(* *\) *)
(* val unfold_right :  f:('a -> 'b) -> stop:('a -> bool) -> next:('a -> 'a) -> 'a -> 'b list *)

(* (\** [unfold_left ~f ~stop ~next v] *)
(*     Same as [unfold_right] but the resulting list is reversed. *)
(* *\) *)
(* val unfold_left :  f:('a -> 'b) -> stop:('a -> bool) -> next:('a -> 'a) -> 'a -> 'b list *)

(** [rev_filter_map f l] is the reversed sublist of [l] containing
    only elements for which [f] returns [Some e]. *)
val rev_filter_map : 'a list -> f:('a -> 'b option) -> 'b list

(** [filter_map f l] is the sublist of [l] containing only elements
    for which [f] returns [Some e].  *)
val filter_map : 'a list -> f:('a -> 'b option) -> 'b list

(** [filter_opt l] is the sublist of [l] containing only elements
    which are [Some e].  In other words, [filter_opt l] = [filter_map ident l]. *)
val filter_opt : 'a option list -> 'a list

(** [partition_map f l] partitions [l] into those elements which Pass [f]
    and those which Fail [f]. *)
val partition_map :'a list ->
  f:('a -> [< `Pass of 'b | `Fail of 'c ]) -> ('b list * 'c list)

(** [reverse_pairs l] reverses each tuple inside an association list. *)
val reverse_pairs : ('a * 'b) list -> ('b * 'a) list

(** [split_n n \[e1; ...; em\]] is [(\[e1; ...; en\], \[en+1; ...; em\])].
    If [n > m], [(\[e1; ...; em\], \[\])] is returned.  If [n <= 0],
    [(\[\], (\[e1; ...; em\])] is returned. *)
val split_n : int -> 'a list -> ('a list * 'a list)

(** [sub pos len l] is the [len]-element sublist of [l], starting at [pos]. *)
val sub : 'a list -> pos:int -> len:int -> 'a list

(** [slice l start stop] returns a new list including elements [l.(start)] through
    [l.(stop-1)], normalized python-style. *)
val slice : 'a list -> int -> int -> 'a list

(** [first_n n l] is [fst (split_n n l)]. *)
val first_n : int -> 'a list -> 'a list

val concat : 'a list list -> 'a list
(** Concatenate a list of lists.  The elements of the argument are all
   concatenated together (in the same order) to give the result.
   Tail recursive over outer and inner lists. *)

val concat_map : 'a list -> f:('a -> 'b list) -> 'b list
(**  Map a function over a list and concatenate the results. *)

(* CRv2 cfalls: I think we should remove this function and always use concat (or vice
   versa). *)
val flatten : 'a list list -> 'a list
(** Same as [concat]. *)

(* CRv2 rdouglass: rename assoc to assoc_exn, assoc_opt to assoc *)
(** [assoc_opt a l] returns the value associated with key [a] in the list of pairs [l]
 * as an option. *)
val assoc_opt : 'a -> ('a * 'b) list -> 'b option

val cons : 'a -> 'a list -> 'a list

val cartesian_product : 'a list -> 'b list -> ('a * 'b) list

(* CR sweeks: I'm not sure about the argument order here. *)
val to_string : ('a -> string) -> 'a t -> string

module Infix : sig
  val ( @ ) : 'a list -> 'a list -> 'a list
end
