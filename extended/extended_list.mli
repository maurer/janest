(** Extensions to [Core.Core_list].*)

open Core.Std

(** [inter l1 l2] returns a list without duplicates of all elements of l1 that are in l2 *)
val inter : 'a list -> 'a list -> 'a list

(** [diff l1 l2] returns a list of all elements of l1 that are not in l2 *)
val diff : 'a list -> 'a list -> 'a list

(** [classify l ~equal ~f] elements [x] and [y] of list [l] are assigned to the
    same class iff [equal (f x) (f y)] returns true. The default for [equal] is ( = ) *)
val classify : ?equal:('b -> 'b -> bool) -> f:('a -> 'b) -> 'a list ->
  ('b * 'a list) list

(** [take_while xs f] produces the longest prefix of [xs] on which [f] is always true. *)
val take_while : 'a list -> ('a -> bool) -> 'a list

(** [split_while xs f] splits [xs] into sublists [ys] and [zs] such that [ys] is
	the longest prefix on which [f] is true, and [zs] is the remaining elements. *)
val split_while : 'a list -> ('a -> bool) -> ('a list * 'a list)

(** [intersperse] returns a list with the given element placed between every two
  original elements.  e.g.  intersperse [1;2;3] 0  =  [1; 0; 2; 0; 3] *)
val intersperse : 'a list -> 'a -> 'a list

(** [enumerate_from n xs] returns a list of pairs constructed by pairing an
    incrementing counter, starting at [n], with the elements of [xs].
    e.g.  enumerate_from 1 [a,b,c]  =  [a,1; b,2; c,3] *)
val enumerate_from : int -> 'a list -> ('a * int) list



(** fold_left_term is like fold_left, except that you can halt early.  The function to be
    folded should return a bool along with the new accumulator.  True indicates that it
    should continue, false means it should halt *)
val fold_left_term : 'a list -> f:('b -> 'a -> [`Final of 'b | `Continue of 'b]) -> init:'b -> 'b

val max : ?cmp:('a -> 'a -> int) -> 'a list -> 'a option
val min : ?cmp:('a -> 'a -> int) -> 'a list -> 'a option

val max_exn : ?cmp:('a -> 'a -> int) -> 'a list -> 'a
val min_exn : ?cmp:('a -> 'a -> int) -> 'a list -> 'a


(* (\** *)
(*    [unfold_right ~f ~stop ~next v] *)
(*    map f over the list built up of subsequent calls to next v until (stop x = true) *)
(* *\) *)
(* val unfold_right :  f:('a -> 'b) -> stop:('a -> bool) -> next:('a -> 'a) -> 'a -> 'b list *)

(* (\** [unfold_left ~f ~stop ~next v] *)
(*     Same as [unfold_right] but the resulting list is reversed. *)
(* *\) *)
(* val unfold_left :  f:('a -> 'b) -> stop:('a -> bool) -> next:('a -> 'a) -> 'a -> 'b list *)
