(** Memoization code *)

(** Returns memoized version of any function with a single argument. *)
val general : ('a -> 'b) -> ('a -> 'b)

(** Returns memoized version of any function where data is kept
    until argument changes. *)
val ident : ('a -> 'b) -> ('a -> 'b)

(** Returns memoized version of any function with argument unit. *)
val unit : (unit -> 'a) -> (unit -> 'a)

