(* Functions which improve terseness in code but can reduce readability
   too much for production code *)

(** Functional composition. Takes two functions f and g and returns a
    third function equivalent to [fun x -> f (g x)] *)
(*  Note for haskellers: This is not the $ in Haskell. Our version of Haskell's $
    is called |! in Core and takes its arguments in reverse. This operator is like
    Haskell's . *)
val ( $ ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

