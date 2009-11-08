(* A function for trivial function combinators.  Generally, that means functions with no
   non-polymorphic types.  These functions often increase terseness at the expense of
   readability, so please use them with restraint. *)

(** A 'pipe' operator. *)
val ( |! ) : 'a -> ( 'a -> 'b) -> 'b

(** produces a function that just returns its first argument *)
val const : 'a -> _ -> 'a

(** Negates a function *)
val non : ('a -> bool) -> 'a -> bool

(** [forever f] runs [f ()] until it throws an exception and returns the exception.
    This function is useful for read_line loops, etc. *)
val forever : (unit -> unit) -> exn

(** The identity function*)
external ident : 'a -> 'a = "%identity"
