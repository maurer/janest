type t = exn

(** Raised when finalization after an exception failed, too.
    The first exception argument is the one raised by the initial
    function, the second exception the one raised by the finalizer. *)
exception Finally of exn * exn

val to_string : t -> string

val register_converter : (t -> string option) -> unit
  
val sexp_of_t : t -> Sexplib.Sexp.t


(** A common idiom used in functional languages, that executes [f] and
    afterwards executes [finally], whether [f] throws an exception or
    not.
*)
val protectx : f:('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b
  
val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a

val pp : Format.formatter -> t -> unit
