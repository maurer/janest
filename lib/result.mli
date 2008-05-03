open Interfaces

type ('a, 'err) t =
  | Ok of 'a
  | Error of 'err

include Sexpable.S2 with type ('a,'err) sexpable = ('a,'err) t

(* Monad-like operators *)
val bind : ('a,'err) t -> ('a -> ('b,'err) t) -> ('b,'err) t
val return : 'a -> ('a,'err) t

val is_ok : ('a, 'err) t -> bool
val is_error : ('a, 'err) t -> bool

val iter : f:('a -> unit) -> ('a, 'err) t -> unit
val map : f:('a -> 'c) -> ('a, 'err) t -> ('c, 'err) t
val call : f:(('a -> unit), 'err) t -> 'a -> unit
val apply : f:(('a -> 'b), 'err) t -> 'a -> ('b, 'err) t

