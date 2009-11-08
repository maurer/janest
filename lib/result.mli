type ('ok, 'err) t =
  | Ok of 'ok
  | Error of 'err

include Sexpable.S2 with type ('a,'err) sexpable = ('a,'err) t
include Binable.S2 with type ('a,'err) binable = ('a,'err) t
include Monad.S2 with type ('a,'err) monad = ('a,'err) t

val fail : 'err -> (_, 'err) t

val is_ok : (_, _) t -> bool
val is_error : (_, _) t -> bool

val ok : ('a, _) t -> 'a option
val error : (_, 'err) t -> 'err option

val iter : ('a, _) t -> f:('a -> unit) -> unit
val map : ('a, 'err) t  -> f:('a -> 'c) -> ('c, 'err) t

(* these two are rarely used *)
val call : f:(('a -> unit), _) t -> 'a -> unit
val apply : f:(('a -> 'b), 'err) t -> 'a -> ('b, 'err) t

(** useful in [List.partition] *)
val ok_fst : ('ok, 'err) t -> [ `Fst of 'ok | `Snd of 'err ]

val trywith : (unit -> 'a) -> ('a, exn) t


