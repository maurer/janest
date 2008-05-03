type 'a t = 'a option

include Container.S1 with type 'a container = 'a t
(** Options form a monad, where
   return x = Some x
   None >>= f = None
   Some x >>= f = f x
 *)
include Monad.S with type 'a monad = 'a t
include Sexpable.S1 with type 'a sexpable = 'a t

(** [is_none t] returns true iff t = None. *)
val is_none : 'a t -> bool

(** [is_some t] returns true iff t = Some x. *)
val is_some : 'a t -> bool

(** [map o f] map 'a option to a 'b option using ~f *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(** [call x f] run optional function on argument *)
val call : 'a -> f:('a -> unit) t -> unit

(** [apply x f] run optional function on argument and return an option *)
val apply : 'a -> f:('a -> 'b) t -> 'b t

(** [value None ~default] = [default]
    [value (Some x) ~default] = [x]
*)
val value : 'a t -> default:'a -> 'a

(** [value_exn (Some x)] = [x].
    [value_exn None] raises an exception. *)
val value_exn : 'a t -> 'a
  
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(* CRv2 cfalls:
   val wrap : ('a -> 'b) -> ('a -> 'b t)
   let wrap f x = try Some (f x) with _ -> None
*)
  
(* CRv2 dpowers:
  val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b
  let value_map t ~default ~f =
    match t with
    | None -> default
    | Some v -> f v 
*)