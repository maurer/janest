type 'a t = 'a option

include Container.S1 with type 'a container = 'a t
(** Options form a monad, where
   return x        =  Some x
   None >>= f      =  None
   Some x >>= f    =  f x
 *)

include Monad.S with type 'a monad = 'a t
include Sexpable.S1 with type 'a sexpable = 'a t

(** [is_none t] returns true iff t = None. *)
val is_none : 'a t -> bool

(** [is_some t] returns true iff t = Some x. *)
val is_some : 'a t -> bool

(** [value_map None     ~default ~f] = [default]
    [value_map (Some x) ~default ~f] = [f x] *)
val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b

(** [map o f] map 'a option to a 'b option using ~f *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(** [map2 o f] map 'a option and 'b option to a 'c option using ~f *)
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

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

(** [value_exn_message message (Some x)] = [x].
    [value_exn_message message None] raises exception Failure with
    string [message]. *)
val value_exn_message : string -> 'a t -> 'a

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val some : 'a -> 'a t

val both : 'a option -> 'b option -> ('a * 'b) option

(** [wrap_exn f] provides an exception-free interface to [f].  It
    returns a function that returns [Some x] whenever [f] would have
    returned [x] and [None] whenever [f] would have raised an exception.
*)
val wrap_exn : ('a -> 'b) -> ('a -> 'b t)
