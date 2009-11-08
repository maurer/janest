type 'a t = 'a ref = { mutable contents : 'a }

val create : 'a -> 'a t

val (!) : 'a t -> 'a

val (:=) : 'a t -> 'a -> unit
  
(** [equal t1 t2] returns true if [t1] and [t2] are the same ref cell. *)
val equal : 'a t -> 'a t -> bool

(** [swap t1 t2] swaps the values in [t1] and [t2]. *)
val swap : 'a t -> 'a t -> unit

(** [replace t f] is [t := f !t] *)
val replace : 'a t -> ('a -> 'a) -> unit
