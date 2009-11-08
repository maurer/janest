type t = Condition.t

val create : unit -> t
val equal : t -> t -> bool
val wait : t -> Mutex.t -> unit
val timedwait : t -> Mutex.t -> Time.t -> bool
val signal : t -> unit
val broadcast : t -> unit
