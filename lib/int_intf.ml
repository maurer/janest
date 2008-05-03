open Interfaces

module type S = sig
  type t

  include Binable with type binable = t
  include Comparable with type comparable = t
  include Floatable with type floatable = t
  include Hashable with type hashable = t
  include Setable with type setable = t
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t

  val zero : t
  val one : t
  val minus_one : t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t

  (* CRv2 sweeks: Should we get rid of {add,div,mult,sub}? *)
  val add : t -> t -> t
  val div : t -> t -> t
  val mul : t -> t -> t
  val neg : t -> t
  val rem : t -> t -> t
  val sub : t -> t -> t

  val succ : t -> t
  val pred : t -> t

  val abs : t -> t

  (* CRv2 yminsky: Should maybe be max_value and min_value, to follow Time.*? *)
  val max_int : t
  val min_int : t

  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val decr : t ref -> unit
  val incr : t ref -> unit

  (* CRv2 sweeks: Would it be easier to read if we chose symbols for the below?
     Maybe: <<, >>, and ~>> ?
  *)
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
end
