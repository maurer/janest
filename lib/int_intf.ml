open Interfaces

module type S = sig
  type t

  include Binable with type binable = t
  include Comparable with type comparable = t
  include Floatable with type floatable = t
  include Hashable.S_binable with type hashable = t
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t

  val num_bits : int

  val zero : t
  val one : t
  val minus_one : t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t

  

  val neg : t -> t

  val rem : t -> t -> t

  val succ : t -> t
  val pred : t -> t

  val abs : t -> t

  
  val max_int : t
  val min_int : t

  val bit_and : t -> t -> t
  val bit_or : t -> t -> t
  val bit_xor : t -> t -> t
  val bit_not : t -> t

  val decr : t ref -> unit
  val incr : t ref -> unit

  
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t

  val of_int_exn : int -> t
  val to_int_exn : t -> int
  val of_int32_exn : int32 -> t
  val to_int32_exn : t -> int32
  val of_int64_exn : int64 -> t
  val to_int64 : t -> int64

  val of_nativeint_exn : nativeint -> t
  val to_nativeint_exn : t -> nativeint
end
