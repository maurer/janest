include Int_intf.S with type t = int32

val bits_of_float : float -> t
val float_of_bits : t -> float

val of_int : int -> t option
val of_int_exn : int -> t
val to_int : t -> int option
val to_int_exn : t -> int
val of_int32 : int32 -> t
val to_int32 : t -> int32
val of_int64 : int64 -> t option
val of_int64_exn : int64 -> t
val to_int64 : t -> int64
val of_nativeint : nativeint -> t option
val of_nativeint_exn : nativeint -> t
val to_nativeint : t -> nativeint
