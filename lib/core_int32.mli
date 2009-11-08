(* An int of exactly 32 bits, regardless of the machine.

   Side note: There's not much reason to want an int of at least 32 bits (i.e. 32 on
   32-bit machines and 63 on 64-bit machines) because Int63 is basically just as
   efficient.
*)

include Int_intf.S with type t = int32

val bits_of_float : float -> t
val float_of_bits : t -> float

val of_int : int -> t option
val to_int : t -> int option
val of_int32 : int32 -> t
val to_int32 : t -> int32
val of_int64 : int64 -> t option
val of_nativeint : nativeint -> t option
val to_nativeint : t -> nativeint
