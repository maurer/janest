(** unpack a signed 1 byte int from the given buf starting at pos *)
val unpack_signed_8 : buf:string -> pos:int -> int

(** encode and pack the specified int as a signed 1 byte int, and place it in
    the buffer starting at pos *)
val pack_signed_8 : buf:string -> pos:int -> int -> unit

(** unpack a signed 2 byte int from the given buf starting at pos *)
val unpack_signed_16 : buf:string -> pos:int -> int

(** encode and pack the specified int as a signed 2 byte int, and place it in
    the buffer starting at pos *)
val pack_signed_16 : buf:string -> pos:int -> int -> unit

(** unpack a signed 4 byte int32 from the given buf starting at pos *)
val unpack_signed_32 : buf:string -> pos:int -> int32

(** unpack a signed 4 byte int32 from the given buf starting at
    pos. DO NOT USE ON A 32 BIT COMPUTER! *)
val unpack_signed_32_int : buf:string -> pos:int -> int

(** encode and pack the specified int32 as a signed 4 byte int, and
    place it in the buffer starting at pos *)
val pack_signed_32 : buf:string -> pos:int -> Int32.t -> unit

(** encode and pack the specified int32 as a signed 4 byte int, and
    place it in the buffer starting at pos. DO NOT USE ON A 32 BIT
    COMPUTER! *)
val pack_signed_32_int : buf:string -> pos:int -> int -> unit

val unpack_float : buf:string -> pos:int -> float
val pack_float : buf:string -> pos:int -> float -> unit

val test : unit -> unit
