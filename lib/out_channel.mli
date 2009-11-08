type t = out_channel

val stdout : t
val stderr : t


val create : ?binary:bool -> ?append:bool -> ?perm:int -> string -> t

val close : t -> unit
val close_noerr : t -> unit

val set_binary_mode : t -> bool -> unit

val flush : t -> unit


val output : t -> buf:string -> pos:int -> len:int -> unit
val output_string : t -> string -> unit
val output_char : t -> char -> unit
val output_byte : t -> int -> unit
val output_binary_int : t -> int -> unit
val output_value : t -> 'a -> unit

val newline : t -> unit
val output_lines : t -> string list -> unit

val seek : t -> int64 -> unit
val pos : t -> int64
val length : t -> int64
