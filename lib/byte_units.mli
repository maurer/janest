(* Conversions between units of measure based on bytes. *)

type t = [
| `Bytes of Core_int63.t
| `Kilobytes of float
| `Megabytes of float
| `Gigabytes of float
| `Words of Core_int63.t
]

include Sexpable.S with type sexpable = t
include Binable.S with type binable = t
include Comparable.S with type comparable = t
include Hashable.S with type hashable = t

val bytes : t -> Core_int63.t
val kilobytes : t -> float
val megabytes : t -> float
val gigabytes : t -> float
val words : t -> Core_int63.t

