type t = bool

open Interfaces
include Comparable with type comparable = t
include Hashable with type hashable = t
include Setable with type setable = t
include Sexpable with type sexpable = t
include Stringable with type stringable = t

val not : t -> t
val of_int : int -> t
val to_int : t -> int
