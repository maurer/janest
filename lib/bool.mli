type t = bool

open Interfaces
include Comparable with type comparable = t
include Hashable with type hashable = t
include Sexpable with type sexpable = t
include Binable with type binable = t
include Stringable with type stringable = t

