module type S = sig
  type hashable
  val equal : hashable -> hashable -> bool
  val hash : hashable -> int
  module Table : Core_hashtbl.S with type key = hashable
  module Hash_set : Hash_set.Mono with type elt = hashable
  module Hash_queue : Hash_queue.S with type Key.t = hashable
end

module Make (T : Core_hashtbl.HashedType) : S with type hashable = T.t = struct
  include T
  type hashable = t
  module Table = Core_hashtbl.Make (T)
  module Hash_set = Hash_set.Make (T)
  module Hash_queue = Hash_queue.Make (T)
end
