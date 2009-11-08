

module type S = sig
  type hashable
  
  val equal : hashable -> hashable -> bool
  
  val hash : hashable -> int
  module Table : Core_hashtbl.S with type key = hashable
  module Hash_set : Hash_set.S with type elt = hashable
  module Hash_queue : Hash_queue.S with type Key.t = hashable
  module Hash_heap : Hash_heap.S with type Key.t = hashable
end

module Make (T : Core_hashtbl.HashedType) : S with type hashable = T.t = struct
  include T
  type hashable = t
  module Table = Core_hashtbl.Make (T)
  module Hash_set = Hash_set.Make (T)
  module Hash_queue = Hash_queue.Make (T)
  module Hash_heap = Hash_heap.Make (T)
end

module type S_binable = sig
  type hashable
  val equal : hashable -> hashable -> bool
  val hash : hashable -> int
  module Table : sig
    include Core_hashtbl.S with type key = hashable
    include Binable.S1 with type 'a binable = 'a t
  end
  module Hash_set : Hash_set.S_binable with type elt = hashable
  module Hash_queue : Hash_queue.S with type Key.t = hashable
  module Hash_heap : Hash_heap.S with type Key.t = hashable
end

module Make_binable (T : sig
  include Core_hashtbl.HashedType
  include Binable.S with type binable = t
end) : S_binable with type hashable = T.t = struct
  module Table = Core_hashtbl.Make_binable (T)
  include T
  type hashable = t
  module Hash_set = Hash_set.Make_binable (T)
  module Hash_queue = Hash_queue.Make (T)
  module Hash_heap = Hash_heap.Make (T)
end
