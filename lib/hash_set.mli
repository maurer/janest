open Hash_set_intf

include S1

module type S = S
module type S_binable = S_binable

module Make (H : Core_hashtbl.HashedType) : S with type elt = H.t

module Make_binable (H : sig
  include Core_hashtbl.HashedType
  include Binable.S with type binable = t
end) : sig
  include S with type elt = H.t
  include Binable.S with type binable = t
end
