(* CRv2 YM: Setable should include Mappable, and should all be included by default in
   Comparable *)
module type S = sig
  type setable
  module Set : Set.S with type elt = setable
end

module Make (T : Set.OrderedType) : S with type setable = T.t = struct
  include T
  type setable = t
  module Set = Set.Make (T)
end

