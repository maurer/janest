(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
module type Infix = sig
  type comparable
  val ( >= ) : comparable -> comparable -> bool
  val ( <= ) : comparable -> comparable -> bool
  val ( = ) : comparable -> comparable -> bool
  val ( > ) : comparable -> comparable -> bool
  val ( < ) : comparable -> comparable -> bool
  val ( <> ) : comparable -> comparable -> bool
end

module type S = sig
  include Infix
  val compare : comparable -> comparable -> int
  val ascending : comparable -> comparable -> int
  val descending : comparable -> comparable -> int
  val min : comparable -> comparable -> comparable
  val max : comparable -> comparable -> comparable
  module Map : Core_map.S with type key = comparable
  module Set : Core_set.S with type elt = comparable
end

module Poly (T : sig
  type t
  include Sexpable.S with type sexpable = t
end) : S with type comparable = T.t = struct
  type comparable = T.t
  include Pervasives                    (* for Infix *)
  let ascending = compare
  let descending x y = compare y x
  module C = struct
    include T
    let compare = compare
  end
  module Map = Core_map.Make (C)
  module Set = Core_set.Make (C)
end

module Make (T : sig
  type t
  include Sexpable.S with type sexpable = t
  val compare : t -> t -> int
end) : S with type comparable = T.t = struct
  type comparable = T.t

  let compare = T.compare
  let ascending = compare
  let descending t t' = compare t' t

  module Infix = struct
    let (>) a b = compare a b > 0
    let (<) a b = compare a b < 0
    let (>=) a b = compare a b >= 0
    let (<=) a b = compare a b <= 0
    let (=) a b = compare a b = 0
    let (<>) a b = compare a b <> 0
  end
  include Infix

  let min t t' = if t <= t' then t else t'
  let max t t' = if t >= t' then t else t'

  module Map = Core_map.Make(T)
  module Set = Core_set.Make(T)
end

(** Inherit comparability from a component. *)
module Inherit (C : S)
  (T : sig
    type t
    include Sexpable.S with type sexpable = t
    val component : t -> C.comparable
  end)
  : S with type comparable = T.t = struct

    type comparable = T.t
    (* We write [binary] in this way for performance reasons.  It is always
     * applied to one argument and builds a two-argument closure.
     *)
    let binary f = (); fun t t' -> f (T.component t) (T.component t')
    let compare = binary C.compare
    let (>=) = binary C.(>=)
    let (<=) = binary C.(<=)
    let (=) = binary C.(=)
    let (>) = binary C.(>)
    let (<) = binary C.(<)
    let (<>) = binary C.(<>)
    let ascending = binary C.ascending
    let descending = binary C.descending
    let min t t' = if t <= t' then t else t'
    let max t t' = if t >= t' then t else t'

    module M = struct
      include T
      let compare = compare
    end

    module Map = Core_map.Make (M)
    module Set = Core_set.Make (M)
end
