(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
open Std_internal

(** Module for simple closed intervals over arbitrary types that are ordered
    correctly using polymorphic compare. *)

module type S = Interval_intf.S
module type S1 = Interval_intf.S1

include Interval_intf.S1

module Make(M : sig
  type t
  include Comparable.S with type comparable = t
  include Sexpable.S with type sexpable = t
  include Binable.S with type binable = t
end)
  : S with type bound = M.t and type 'a poly_t = M.t t
  
    
(** Specific optimized modules *)

module Float : S with type bound = Float.t and type 'a poly_t = Float.t t 
module Int : S with type bound = Core_int.t and type 'a poly_t = Core_int.t t
module Time : S with type bound = Time.t and type 'a poly_t = Time.t t
