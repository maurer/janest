(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
open Std_internal

module type Unit_ref = sig
  (* note, use equal (in this module) for comparison, poly compare
     will not work correctly. Use the hashtbl included with this
     module. *)

  type t with sexp_of

  include Hashable with type hashable = t

  val create : unit -> t
  val to_string : t -> string
end

module type Id = sig
  type t

  include Binable with type binable = t
  include Comparable with type comparable = t
  include Floatable with type floatable = t
  include Hashable with type hashable = t
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t

  val create : unit -> t
end 
  
(** A unique identifier based on a small allocated block, and an
    integer for hashing. All ids held in memory remain unique no
    matter how many ids have been created. *)
module Unit_ref (Z : sig end) : Unit_ref

(** an abstract unique identifier based on 64 bit integers. *)
module Int63 (Z : sig end) : Id
module Int64 (Z : sig end) : Id
