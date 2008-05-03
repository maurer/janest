(*pp camlp4o -I `ocamlfind query type-conv` -I `ocamlfind query sexplib` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core_sexp"

module Sexp = Sexplib.Sexp

include Sexp

type sexpable = t
  
include (struct
  type t = Sexp.t = Atom of string | List of t list with sexp, bin_io
  type binable = t
  type sexpable = t
end : Interfaces.Binable with type binable = t)
