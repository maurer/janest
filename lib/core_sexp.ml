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

(* The purpose of these modules is to allow bin_io to work with these special sexp types.
   The more direct method of adding "with bin_io" at the point of the initial declaration of
   the types is not possible because sexplib does not (should not) depend on bin_io. *)


module Sexp_option = struct
  type 'a sexp_option = 'a option with bin_io
end

module Sexp_list = struct
  type 'a sexp_list = 'a list with bin_io
end
