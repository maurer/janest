(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Binable"

include Bin_prot.Binable

module Of_stringable (M : Stringable.S) =
  Bin_prot.Utils.Make_binable (struct
    module Binable = struct
      type t = string with bin_io
      type binable = t
    end
    type t = M.stringable
    let to_binable = M.to_string
    let of_binable = M.of_string
  end)
