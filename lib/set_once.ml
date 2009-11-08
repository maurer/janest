(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Set_once"

open Result

exception Already_set

type 'a t = 'a option ref with sexp

type 'a sexpable = 'a t

let create () = ref None

let set_exn t v =
  match !t with
  | None -> t := Some v
  | Some _ -> raise Already_set

let set t v =
  try Ok (set_exn t v)
  with Already_set -> Error "already set"

let get t = !t
