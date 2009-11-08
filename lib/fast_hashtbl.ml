(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)

(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Fast_hashtbl"

(* Hashtbl with a faster replace operation, but with the cost of an extra reference
   for every entry. *)

module Hashtbl = MoreLabels.Hashtbl

type ('key,'data) t = ('key,'data ref) Hashtbl.t with sexp_of

let create n = Hashtbl.create n
let clear = Hashtbl.clear
let add t ~key ~data = Hashtbl.add t ~key ~data:(ref data)
let find_exn t key = ! (Hashtbl.find t key)
let find t key = try Some (! (Hashtbl.find t key)) with Not_found -> None
let remove = Hashtbl.remove
let mem = Hashtbl.mem
let replace t ~key ~data =
  try
    (Hashtbl.find t key) := data
  with
  Not_found -> Hashtbl.add t ~key ~data:(ref data)
let iter ~f t = Hashtbl.iter ~f:(fun ~key ~data -> f ~key ~data:!data) t
let fold ~f t ~init = Hashtbl.fold ~f:(fun ~key ~data -> f ~key ~data:!data) t ~init
let length = Hashtbl.length
