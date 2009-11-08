(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "bool"

let invalid_argf = Core_printf.invalid_argf

module T = struct
  type t = bool with bin_io, sexp

  type binable = t
  type comparable = t
  type sexpable = t
  type stringable = t

  let compare (t : t) t' = compare t t'
  (* we use physical equality here because for bools it is the same *)
  let equal (t : t) t' = t == t'
  let hash x = if x then 1 else 0
end

include T

let of_string = function
  | "true" -> true
  | "false" -> false
  | s -> invalid_argf "Bool.of_string: expected true or false but got %s" s ()
;;

let to_string = string_of_bool

let min (x : t) y = if x < y then x else y
let max (x : t) y = if x > y then x else y
let ascending = compare
let descending x y = compare y x
let ( >= ) (x : t) y = x >= y
let ( <= ) (x : t) y = x <= y
let ( = ) = equal
let ( > ) (x : t) y = x > y
let ( < ) (x : t) y = x < y
let ( <> ) (x : t) y = x != y

(* Making bool hashable may seem frivolous, but consider an aggregate type with
   a bool in it that needs a custom hash function. *)
include Hashable.Make (T)
module Set = Core_set.Make (T)
module Map = Core_map.Make (T)

