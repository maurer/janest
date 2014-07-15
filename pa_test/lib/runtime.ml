open Sexplib
open Sexplib.Conv

exception E of string * Sexp.t with sexp

let make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol =
  String.concat ""
    [ pos_fname
    ; ":"; string_of_int pos_lnum
    ; ":"; string_of_int (pos_cnum - pos_bol)
    ]

let string_of_loc {Lexing.pos_fname; pos_lnum; pos_cnum; pos_bol} =
  make_location_string ~pos_fname ~pos_lnum ~pos_cnum ~pos_bol

let sexp_of_loc t =
  Sexp.Atom (string_of_loc t)

let failwith message sexp = raise (E (message, sexp))
