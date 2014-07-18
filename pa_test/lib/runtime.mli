(** Functions called by the generated code *)
val sexp_of_loc : Lexing.position -> Sexplib.Sexp.t
val failwith : string -> Sexplib.Sexp.t -> _

val make_location_string
  :  pos_fname:string
  -> pos_lnum:int
  -> pos_cnum:int
  -> pos_bol:int
  -> string

(** [string_of_loc] is used to implement [sexp_of_loc], and
    is exposed at least so it can be rebound in core_kernel. *)
val string_of_loc : Lexing.position -> string
