open Core.Std

val format : Sexp.t -> Pp.t

val is_atom : Sexp.t -> bool
val is_list : Sexp.t -> bool

val atom : string -> Sexp.t
val list : Sexp.t list -> Sexp.t

(**
   The ocaml pretty printer (used by sexplib) is a speed daemon  but is,
   sadly enough, produces wrong output (e.g it overflows in places where this
   could have avoided). This uses a printer from wadler's a prettier printer to
   output strings suited to human consumption.
*)
val to_string_hum' : Sexp.t -> string

(**
   A more readable but less compact pretty printer than the one bundled by
   sexplib. This is going through a test period at which point it might
   make it in sexplib. It uses ocaml's pretty-printing library so it is both
   fast and broken.
*)
val pp_hum' : Format.formatter -> Sexp.t -> unit

val to_string_hum' : Sexp.t -> string

val comment : string -> string

val print_diff : ?oc:out_channel -> Sexp.t -> Sexp.t -> unit

(* Returns a smaller sexp by replacing sections with "...".  Will try to show parts of the
   sexp "near" sub_sexp.

   Limiting size to length a string length is less efficient than a certain depth.  The
   meaning of a given depth is arbitrary except that more depth gives you a bigger sexp.  Try
   100 or so. *)
val summarize : Sexp.t -> sub_sexp:Sexp.t -> size:[ `depth of int | `string of int ] -> Sexp.t
