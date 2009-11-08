(** Extensions to [Core.Float].*)

val pretty : float -> string
(**
   pretty-print a float using no more than four characters, using abberviations
   k, m, g, t.
*)

val to_string_hum : float -> string
