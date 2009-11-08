(** Extensions to [Core.Exn].*)

open Core.Std

(* Hopefully in the near future we won't need this function anymore because our exception printing*)

(** The [to_string] function is slightly tweaked to avoid escaping the string
content of [Failure]. *)
val to_string : exn -> string

(** This is also an ever so slight variation of [to_string] target more at user
than developers ([Failure s] is just printed as [s])
*)
val to_string_hum : exn -> string

