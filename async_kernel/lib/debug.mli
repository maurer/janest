(** Internal Async debugging functions. *)

open Core.Std

include module type of Config.Print_debug_messages_for

(* Calls to [Debug.log] should look like [if Debug.??? then Debug.log ...]. *)
val log        : string -> 'a -> ('a -> Sexp.t) -> unit
val log_string : string ->                         unit
