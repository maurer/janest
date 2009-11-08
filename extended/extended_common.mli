

(** Pervasive functions. *)


val run_main : (unit -> unit) -> _


(** Like Exn.handle_uncaught but colors stderr in red. *)
val run_highlighted : (unit -> unit) -> _
