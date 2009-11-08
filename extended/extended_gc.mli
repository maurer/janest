(** Extensions to [Core.Gc].*)

val without_compactions : ?logger:(string -> unit) -> f:('a -> 'b) -> 'a -> 'b
(** [without_compactions f a] will call f a so that Gc.compact is never called
    during its execution, then restore compactions to the previous setting. *)

