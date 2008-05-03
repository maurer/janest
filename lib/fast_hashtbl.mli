(** Hashtbl which stores a reference to the data for faster replace.
   Initial adds are _marginally_ slower, while replace is way
   faster.  Lookups etc. are the same. *)
open Sexplib
  
type ('key,'value) t

val sexp_of_t :
  ('key -> Sexp.t) ->
  ('value -> Sexp.t) ->
  ('key, 'value) t -> Sexp.t

val create : int -> ('key,'value) t
val clear : ('key,'value) t -> unit
val add : ('key,'value) t -> key:'key -> data:'value -> unit
val find_exn : ('key,'value) t -> 'key -> 'value
val find : ('key,'value) t -> 'key -> 'value option
val mem : ('key,'value) t -> 'key -> bool
val remove : ('key,'value) t -> 'key -> unit
val replace : ('key,'value) t -> key:'key -> data:'value -> unit
val iter : f:(key:'key -> data:'value -> unit) -> ('key,'value) t -> unit
val fold : f:(key:'key -> data:'value -> 'c -> 'c) -> ('key,'value) t -> init:'c -> 'c
val length : ('key,'value) t -> int
