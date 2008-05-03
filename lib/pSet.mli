(** Polymorphic set module.  If you open Core.Std, this is your Set module. *)
open Sexplib

(* CRv2 sweeks: include Container.S1 *)

type 'elt t
val t_of_sexp: (Sexp.t -> 'elt) -> Sexp.t -> 'elt t
val sexp_of_t: ('elt -> Sexp.t) -> 'elt t -> Sexp.t
val empty: 'elt t
val is_empty: 'elt t -> bool
val mem: 'elt -> 'elt t -> bool
val add: 'elt -> 'elt t -> 'elt t
val singleton: 'elt -> 'elt t
val remove: 'elt -> 'elt t -> 'elt t
val union: 'elt t -> 'elt t -> 'elt t
val inter: 'elt t -> 'elt t -> 'elt t
val diff: 'elt t -> 'elt t -> 'elt t
val compare: 'elt t -> 'elt t -> int
val equal: 'elt t -> 'elt t -> bool
val subset: 'elt t -> 'elt t -> bool
val iter: f:('elt -> unit) -> 'elt t -> unit
val fold: f:('elt -> 'a -> 'a) -> 'elt t -> init:'a -> 'a
val for_all: f:('elt -> bool) -> 'elt t -> bool
val exists: f:('elt -> bool) -> 'elt t -> bool
val filter: f:('elt -> bool) -> 'elt t -> 'elt t
val filter_map: f:('elt -> 'a option) -> 'elt t -> 'a t
val partition: f:('elt -> bool) -> 'elt t -> 'elt t * 'elt t
val cardinal: 'elt t -> int (* CRv2 sweeks: rename as [length] *)
val elements: 'elt t -> 'elt list
val min_elt: 'elt t -> 'elt option
val min_elt_exn: 'elt t -> 'elt
val max_elt: 'elt t -> 'elt option
val max_elt_exn: 'elt t -> 'elt
val choose: 'elt t -> 'elt option
val choose_exn: 'elt t -> 'elt
val of_list: 'elt list -> 'elt t
val to_list: 'elt t -> 'elt list
val of_array: 'elt array -> 'elt t
val to_array: 'elt t -> 'elt array
val map: f:('a -> 'b) -> 'a t -> 'b t
val subsets: 'elt t -> int -> 'elt t t
val split: 'elt -> 'elt t -> 'elt t * bool * 'elt t
val group_by: 'elt t -> equiv:('elt -> 'elt -> bool) -> 'elt t list
