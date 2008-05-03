(*pp camlp4o -I `ocamlfind query bin_prot` -I `ocamlfind query type-conv` pa_type_conv.cmo pa_bin_prot.cmo *)

(** Polymorphic map module.  Just like the standard Map module, but with the
    order of the two type parameters inverted (we think [key,value] makes a lot
    more sense than [value,key].  There are also a few additional Map operations
    in here.  If you open Core.Std, this is your Map module. *)

(** {6 Polymorphic versions of standard Map operations} *)

open Sexplib

type ('key,'a) t with bin_io

val t_of_sexp: (Sexp.t -> 'key) -> (Sexp.t -> 'a) -> Sexp.t -> ('key,'a) t
val sexp_of_t: ('key -> Sexp.t) -> ('a -> Sexp.t) -> ('key,'a) t -> Sexp.t

  (** the empty map *)
val empty: ('key,'a) t

val is_empty: ('key, 'a) t -> bool
  (** Test whether a map is empty or not. *)

val cardinal : ('key, 'a) t -> int
(** [cardinal map] @return number of elements in [map]. *)

  (** returns a new map with the specified new binding;
      if the key was already bound, its previous binding disappears. *)
val add: key:'key -> data:'a -> ('key,'a) t -> ('key,'a) t

  (** returns the value bound to the given key, raising [Not_found] if none
      such exists *)
val find_exn: ('key,'a) t -> 'key -> 'a

val find: ('key,'a) t -> 'key -> 'a option

(* CRv2 CF: find_default *)

  (** returns a new map with any binding for the key in question removed *)
val remove: ('key,'a) t -> 'key -> ('key,'a) t

  (** [mem key map] tests whether [map] contains a binding for [key] *)
val mem: ('key,'a) t -> 'key -> bool

  (** iterator for map *)
val iter: f:(key:'key -> data:'a -> unit) -> ('key,'a) t -> unit

  (** returns new map with bound values replaced by f applied to the bound values *)
val map: f:('a -> 'b) -> ('key,'a) t -> ('key,'b) t

  (** like [map], but function takes both key and data as arguments *)
val mapi: f:(key:'key -> data:'a -> 'b) -> ('key,'a) t -> ('key,'b) t

  (** folds over keys and data in map *)
val fold: f:(key:'key -> data:'a -> 'b -> 'b) -> ('key,'a) t -> init:'b -> 'b

  (** filter for map *)
val filter: f:(key:'key -> data:'a -> bool) -> ('key,'a) t -> ('key,'a) t

  (** returns new map with bound values filtered by f applied to the bound values *)
val filter_map: f:('a -> 'b option) -> ('key,'a) t -> ('key,'b) t

  (** like [filter_map], but function takes both key and data as arguments*)
val filter_mapi: f:(key:'key -> data:'a -> 'b option) -> ('key,'a) t -> ('key,'b) t

val compare: ('a -> 'a -> int) -> ('key,'a) t -> ('key,'a) t -> int
  (** Total ordering between maps.  The first argument is a total ordering
      used to compare data associated with equal keys in the two maps. *)

val equal: ('a -> 'a -> bool) -> ('key,'a) t -> ('key,'a) t -> bool
  (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
      equal, that is, contain equal keys and associate them with
      equal data.  [cmp] is the equality predicate used to compare
      the data associated with the keys. *)

  (** returns list of keys in map *)
val keys: ('key,'a) t -> 'key list

  (** equivalent to [mem] *)
val has_key: ('key,'a) t -> 'key -> bool

  (** returns list of data in map *)
val data: ('key,'a) t -> 'a list

  (** creates map from association list with unique keys *)
val of_alist: ('key * 'a) list -> [ `Ok of ('key,'a) t | `Duplicate_key of 'key ]

  (** creates map from association list with unique keys.  Raises Failure if duplicate
      keys are found. *)
val of_alist_exn: ('key * 'a) list -> ('key,'a) t

  (** creates map from association list with possibly repeated keys. *)
val of_alist_multi: ('key * 'a) list -> ('key,'a list) t

  (** creates association list from map *)
val to_alist: ('key,'a) t -> ('key * 'a) list

(** {6 Additional operations on maps} *)

(** combines an association list into a map, folding together the bound
    values (for experts only) *)
val combine_alist: ('key * 'b) list -> init:'a -> f:('b -> 'a -> 'a)
  -> ('key,'a) t

(** merges two maps *)
val merge: f:(key:'key -> 'data1 option -> 'data2 option -> 'data3 option) ->
  ('key,'data1) t -> ('key,'data2) t -> ('key,'data3) t

module Infix : sig
  (** {!Map.find_exn}. *)
  val ( |= ) : ('key,'a) t -> 'key -> 'a
  (** {!Map.find} *)
  val ( |?= ) : ('key, 'a) t -> 'key -> 'a option
  (** {!Map.add}. *)
  val ( |< ) : ('key,'a) t -> 'key * 'a -> ('key,'a) t
end

val min_elt : ('key, 'data) t -> ('key * 'data) option
(** [min_elt map] @return Some [(key, data)] pair corresponding to the
    minimum key in [map], None if empty. *)

val min_elt_exn : ('key, 'data) t -> 'key * 'data
(** [min_elt_exn map] @return the [(key, data)] pair corresponding to the
    minimum key in [map], raises [Not_found] if [map] is empty. *)

val max_elt : ('key, 'data) t -> ('key * 'data) option
(** [max_elt map] @return Some [(key, data)] pair corresponding to the
    maximum key in [map], and None if [map] is empty. *)

val max_elt_exn : ('key, 'data) t -> 'key * 'data
(** [max_elt_exn map] @return the [(key, data)] pair corresponding to the
    maximum key in [map], raises an exception if [map] is empty. *)


(** same semantics as similar functions in List *)
val for_all : f:('data -> bool) -> ('key, 'data) t -> bool
val exists : f:('data -> bool) -> ('key, 'data) t -> bool
