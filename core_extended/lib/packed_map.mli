(** A packed map is a map from keys to values, represented using a packed array of
    key-value tuples which is sorted by key. Construction is very slow, but lookup is a
    reasonable speed. The main purpose is to be able to construct very large lookup tables
    that don't have much GC overhead. *)
open Core.Std

module type Key = sig
  type t with sexp, bin_io
  include Comparable.S with type t := t
  module Packed_array : Packed_array.S with type elt := t
end

module type Value = sig
  type t with sexp, bin_io
  module Packed_array : Packed_array.S with type elt := t
end

module type S = sig
  type t     with sexp, bin_io
  type key   with sexp, bin_io
  type value with sexp, bin_io

  val empty            : t

  val of_alist         : (key * value) list -> t
  val to_alist         : t -> (key * value) list
  val of_aarray        : (key * value) array -> t
  val of_sorted_aarray : (key * value) array -> t
  val of_hashtbl       : (key, value) Hashtbl.t -> t

  val find             : t -> key -> value option
  val mem              : t -> key -> bool
  val iter             : t -> f:(key:key -> data:value -> unit) -> unit
end

module Make (K : Key) (V : Value) : S with type key := K.t and type value := V.t
