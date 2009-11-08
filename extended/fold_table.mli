open Core.Std

module type Fold = Fold_map.Fold

module type Fold_map = sig
  type in_value
  type out_value

  type 'key t



  val empty     : _ t
  val singleton : 'a -> in_value -> 'a t
  val is_empty  : _ t -> bool
  val cardinal  : _ t -> int
  val add       : key:'a -> data:in_value -> 'a t -> 'a t
  val find      : 'a t -> 'a -> out_value
  val remove    : 'a t -> 'a -> 'a t
  val set       : key:'a -> data:out_value -> 'a t -> 'a t
  val mem       : 'a t -> 'a -> bool
  val iter      : f:(key:'a -> data:out_value -> unit) -> 'a t -> unit
  val fold      : f:(key:'a -> data:out_value -> 'b -> 'b) -> 'a t -> init:'b -> 'b
  val filter    : f:(key:'a -> data:out_value -> bool) -> 'a t -> 'a t
  val keys      : 'a t -> 'a list
  val data      : _ t -> out_value list
  val to_alist  : 'a t -> ('a * out_value) list
  val for_all   : f:(out_value -> bool) -> _ t -> bool
  val exists    : f:(out_value -> bool) -> _ t -> bool
end

module Make (Fold : Fold) : Fold_map
  with type in_value = Fold.data
  and type out_value = Fold.t
