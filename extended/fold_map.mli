(* Creates a map that folds in new values.  An example would be a multi-map in which a key
   is initialized with the empty list as its value, and adding a new key/value pair appends
   the value to the key's list. *)



open Core.Std

module type Fold = sig
  type t
  type data
  val init : t
  val f : t -> data -> t
end

module type Fold_sexpable = sig
  include Fold
  include Sexpable with type sexpable = t
end

module Cons (T : sig type t  end) : Fold
  with type t = T.t list
  and type data = T.t

module Cons_sexpable (T:Sexpable) : Fold_sexpable
  with type t = T.sexpable list
  and type data = T.sexpable


module Multiply : Fold_sexpable with type t = int and type data = int
module Add      : Fold_sexpable with type t = int and type data = int

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
  val of_list : ('a * in_value) list -> 'a t
  val for_all   : f:(out_value -> bool) -> _ t -> bool
  val exists    : f:(out_value -> bool) -> _ t -> bool
  val to_map    : 'a t -> ('a , out_value) Map.t
  val of_map    : ('a , out_value) Map.t -> 'a t
end

module type Fold_map_sexpable = sig
  include Fold_map
  include Sexpable.S1 with type 'key sexpable = 'key t
end

module Make (Fold : Fold) : Fold_map
  with type in_value = Fold.data
  and type out_value = Fold.t

module Make_sexpable (Fold_sexpable : Fold_sexpable) : Fold_map_sexpable
  with type in_value = Fold_sexpable.data
  and type out_value = Fold_sexpable.t
