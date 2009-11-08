(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core_extended.Fold_map"

open Core.Std
module Map_intf = Core.Core_map_intf

(* argument to functor: maintains per-key state *)
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


(* a common example of Fold -- creates a multi-map *)
module Cons (T : sig
  type t
end) :
  Fold
  with type t = T.t list
  and type data = T.t
  =
struct
  type data = T.t
  type t = T.t list
  let init = []
  let f list x = x :: list
end

module Cons_sexpable (T:Sexpable) = struct
  include Cons (struct include T type t = sexpable end)
  type sexpable = T.sexpable list
  let t_of_sexp = list_of_sexp T.t_of_sexp
  let sexp_of_t = sexp_of_list T.sexp_of_t
end


(* Fold for adding, e.g. symbol positions *)
module Add = struct
  type data = int
  include Int
  let init = 0
  let f = (+)
end

module Multiply = struct
  type data = int
  include Int
  let init = 1
  let f = ( * )
end

(* A map where the type of values added can be different from the type of values gotten out. *)
module type Fold_map = sig
  type in_value
  type out_value

  type 'key t

  val empty: _ t
  val singleton: 'a -> in_value -> 'a t
  val is_empty: _ t -> bool
  val cardinal : _ t -> int
  val add: key:'a -> data:in_value -> 'a t -> 'a t
  val find: 'a t -> 'a -> out_value
  val remove: 'a t -> 'a -> 'a t
  val set: key:'a -> data:out_value -> 'a t -> 'a t
  val mem: 'a t -> 'a -> bool
  val iter: f:(key:'a -> data:out_value -> unit) -> 'a t -> unit
  val fold: f:(key:'a -> data:out_value -> 'b -> 'b) -> 'a t -> init:'b -> 'b
  val filter: f:(key:'a -> data:out_value -> bool) -> 'a t -> 'a t
  val keys: 'a t -> 'a list
  val data: _ t -> out_value list
  val to_alist: 'a t -> ('a * out_value) list
  val of_list : ('a * in_value) list -> 'a t
  val for_all : f:(out_value -> bool) -> _ t -> bool
  val exists  : f:(out_value -> bool) -> _ t -> bool
  val to_map  : 'a t -> ('a , out_value) Map.t
  val of_map  : ('a , out_value) Map.t -> 'a t
end

module type Fold_map_sexpable = sig
  include Fold_map
  include Sexpable.S1 with type 'key sexpable = 'key t
end

(* implementation *)
module Make (Fold : Fold) = struct
  type in_value = Fold.data
  type out_value = Fold.t               (* with sexp *)

  include (Map : Map_intf.Gen(Map.T).S)
  type 'a t = ('a,Fold.t) Map.t (* with sexp *)

  let to_map = ident
  let of_map = ident

  let singleton key in_value = Map.singleton key (Fold.f Fold.init in_value)
  let find t key =
    match Map.find t key with
    | None -> Fold.init
    | Some v -> v

  let add ~key ~data t =
    Map.add ~key ~data:(Fold.f (find t key) data) t

  let set = Map.add

  let of_list l = List.fold_left l ~init:empty ~f:(fun t (key,data) ->
    add t ~key ~data)
end

module Make_sexpable (Fold_sexpable : Fold_sexpable) : Fold_map_sexpable
  with type in_value = Fold_sexpable.data
  and type out_value = Fold_sexpable.t
  = struct
    include Make (Fold_sexpable)
    type 'key sexpable = 'key t
    let t_of_sexp key_of_sexp = Map.t_of_sexp key_of_sexp Fold_sexpable.t_of_sexp
    let sexp_of_t sexp_of_key = Map.sexp_of_t sexp_of_key Fold_sexpable.sexp_of_t
  end
