module List = StdLabels.List
open Sexplib
open Sexplib.Conv

(* CR sweeks: just write the signature twice.  This Mono_or_poly stuff is
   confusing to users and to OCaml (it causes bad type error messages and
   missed chances for generalization). *)

(* todo: add most or all PSet functions *)
module type Mono_or_poly = sig
  type 'a elt_
  type 'a set

  val create : int -> 'a set

  val add : 'a set -> 'a elt_ -> unit
  val strict_add : 'a set -> 'a elt_ -> unit (* fails if element is already there *)

  val remove : 'a set -> 'a elt_ -> unit
  val strict_remove : 'a set -> 'a elt_ -> unit (* fails if element wasn't there *)

  val clear : 'a set -> unit
  val fold : f:('a -> 'b elt_ -> 'a) -> init:'a -> 'b set -> 'a
  val iter : f:('a elt_ -> unit) -> 'a set -> unit
  val length : 'a set -> int
  val mem : 'a set -> 'a elt_ -> bool
  val of_list : 'a elt_ list -> 'a set
  val to_list : 'a set -> 'a elt_ list
end

module type Mono = sig
  type elt
  include Mono_or_poly with type 'a elt_ = elt
  type t = unit set
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module Make_mono_or_poly (
  Hashtbl : sig (* CRv2 / CF: should be in Core_hashtbl.mli *)
    type 'a key
    type ('a, 'b) t
    val add : ('a, 'b) t -> key:('a key) -> data:'b -> unit
    val clear : ('a, 'b) t -> unit
    val create : int -> ('a, 'b) t
    val fold : f:(key:('a key) -> data:'b -> 'c -> 'c) -> ('a, 'b) t -> init:'c -> 'c
    val iter : f:(key:('a key) -> data:'b -> unit) -> ('a, 'b) t -> unit
    val keys : ('a, 'b) t -> 'a key list
    val length : ('a, 'b) t -> int
    val mem : ('a, 'b) t -> 'a key -> bool
    val of_alist : ('a key * 'b) list -> ('a, 'b) t
    val remove : ('a, 'b) t -> 'a key -> unit
    val replace : ('a, 'b) t -> key:('a key) -> data:'b -> unit
  end)
      = 
struct
  open Hashtbl
  type 'a elt_ = 'a Hashtbl.key
  type 'a set = ('a, unit) Hashtbl.t
  let clear = clear
  let create = create
  let length = length
  let mem = mem

  let add t k = Hashtbl.replace t ~key:k ~data:()
  let strict_add t k = 
    if mem t k then failwith "Hash_set.strict_add" 
    else Hashtbl.add t ~key:k ~data:()

  let remove = remove
  let strict_remove t k =
    if mem t k then remove t k else failwith "Hash_set.strict_remove" 

  let fold ~f ~init t = Hashtbl.fold t ~init ~f:(fun ~key ~data:_ acc -> f acc key)
  let iter ~f t = Hashtbl.iter t ~f:(fun ~key ~data:_ -> f key)
  let to_list = Hashtbl.keys
  let of_list l = Hashtbl.of_alist (List.map l ~f:(fun k -> k, ()))

  let sexp_of_t sexp_of_k t = sexp_of_list sexp_of_k (to_list t)
  let t_of_sexp k_of_sexp sexp =
    let t = create 0 in
    let sexps =  match sexp with
      | Sexp.Atom _ -> Conv.of_sexp_error
          "Hash_set: found Atom where list was expected" sexp
      | Sexp.List l -> l
    in
    List.iter sexps ~f:(fun k_sexp -> 
      let k = k_of_sexp k_sexp in
      if mem t k then Conv.of_sexp_error "Hash_set: duplicate element found" k_sexp
      else Hashtbl.add t ~key:k ~data:()
    );
    t
  end

module Poly : sig
  include Mono_or_poly with type 'a elt_ = 'a
  type 'a t = 'a set
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
end = struct
  include Make_mono_or_poly (
    struct
      include Core_hashtbl
      type 'a key = 'a
    end
  )
  type 'a t = 'a set
end

module Make (H : sig
  include Core_hashtbl.HashedType
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end
) : Mono 
  with type elt = H.t = struct
  module Hashtbl = Core_hashtbl.Make (H)
  include Make_mono_or_poly (struct
    type 'a key = Hashtbl.key       (* CRv2 ??: should be in Core_hashtbl.mli *)
    type ('a, 'b) t = 'b Hashtbl.t
    let add = Hashtbl.add
    let clear = Hashtbl.clear
    let create = Hashtbl.create
    let fold = Hashtbl.fold
    let iter = Hashtbl.iter
    let keys = Hashtbl.keys
    let length = Hashtbl.length
    let mem = Hashtbl.mem
    let of_alist = Hashtbl.of_alist
    let remove = Hashtbl.remove
    let replace = Hashtbl.replace
  end)
  let sexp_of_t set = sexp_of_t H.sexp_of_t set
  let t_of_sexp set = t_of_sexp H.t_of_sexp set
  type t = unit set
  type elt = H.t
end
