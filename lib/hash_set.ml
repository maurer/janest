(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core.Hash_set"

module List = StdLabels.List
open Sexplib
open Sexplib.Conv
open Hash_set_intf

module type S = S
module type S_binable = S_binable
module type S1 = S1

module Make_gen (Hashtbl : Core_hashtbl_intf.Gen) = struct
  module T = struct
    type 'a elt = 'a Hashtbl.T.key
    type 'a t = ('a, unit) Hashtbl.T.t
  end
  let clear = Hashtbl.clear
  let create = Hashtbl.create
  let length = Hashtbl.length
  let mem = Hashtbl.mem
  let is_empty t = Hashtbl.length t = 0

  let add t k = Hashtbl.replace t ~key:k ~data:()

  let strict_add t k = 
    if mem t k then failwith "Hash_set.strict_add" 
    else Hashtbl.replace t ~key:k ~data:()

  let remove = Hashtbl.remove
  let strict_remove t k =
    if mem t k then remove t k else failwith "Hash_set.strict_remove" 

  let fold ~f ~init t = Hashtbl.fold t ~init ~f:(fun ~key ~data:_ acc -> f acc key)
  let iter ~f t = Hashtbl.iter t ~f:(fun ~key ~data:_ -> f key)
  let to_list = Hashtbl.keys
  let of_list l =
    let t = Hashtbl.create 1 in
    List.iter l ~f:(fun k -> add t k);
    t

  let equal t1 t2 = Hashtbl.equal t1 t2 (fun () () -> true) 

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
      else Hashtbl.replace t ~key:k ~data:()
    );
    t
  end

include Make_gen (Core_hashtbl)

type 'a t = 'a T.t

module Make (H : Core_hashtbl.HashedType) = struct
  include Make_gen (Core_hashtbl.Make (H))
    
  let sexp_of_t set = sexp_of_t H.sexp_of_t set
  let t_of_sexp set = t_of_sexp H.t_of_sexp set

  type t = unit T.t
  type elt = H.t
end

module Make_binable (H : sig
  include Core_hashtbl.HashedType
  include Binable.S with type binable = t
end) = struct
  include Make (H)

  type dummy = t

  module Make_iterable_binable_spec = struct
    type t = dummy
    type el = H.t with bin_io
    type acc = t
    let module_name = Some "Core.Hash_set"
    let length = length
    let iter = iter
    let init = create
    let insert acc v _i = add acc v; acc
    let finish t = t
  end

  include Bin_prot.Utils.Make_iterable_binable (Make_iterable_binable_spec)
end
