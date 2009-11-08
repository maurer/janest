(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core.Core_hashtbl"

open Sexplib
open Sexplib.Conv
open Core_hashtbl_intf
module List = Core_list
module type HashedType = HashedType
module type S = S

module H = MoreLabels.Hashtbl

type ('a, 'b) t = ('a, 'b) H.t

let shadow_add = H.add  
let clear = H.clear
let copy = H.copy
let create = H.create
let shadow_find = H.find_all
let fold = H.fold
let iter = H.iter
let length = H.length
let mem = H.mem
let remove = H.remove
let replace = H.replace

let hash = H.hash
let hash_param = H.hash_param

(* We define a functor, Extend, that takes a hashtable and adds our
   additional operations.  We need this functor because we need to extend both
   the standard hashtable (MoreLabels.Hashtbl) and specialized hashtables
   created by Hashtbl.Make.
*)
module Extend (T :
  sig
    include Types

    val add : ('a, 'b) t -> key:('a key) -> data:'b -> unit
    val replace : ('a, 'b) t -> key:('a key) -> data:'b -> unit
    val create : int -> ('a, 'b) t
    val find : ('a, 'b) t -> 'a key -> 'b
    val find_all : ('a, 'b) t -> 'a key -> 'b list
    val fold :
      f:(key:('a key) -> data:'b -> 'c -> 'c) ->
      ('a, 'b) t -> init:'c -> 'c
    val iter : f:(key:('a key) -> data:'b -> unit) -> ('a, 'b) t -> unit
    val length : ('a, 'b) t -> int
    val mem : ('a, 'b) t -> 'a key -> bool
    val remove : ('a, 'b) t -> 'a key -> unit
  end) = struct

  module T = T

  let shadow_add = T.add
  let shadow_find = T.find_all

  let find_exn t id = T.find t id

  let iter t ~f = T.iter ~f t

  let fold t ~init ~f = T.fold ~f t ~init

  let mapi t ~f =
    let bindings =
      T.fold t ~init:[] ~f:(fun ~key ~data bindings -> (key, f ~key ~data) :: bindings)
    in
    let new_t = T.create 1 in
    List.iter bindings ~f:(fun (key,data) -> shadow_add new_t ~key ~data);
    new_t

  let map t ~f = mapi t ~f:(fun ~key:_ ~data -> f data)

  let filter_mapi t ~f =
    let bindings =
      T.fold t ~init:[] ~f:(fun ~key ~data bindings -> match f ~key ~data with
        | Some new_data -> (key,new_data) :: bindings
        | None -> bindings)
    in
    let new_t = T.create 1 in
    List.iter bindings ~f:(fun (key,data) -> shadow_add new_t ~key ~data);
    new_t

  let filter_map t ~f = filter_mapi t ~f:(fun ~key:_ ~data -> f data)

  let find t id =
    try
      let res = T.find t id in
      Some res
    with Not_found -> None

  let remove_all t key =
    for i = 1 to List.length (T.find_all t key) do
      T.remove t key
    done

  let find_default t id ~default =
    match find t id with
    | Some x -> x
    | None ->
        let default = default () in
        T.replace t ~key:id ~data:default;
        default

  let iter_vals t ~f = T.iter t ~f:(fun ~key:_ ~data -> f data)

  let of_alist lst =
    let t = T.create (List.length lst) in
    let res = ref (`Ok t) in
    List.iter lst ~f:(fun (k, v) ->
      match T.mem t k with
      | true -> res := `Duplicate_key k
      | false -> T.replace t ~key:k ~data:v
    );
    !res

  let of_alist_exn lst =
    match of_alist lst with
    | `Ok v -> v
    | `Duplicate_key _k -> failwith "Hashtbl.of_alist_exn: duplicate key"

  let of_alist_shadow lst =
    let t = T.create (List.length lst) in
    let default () = [] in
    List.iter lst ~f:(fun (key, datum) ->
      let existing_data = find_default t key ~default in
      let data = datum :: existing_data in
      T.replace t ~key ~data);
    t

  let to_alist htbl =
    T.fold ~f:(fun ~key ~data list -> (key, data)::list) ~init:[] htbl

  
  let to_alist_shadow htbl =
    let visited = T.create (T.length htbl) in
    T.fold ~f:(fun ~key ~data:_ acc ->
      if T.mem visited key then acc
      else (
        shadow_add visited ~key ~data:();
        (key, T.find_all htbl key) :: acc)) ~init:[] htbl

  let keys t =
    let visited = T.create (T.length t) in
    T.fold t ~init:[] ~f:(fun ~key ~data:_ acc -> begin
      if T.mem visited key then acc
      else begin
        T.replace visited ~key ~data:();
        key :: acc
      end
    end)
      
  let data htbl =
    T.fold ~f:(fun ~key:_ ~data list -> data::list) ~init:[] htbl

  let add_to_groups groups ~get_key ~get_data ~combine ~rows =
    List.iter rows ~f:(fun row ->
      let key = get_key row in
      let data = get_data row in
      let data =
        match find groups key with
        | None -> data
        | Some old -> combine old data
      in
      T.replace groups ~key ~data)
  ;;

  let group ?(size=1000) ~get_key ~get_data ~combine rows =
    let res = T.create size in
    add_to_groups res ~get_key ~get_data ~combine ~rows;
    res
  ;;

  let create_mapped ~get_key ~get_data rows =
    let res = T.create (List.length rows) in
    List.iter rows ~f:(fun r ->
      let key = get_key r in
      let data = get_data r in
      
      shadow_add res ~key ~data);
    res
  ;;

  let create_with_key ~get_key rows =
    create_mapped ~get_key ~get_data:(fun x -> x) rows
  ;;

  let merge ~f t1 t2 =
    let bound = max (T.length t1) (T.length t2) in
    let t = T.create bound in
    let unique_keys = T.create bound in
    let record_key ~key ~data:_ = T.replace unique_keys ~key ~data:() in
    iter t1 ~f:record_key;
    iter t2 ~f:record_key;
    iter unique_keys ~f:(fun ~key ~data:_ ->
      match f ~key (find t1 key) (find t2 key) with
      | Some data -> shadow_add t ~key ~data
      | None -> ());
    t

  let filteri_inplace t ~f =
    let to_remove =
      T.fold t ~init:[] ~f:(fun ~key ~data ac ->
        if f key data then ac else key :: ac)
    in
    List.iter to_remove ~f:(fun key -> T.remove t key);
  ;;

  let filter_inplace t ~f =
    filteri_inplace t ~f:(fun _ data -> f data)
  ;;

  let sexp_of_t sexp_of_k sexp_of_d t =
    let coll ~key:k ~data:v acc = Sexp.List [sexp_of_k k; sexp_of_d v] :: acc in
    Sexp.List (T.fold ~f:coll t ~init:[])

  
  
  let t_of_sexp k_of_sexp d_of_sexp sexp =
    match sexp with
    | Sexp.List sexps ->
        let t = T.create 0 in
        List.iter sexps ~f:(function
            (* Bindings in the S-expression list constituting
               the hashtable have the following semantics: bindings
               later in the list shadow bindings earlier in the list.
               Thus inserting them in the order of appearance is the
               correct behavior.  The [sexp_of_t] function writes out
               the S-expressions in the appropriate order (shadowed
               bindings first). *)
          | Sexp.List [k_sexp; v_sexp] ->
              shadow_add t ~key:(k_of_sexp k_sexp) ~data:(d_of_sexp v_sexp)
          | Sexp.List _ | Sexp.Atom _ ->
              Conv.of_sexp_error "Hashtbl.t_of_sexp: tuple list needed" sexp);
        t
    | Sexp.Atom _ ->
        Conv.of_sexp_error
          "Hashtbl.t_of_sexp: found atom where list was expected" sexp

  
  exception Not_equal
  let equal t t' equal =
    let half t t' =
      T.iter t ~f:(fun ~key ~data ->
        match find t' key with
        | None -> raise Not_equal
        | Some data' -> if not (equal data data') then raise Not_equal)
    in
    try
      half t t';
      half t' t;
      true
    with Not_equal -> false
  ;;
end

include Extend (struct
  include H

  type 'a key = 'a
end)

type ('a, 'b) sexpable = ('a, 'b) t

module Make (Key : HashedType) = struct
  module M = H.Make (Key)
  include M

  include Extend (struct
    type ('a, 'b) t = 'b M.t
    type 'a key = Key.t
    let create = M.create
    let find = M.find
    let find_all = M.find_all
    let add = M.add
    let replace = M.replace
    let fold = M.fold
    let iter = M.iter
    let length = M.length
    let mem = M.mem
    let remove = M.remove
  end)
  type 'a sexpable = 'a t
  let sexp_of_t f x = sexp_of_t Key.sexp_of_t f x
  let t_of_sexp f x = t_of_sexp Key.t_of_sexp f x
end

module Make_binable (Key : sig
  include HashedType
  include Binable.S with type binable = t
end) = struct
  include Make (Key)

  type 'a dummy = 'a t

  module Make_iterable_binable1_spec = struct
    type 'a t = 'a dummy
    type 'a el = Key.t * 'a with bin_io
    type 'a acc = 'a el list
    let module_name = Some "Core.Core_hashtbl"
    let length = length
    let iter ~f t = iter ~f:(fun ~key ~data -> f (key, data)) t
    let init _n = []
    let insert acc (key, data) _i = (key, data) :: acc
    let finish acc =
      let t = create (List.length acc) in
      let rec loop = function
        | [] -> t
        | (key, data) :: rest -> shadow_add t ~key ~data; loop rest
      in
      loop acc
  end

  include Bin_prot.Utils.Make_iterable_binable1 (Make_iterable_binable1_spec)
end
