(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)

TYPE_CONV_PATH "Core.Core_map"

open Sexplib
open Core_map_intf

module List = Core_list

module type Key = Key
module type S = S


type ('k, +'v) tree =
  | Empty
  | Node of ('k, 'v) tree * 'k * 'v * ('k, 'v) tree * int

module Raw_impl
  (Key : sig
    (* The type [t] is unary because we need to use it both for unary and
     * nullary key types.  See the two calls to [Raw_impl] below.
     *)
    type 'a t
    val compare : 'a t -> 'a t -> int
  end) = struct

  module T = struct
    type 'k key = 'k Key.t
    type ('k, +'v) t = ('k Key.t, 'v) tree
  end

  let height = function
      Empty -> 0
    | Node(_,_,_,_,h) -> h

  let create l x d r =
    let hl = height l and hr = height r in
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

  let singleton key data = create Empty key data Empty;;

  let bal l x d r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Map.bal"
      | Node(ll, lv, ld, lr, _) ->
          if height ll >= height lr then
            create ll lv ld (create lr x d r)
          else begin
            match lr with
              Empty -> invalid_arg "Map.bal"
            | Node(lrl, lrv, lrd, lrr, _)->
                create (create ll lv ld lrl) lrv lrd (create lrr x d r)
          end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Map.bal"
      | Node(rl, rv, rd, rr, _) ->
          if height rr >= height rl then
            create (create l x d rl) rv rd rr
          else begin
            match rl with
              Empty -> invalid_arg "Map.bal"
            | Node(rll, rlv, rld, rlr, _) ->
                create (create l x d rll) rlv rld (create rlr rv rd rr)
          end
    end else
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec add ~key:x ~data = function
      Empty ->
        Node(Empty, x, data, Empty, 1)
    | Node(l, v, d, r, h) ->
        let c = Key.compare x v in
        if c = 0 then
          Node(l, x, data, r, h)
        else if c < 0 then
          bal (add ~key:x ~data l) v d r
        else
          bal l v d (add ~key:x ~data r)




  (* let rec fold_val ~key:x ~data ~f = function *)
  (*     Empty -> *)
  (*       Node(Empty, x,f None data, Empty, 1) *)
  (*   | Node(l, v, d, r, h) -> *)
  (*       let c = Key.compare x v in *)
  (*       if c = 0 then *)
  (*         Node(l, x,f (Some d) data, r, h) *)
  (*       else if c < 0 then *)
  (*         bal (fold_val ~f ~key:x ~data l) v d r *)
  (*       else *)
  (*         bal l v d (fold_val ~f ~key:x ~data r) *)

  let rec find t x =
    match t with
    | Empty ->
        None
    | Node(l, v, d, r, _) ->
        let c = Key.compare x v in
        if c = 0 then Some d
        else find (if c < 0 then l else r) x

  let rec find_exn t x =
    match find t x with
    | None ->
        raise Not_found
    | Some data -> data

  
  let rec mem t x =
    match t with
    | Empty ->
        false
    | Node(l, v, _, r, _) ->
        let c = Key.compare x v in
        c = 0 || mem (if c < 0 then l else r) x

  let rec min_elt = function
    | Empty -> None
    | Node (Empty, k, d, _, _) -> Some (k, d)
    | Node (l, _, _, _, _) -> min_elt l
  ;;

  let rec min_elt_exn t =
    match min_elt t with
    
    | None -> raise Not_found
    | Some v -> v
  ;;

  let rec max_elt = function
    | Empty -> None
    | Node (_, k, d, Empty, _) -> Some (k, d)
    | Node (_, _, _, r, _) -> max_elt r
  ;;
  let rec max_elt_exn t =
    match max_elt t with
    
    | None -> raise Not_found
    | Some v -> v
  ;;
            
  let rec remove_min_elt t =
    match t with
      Empty -> invalid_arg "Map.remove_min_elt"
    | Node(Empty, _, _, r, _) -> r
    | Node(l, x, d, r, _) -> bal (remove_min_elt l) x d r

  
  let merge t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
        let (x, d) = min_elt_exn t2 in
        bal t1 x d (remove_min_elt t2)

  let rec remove t x =
    match t with
    | Empty ->
        Empty
    | Node(l, v, d, r, _) ->
        let c = Key.compare x v in
        if c = 0 then
          merge l r
        else if c < 0 then
          bal (remove l x) v d r
        else
          bal l v d (remove r x)

  let rec iter ~f = function
      Empty -> ()
    | Node(l, v, d, r, _) ->
        iter ~f l; f ~key:v ~data:d; iter ~f r

  let rec map ~f = function
      Empty               -> Empty
    | Node(l, v, d, r, h) ->
        let l' = map ~f l in
        let d' = f d in
        let r' = map ~f r in
        Node(l', v, d', r', h)

  let rec mapi ~f = function
      Empty               -> Empty
    | Node(l, v, d, r, h) ->
        let l' = mapi ~f l in
        let d' = f ~key:v ~data:d in
        let r' = mapi ~f r in
        Node(l', v, d', r', h)

  let rec fold ~f t ~init:accu =
    match t with
      Empty -> accu
    | Node(l, v, d, r, _) ->
        fold ~f r ~init:(f ~key:v ~data:d (fold ~f l ~init:accu))

  let rec rev_fold ~f t ~init:accu =
    match t with
      Empty -> accu
    | Node(l, v, d, r, _) ->
        rev_fold ~f l ~init:(f ~key:v ~data:d (rev_fold ~f r ~init:accu))

  let filter ~f t =
    fold ~init:Empty t ~f:(fun ~key ~data accu ->
      if f ~key ~data then add ~key ~data accu else accu
    )
  ;;

  let filter_map ~f t =
    fold ~init:Empty t ~f:(fun ~key ~data accu ->
      match f data with
      | None -> accu
      | Some b -> add ~key ~data:b accu
    )
  ;;

  let filter_mapi ~f t =
    fold ~init:Empty t ~f:(fun ~key ~data accu ->
      match f ~key ~data with
      | None -> accu
      | Some b -> add ~key ~data:b accu
    )
  ;;

  module Enum = struct
    type ('k, 'v) t =
      | End
      | More of 'k Key.t * 'v * ('k Key.t, 'v) tree * ('k, 'v) t

    let rec cons t e =
      match t with
        Empty -> e
      | Node(l, v, d, r, _) -> cons l (More(v, d, r, e))
  end

  
  let equal cmp t1 t2 =
    let rec equal_aux e1 e2 =
      match (e1, e2) with
        (Enum.End, Enum.End) -> true
      | (Enum.End, _)  -> false
      | (_, Enum.End) -> false
      | (Enum.More(v1, d1, r1, e1), Enum.More(v2, d2, r2, e2)) ->
          Key.compare v1 v2 = 0 && cmp d1 d2 &&
      equal_aux (Enum.cons r1 e1) (Enum.cons r2 e2)
    in equal_aux (Enum.cons t1 Enum.End) (Enum.cons t2 Enum.End)

  let compare cmp t1 t2 =
    
    let rec compare_aux e1 e2 =
      match (e1, e2) with
        (Enum.End, Enum.End) -> 0
      | (Enum.End, _)  -> -1
      | (_, Enum.End) -> 1
      | (Enum.More(v1, d1, r1, e1), Enum.More(v2, d2, r2, e2)) ->
          let c = Key.compare v1 v2 in
          if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
              compare_aux (Enum.cons r1 e1) (Enum.cons r2 e2)
    in compare_aux (Enum.cons t1 Enum.End) (Enum.cons t2 Enum.End)

  let rec cardinal = function
    | Empty -> 0
    | Node (l, _, _, r, _) -> cardinal l + cardinal r + 1

  let combine_alist alist ~init ~f =
    List.fold_left alist ~init:empty
      ~f:(fun accum (key, data) ->
        let prev_data =
          match find accum key with
          | None -> init
          | Some prev -> prev
        in
        let data = f data prev_data in
        add accum ~key ~data
      )

  let keys t = rev_fold ~f:(fun ~key ~data:_ list -> key::list) t ~init:[]
  let has_key = mem
  let data t = rev_fold ~f:(fun ~key:_ ~data list -> data::list) t ~init:[]

  
  module Z = struct
    exception Local_break

    let of_alist alist =
      
      let bad_key = ref None in
      try
        `Ok (List.fold_left alist ~init:empty ~f:(fun t (key,data) ->
          if mem t key then (bad_key := Some key; raise Local_break)
          else add ~key ~data t))
      with
        Local_break ->
          match !bad_key with None -> assert false | Some x -> `Duplicate_key x
    ;;

    let for_all ~f t =
      try
        iter t ~f:(fun ~key:_ ~data -> if not (f data) then raise Local_break);
        true
      with Local_break -> false

    let exists ~f t =
      try
        iter t ~f:(fun ~key:_ ~data -> if f data then raise Local_break);
        false
      with Local_break -> true
  end
  let of_alist = Z.of_alist
  let for_all  = Z.for_all
  let exists   = Z.exists
  
  let of_alist_exn alist =
    match of_alist alist with
    | `Ok x -> x
    | `Duplicate_key _ -> failwith "Map.of_alist_exn: duplicate key"
  ;;

  let of_alist_multi alist =
    combine_alist alist ~init:[] ~f:Core_list.cons
  ;;

  let to_alist t =
    rev_fold t ~init:[] ~f:(fun ~key ~data x -> (key,data)::x)
  ;;
  
  
  let merge ~f t1 t2 =
    let all_keys =
      Core_list.dedup ~compare:Key.compare (Core_list.append (keys t1) (keys t2))
    in
    List.fold_left ~init:empty all_keys
      ~f:(fun t key ->
            match f ~key (find t1 key) (find t2 key) with
            | None -> t
            | Some data -> add ~key ~data t)

  
  let t_of_sexp key_of_sexp value_of_sexp = function
    | Type.List lst ->
        let coll t = function
          | Type.List [k_sexp; v_sexp] ->
              let key = key_of_sexp k_sexp in
              let value = value_of_sexp v_sexp in
              if mem t key then Conv.of_sexp_error "Map.t_of_sexp: duplicate key" k_sexp
              else add ~key ~data:value t
          | sexp -> Conv.of_sexp_error "Map.t_of_sexp: tuple list needed" sexp
        in
        List.fold_left ~f:coll ~init:empty lst
    | sexp ->
        Conv.of_sexp_error "Map.t_of_sexp: list needed" sexp

  let sexp_of_t sexp_of_key sexp_of_value t =
    let f ~key ~data acc = Type.List [sexp_of_key key; sexp_of_value data] :: acc in
    Type.List (rev_fold ~f t ~init:[])


end

module Make (Key : Key) = struct
  include Raw_impl (struct
    type 'a t = Key.t
    let compare = Key.compare
  end)

  type key = Key.t
  type +'v t = (Key.t, 'v) tree

  type +'a sexpable = 'a t
  let t_of_sexp a_of_sexp sexp = t_of_sexp Key.t_of_sexp a_of_sexp sexp
  let sexp_of_t sexp_of_a t = sexp_of_t Key.sexp_of_t sexp_of_a t
end

module Key = struct
  type 'a t = 'a
  let compare = Pervasives.compare
end

include Raw_impl (Key)

type ('a, +'b) t = ('a, 'b) tree
type ('a, +'b) sexpable = ('a, 'b) t


include Bin_prot.Utils.Make_iterable_binable2 (struct
  type ('a, 'b) t = ('a, 'b) tree
  type ('a, 'b) el = 'a * 'b with bin_io
  type ('a, 'b) acc = ('a , 'b) t
  let module_name = Some "Core.Core_map"
  let length = cardinal
  let iter ~f t = iter ~f:(fun ~key ~data -> f (key, data)) t
  let init _n = empty

  let insert acc (key, data) _i =
    if mem acc key then failwith "Map.bin_read_t_: duplicate element in map"
    else add ~key ~data acc

  let finish t = t
end)

module Make_binable (Key : sig
  type t
  include Binable.S with type binable = t
end) = struct
  type +'v dummy = (Key.t, 'v) t with bin_io
  type +'v t = 'v dummy with bin_io
  type +'v binable = 'v t
end
