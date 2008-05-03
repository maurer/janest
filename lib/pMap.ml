(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core.pMap"

(* CR sweeks: Should this file be renamed as core_map.ml? *)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pMap.ml,v 1.5 2003/09/10 15:40:01 sandor Exp $ *)

open StdLabels

type ('key,'a) t =
    Empty
  | Node of ('key,'a) t * 'key * 'a * ('key,'a) t * int
with bin_io

let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
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
      let c = Pervasives.compare x v in
      if c = 0 then
        Node(l, x, data, r, h)
      else if c < 0 then
        bal (add ~key:x ~data l) v d r
      else
        bal l v d (add ~key:x ~data r)

let rec find t x =
  match t with
  | Empty ->
      None
  | Node(l, v, d, r, _) ->
      let c = Pervasives.compare x v in
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
      let c = Pervasives.compare x v in
      c = 0 || mem (if c < 0 then l else r) x

let rec min_binding = function
    Empty -> raise Not_found
  | Node(Empty, x, d, _, _) -> x, d
  | Node(l, _, _, _, _) -> min_binding l

let rec remove_min_binding t =
  match t with
    Empty -> invalid_arg "Map.remove_min_elt"
  | Node(Empty, _, _, r, _) -> r
  | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
      let (x, d) = min_binding t2 in
      bal t1 x d (remove_min_binding t2)

let rec remove t x =
  match t with
  | Empty ->
      Empty
  | Node(l, v, d, r, _) ->
      let c = Pervasives.compare x v in
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

type ('key, 'a) enumeration =
  | End
  | More of 'key * 'a * ('key, 'a) t * ('key, 'a) enumeration

let rec cons_enum t e =
  match t with
    Empty -> e
  | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

let compare cmp t1 t2 =
  let rec compare_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
        let c = Pervasives.compare v1 v2 in
        if c <> 0 then c else
          let c = cmp d1 d2 in
          if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in compare_aux (cons_enum t1 End) (cons_enum t2 End)

let equal cmp t1 t2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
        Pervasives.compare v1 v2 = 0 && cmp d1 d2 &&
    equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in equal_aux (cons_enum t1 End) (cons_enum t2 End)

let rec cardinal = function
  | Empty -> 0
  | Node (l, _, _, r, _) -> cardinal l + cardinal r + 1

let combine_alist alist ~init ~f =
  let combine_add g (a,b) ~f =
    let prev = match find g a with None -> init | Some prev -> prev in
    let b = f b prev in
    add g ~key:a ~data:b
  in
  List.fold_left alist ~init:empty
    ~f:(fun g (a,b) -> combine_add g (a,b) ~f)

let rec rev_fold ~f t ~init:accu =
  match t with
    Empty -> accu
  | Node(l, v, d, r, _) ->
      rev_fold ~f l ~init:(f ~key:v ~data:d (rev_fold ~f r ~init:accu))

let keys t = rev_fold ~f:(fun ~key ~data:_ list -> key::list) t ~init:[]
let has_key = mem
let data t = rev_fold ~f:(fun ~key:_ ~data list -> data::list) t ~init:[]

let of_alist alist =
  let bad_key = ref None in
  try
    `Ok (List.fold_left alist ~init:empty ~f:(fun t (key,data) ->
      if mem t key then (bad_key := Some key; raise Exit)
      else add ~key ~data t))
  with
    Exit ->
      match !bad_key with None -> assert false | Some x -> `Duplicate_key x
;;

let of_alist_exn alist =
  match of_alist alist with
  | `Ok x -> x
  | `Duplicate_key _ -> failwith "PMap.of_alist_exn: duplicate key"
;;

let of_alist_multi alist =
  combine_alist alist ~init:[] ~f:Core_list.cons
;;

let to_alist t =
  rev_fold ~f:(fun ~key ~data x -> (key,data)::x) t ~init:[]

let merge ~f t1 t2 =
  let all_keys = Core_list.dedup (Core_list.append (keys t1) (keys t2)) in
  List.fold_left ~init:empty all_keys
    ~f:(fun t key ->
          match f ~key (find t1 key) (find t2 key) with
          | None -> t
          | Some data -> add ~key ~data t)

open Sexplib

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
  Type.List (fold ~f t ~init:[])

module Infix = struct
  let ( |= ) t key = find_exn t key
  let ( |?= ) t key = find t key
  let ( |< ) t (key,data) = add ~key ~data t
end

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

exception Short_circuit

let for_all ~f t =
  try iter ~f:(fun ~key:_ ~data -> if not (f data) then raise Short_circuit) t; true
  with Short_circuit -> false

let exists ~f t =
  try iter ~f:(fun ~key:_ ~data -> if (f data) then raise Short_circuit) t; false
  with Short_circuit -> true
