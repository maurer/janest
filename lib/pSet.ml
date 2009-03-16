(* CR sweeks: Should this file be renamed as core_set.ml? *)
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

(* $Id: pSet.ml,v 1.2 2003/09/10 15:40:01 sandor Exp $ *)

(* Sets over ordered types *)

open StdLabels
open MoreLabels

type 'elt t = Empty | Node of 'elt t * 'elt * 'elt t * int

    (* Sets are represented by balanced binary trees (the heights of the
       children differ by at most 2 *)

let height = function
    Empty -> 0
  | Node(_, _, _, h) -> h

    (* Creates a new node with left son l, value v and right son r.
       We must have all elements of l < v < all elements of r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. *)

let create l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced and | height l - height r | <= 3.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. *)

let bal l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Set.bal"
    | Node(ll, lv, lr, _) ->
        if height ll >= height lr then
          create ll lv (create lr v r)
        else begin
          match lr with
            Empty -> invalid_arg "Set.bal"
          | Node(lrl, lrv, lrr, _)->
              create (create ll lv lrl) lrv (create lrr v r)
        end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Set.bal"
    | Node(rl, rv, rr, _) ->
        if height rr >= height rl then
          create (create l v rl) rv rr
        else begin
          match rl with
            Empty -> invalid_arg "Set.bal"
          | Node(rll, rlv, rlr, _) ->
              create (create l v rll) rlv (create rlr rv rr)
        end
  end else
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

    (* Insertion of one element *)

let rec add t x =
  match t with
  | Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) ->
      let c = Pervasives.compare x v in
      if c = 0 then t else
        if c < 0 then bal (add l x) v r else bal l v (add r x)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

let rec join l v r =
  match (l, r) with
    (Empty, _) -> add r v
  | (_, Empty) -> add l v
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
          create l v r

    (* Smallest and greatest element of a set *)

let rec min_elt = function
    Empty -> None
  | Node(Empty, v, _, _) -> Some v
  | Node(l, _, _, _) -> min_elt l

let rec min_elt_exn t =
  match min_elt t with
  | None -> raise Not_found
  | Some v -> v

let rec max_elt = function
    Empty -> None
  | Node(_, v, Empty, _) -> Some v
  | Node(_, _, r, _) -> max_elt r

let rec max_elt_exn t =
  match max_elt t with
  | None -> raise Not_found
  | Some v -> v

    (* Remove the smallest element of the given set *)

let rec remove_min_elt = function
    Empty -> invalid_arg "Set.remove_min_elt"
  | Node(Empty, _, r, _) -> r
  | Node(l, v, r, _) -> bal (remove_min_elt l) v r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2. *)

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> bal t1 (min_elt_exn t2) (remove_min_elt t2)

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
  | Empty, t | t, Empty -> t
  | (_, _) -> join t1 (min_elt_exn t2) (remove_min_elt t2)

    (* Splitting.  split x s returns a triple (l, present, r) where
       - l is the set of elements of s that are < x
       - r is the set of elements of s that are > x
       - present is false if s contains no element equal to x,
       or true if s contains an element equal to x. *)

let rec split t x =
  match t with
  | Empty ->
      (Empty, false, Empty)
  | Node(l, v, r, _) ->
      let c = Pervasives.compare x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split l x in (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = split r x in (join l v lr, pres, rr)

    (* Implementation of the set operations *)

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec mem t x =
  match t with
  | Empty -> false
  | Node(l, v, r, _) ->
      let c = Pervasives.compare x v in
      c = 0 || mem (if c < 0 then l else r) x

let singleton x = Node(Empty, x, Empty, 1)

let rec remove t x =
  match t with
    Empty -> Empty
  | Node(l, v, r, _) ->
      let c = Pervasives.compare x v in
      if c = 0 then merge l r else
        if c < 0 then bal (remove l x) v r else bal l v (remove r x)

let rec union s1 s2 =
  match (s1, s2) with
  | Empty, t | t, Empty -> t
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      if h1 >= h2 then
        if h2 = 1 then add s1 v2 else begin
          let (l2, _, r2) = split s2 v1 in
          join (union l1 l2) v1 (union r1 r2)
        end
      else
        if h1 = 1 then add s2 v1 else begin
          let (l1, _, r1) = split s1 v2 in
          join (union l1 l2) v2 (union r1 r2)
        end

let rec inter s1 s2 =
  match (s1, s2) with
  | Empty, _ | _, Empty -> Empty
  | (Node(l1, v1, r1, _), t2) ->
      match split t2 v1 with
        (l2, false, r2) ->
          concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
          join (inter l1 l2) v1 (inter r1 r2)

let rec diff s1 s2 =
  match (s1, s2) with
    (Empty, _) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
      match split t2 v1 with
        (l2, false, r2) ->
          join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
          concat (diff l1 l2) (diff r1 r2)

type 'elt enumeration = End | More of 'elt * 'elt t * 'elt enumeration

let rec cons_enum s e =
  match s with
    Empty -> e
  | Node(l, v, r, _) -> cons_enum l (More(v, r, e))

let rec compare_aux e1 e2 =
  match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
      let c = Pervasives.compare v1 v2 in
      if c <> 0
      then c
      else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

let compare s1 s2 =
  compare_aux (cons_enum s1 End) (cons_enum s2 End)

let equal s1 s2 =
  compare s1 s2 = 0

let rec subset s1 s2 =
  match (s1, s2) with
    Empty, _ ->
      true
  | _, Empty ->
      false
  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
      let c = Pervasives.compare v1 v2 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
      else
        subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2

let rec iter ~f = function
    Empty -> ()
  | Node(l, v, r, _) -> iter ~f l; f v; iter ~f r

let rec fold ~f s ~init:accu =
  match s with
    Empty -> accu
  | Node(l, v, r, _) -> fold ~f r ~init:(f v (fold ~f l ~init:accu))

let rec for_all ~f:p = function
    Empty -> true
  | Node(l, v, r, _) -> p v && for_all ~f:p l && for_all ~f:p r

let rec exists ~f:p = function
    Empty -> false
  | Node(l, v, r, _) -> p v || exists ~f:p l || exists ~f:p r

let filter ~f:p s =
  let rec filt accu = function
    | Empty -> accu
    | Node(l, v, r, _) ->
        filt (filt (if p v then add accu v else accu) l) r in
  filt Empty s

let filter_map ~f:p s =
  let rec filt accu = function
    | Empty -> accu
    | Node(l, v, r, _) ->
        filt (filt (match p v with
		      None -> accu 
		    | Some v -> add accu v) l) r 
  in
  filt Empty s

let filter_opt t =
  filter_map ~f:(fun x -> x) t
;;

let partition ~f:p s =
  let rec part (t, f as accu) = function
    | Empty -> accu
    | Node(l, v, r, _) ->
        part (part (if p v then (add t v, f) else (t, add f v)) l) r in
  part (Empty, Empty) s

let rec cardinal = function
    Empty -> 0
  | Node(l, _, r, _) -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt
let choose_exn = min_elt_exn

let of_list lst = List.fold_left ~f:add ~init:empty lst
let to_list s = elements s

let of_array ar = Array.fold_left ~f:add ~init:empty ar
let to_array s = Array.of_list (elements s)

(* The following implementation of to_array is faster, and as far as
   we know it's correct, but we don't want to code review it yet.
let to_array = function
  | Empty -> [||]
  | Node (l, v, r, _) ->
      let n = cardinal l + cardinal r + 1 in
      let res = Array.create n v in
      let pos_ref = ref 0 in
      let rec loop = function
        | Empty -> ()
        | Node (l, v, r, _) ->
            loop l;
            res.(!pos_ref) <- v;
            incr pos_ref;
            loop r
      in
      loop l;
      incr pos_ref;
      loop r;
      res
*)

let map ~f t =
  fold t ~init:empty ~f:(fun x t -> add t (f x))

let rec subsets s n =
  let size = cardinal s in
  if size < n || n < 0 then empty
  else if n = 0 then singleton empty
  else if size = n then singleton s
  else
    let elt = choose_exn s in
    let remain = remove s elt in
    let subsets_without_elt = subsets remain n in
    let subsets_with_elt =
      let temp = subsets remain (n-1) in
      map temp ~f:(fun t ->add t elt)
    in
    union subsets_without_elt subsets_with_elt

let group_by set ~equiv = 
  let rec loop set equiv_classes =
    if is_empty set
    then equiv_classes
    else 
      let x = choose_exn set in
      let equiv_x, not_equiv_x = partition ~f:(equiv x) set in
      loop not_equiv_x (equiv_x :: equiv_classes)
  in
  loop set []

open Sexplib
      
let t_of_sexp el_of_sexp = function
  | Type.List lst ->
      let coll set el_sexp = 
        let el = el_of_sexp el_sexp in
        if mem set el then 
          Conv.of_sexp_error "Set.t_of_sexp: duplicate element in set" el_sexp
        else add set el
      in
      List.fold_left ~f:coll ~init:empty lst
  | sexp -> Conv.of_sexp_error "Set.t_of_sexp: list needed" sexp

let sexp_of_t sexp_of_el set =
  Type.List (fold ~f:(fun el acc -> sexp_of_el el :: acc) set ~init:[])

