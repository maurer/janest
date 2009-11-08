(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)

(* A set of integers is represented as a compressed complete D-ary tree, where
   the height of the tree is [ceil (log_D(N))], where N is the largest integer in
   the set.  Each leaf corresponds to one integer that could be in the set.
   The compression strategy is to represent maximal subtrees in which
   all leaves have the same value (i.e. they are all in or all not in) as a
   single node, and expand the node on demand when the subtree changes.
*)

   

TYPE_CONV_PATH "Int_set"

open Core.Std

module Array = struct
  include Array
  let fold t ~init ~f = fold_left t ~init ~f
end
module Exn = struct
  let to_string = Exn.to_string
end
let failwithf f = Printf.ksprintf (fun s () -> failwith s) f
let invalid_argf f = Printf.ksprintf (fun s () -> invalid_arg s) f
  
let check_invariant = false

let debug = false

let debug_print f = Printf.ksprintf (fun msg () -> eprintf "%s\n%!" msg) f

let complete_length ~log2_degree ~height =
  assert (height >= 0);
  assert (log2_degree >= 1);
  1 lsl (height * log2_degree);
;;

module Tree = struct
  module T = struct
    type t =
    | Empty
    | Full
    | Node of node
    and node = {
      mutable num_full : int;
      children : t array;
    } with sexp
  end
  include T

  let is_full = function
    | Full -> true
    | Empty | Node _ -> false
  ;;

  let invariant t height =
    let rec loop t height =
      assert (height >= 0);
      match t with
      | Empty | Full -> ()
      | Node node ->
          assert (height > 0);
          (* All the children can't all be full, or we would have been full. *)
          assert (node.num_full < Array.length node.children);
          assert (node.num_full
                     = (Array.fold node.children ~init:0
                           ~f:(fun ac t -> if is_full t then ac + 1 else ac)));
          let height = height - 1 in
          Array.iter node.children ~f:(fun t -> loop t height);
    in
    loop t height
  ;;

  let rec height t =
    match t with
    | Empty | Full -> 0
    | Node node ->
        1 + (Array.fold node.children ~init:0 ~f:(fun ac t -> max ac (height t)))
  ;;

  let rec length t ~log2_degree ~height =
    let rec loop t height =
      match t with
      | Empty -> 0
      | Full -> complete_length ~log2_degree ~height
      | Node node ->
          let height = height - 1 in
          Array.fold node.children ~init:0 ~f:(fun ac t -> ac + loop t height)
    in
    loop t height
  ;;
end

open Tree.T

type t = {
  log2_degree : int;
  degree : int;
  mask : int;
  mutable height : int;
  mutable max_for_height : int;
  mutable min_element : int;
  mutable max_element : int;
  mutable tree : Tree.t;
  mutable length : int;
} with sexp

type sexpable = t

let to_string t = Sexp.to_string_hum (sexp_of_t t)

let complete_length t ~height =
  complete_length ~log2_degree:t.log2_degree ~height
;;
  
let invariant t =
  assert (t.log2_degree >= 1);
  assert (t.degree = 1 lsl t.log2_degree);
  assert (t.mask = t.degree - 1); 
  assert (t.height >= 0);
  assert (Tree.height t.tree <= t.height);
  let complete_length = complete_length t ~height:t.height in
  assert (t.max_for_height = complete_length - 1);
  assert (0 <= t.length && t.length <= complete_length);
  assert (t.length = Tree.length t.tree ~log2_degree:t.log2_degree ~height:t.height);
  assert (
    t.length = 0
      || (0 <= t.min_element
           && t.min_element <= t.max_element
           && t.max_element <= t.max_for_height));
  
  begin match t.tree with
  | Empty -> assert (t.height = 0);
  | Full | Node _ -> ()
  end;
  Tree.invariant t.tree t.height;
;;

let invariant t =
  try invariant t
  with exn ->
    failwithf "invariant failed: %s\n%s" (Exn.to_string exn) (to_string t) ()
;;

let length t = t.length

let is_empty t = length t = 0

let min_element t = if is_empty t then None else Some t.min_element

let max_element t = if is_empty t then None else Some t.max_element

let create_node t = {
  num_full = 0;
  children = Array.create t.degree Empty;
}

(* An integer uniquely determines a path in the tree, where each log2_degree
   bits specifies an edge.
   [offset t i ~height] returns the edge to follow (i.e. its index) to get to
   [i] when at the subtree of height [height].
*)
let offset t i ~height = t.mask land (i lsr ((height - 1) * t.log2_degree))

let create ?(log2_degree = 4) () =
  if log2_degree < 1 then
    invalid_argf
      "Int_set.create ~log2_degree:%d error: log2_degree must be >= 1"
      log2_degree ();
  let degree = 1 lsl log2_degree in
  let t = {
    log2_degree = log2_degree;
    degree = degree;
    mask = degree - 1;
    height = 0;
    max_for_height = 0; 
    max_element = -1;
    min_element = -1;
    tree = Empty;
    length = 0;
  } in
  if debug then
    debug_print "create ~log2_degree:%d = %s" log2_degree (to_string t) ();
  if check_invariant then invariant t;
  t
;;

let mem t i =
  if check_invariant then invariant t;
  if i < 0 then invalid_argf "Int_set.mem %d: int must be nonnegative" i ();
  (i <= t.max_element)
  && begin
    let rec loop height tree =
      match tree with
      | Empty -> false
      | Full -> true
      | Node node -> loop (height - 1) (node.children.(offset t i ~height))
    in
    loop t.height t.tree
  end
;;

let ensure_can_hold t i =
  while i > t.max_for_height do
    let tree = t.tree in
    begin match tree with
    | Empty -> ()
    | Full | Node _ ->
        let node = create_node t in
        node.children.(0) <- tree;
        begin match tree with
        | Empty -> assert false
        | Node _ -> ()
        | Full -> node.num_full <- 1
        end;
        t.tree <- Node node;
    end;
    t.height <- t.height + 1;
    t.max_for_height <- complete_length t ~height:t.height - 1;
  done;
;;

let incr_num_full node = node.num_full <- node.num_full + 1

let fill_children t node height ~lo_offset ~hi_offset =
  if debug then
    debug_print "fill_children ~height:%d ~lo_offset:%d ~hi_offset:%d\n%s\n%s"
      height lo_offset hi_offset
      (to_string t) (Sexp.to_string_hum (Tree.sexp_of_node node)) ();
  let height = height - 1 in
  let complete_subtree_length = complete_length t ~height in
  let children = node.children in
  for i = lo_offset to hi_offset; do
    match children.(i) with
    | Full -> ()
    | Empty | Node _ ->
        t.length <-
          t.length + (complete_subtree_length
                       - (Tree.length children.(i)
                             ~log2_degree:t.log2_degree ~height));
        children.(i) <- Full;
        incr_num_full node;
  done;
;;

let rec add_range_to_node t node height ~lo ~hi =
  if debug then
    debug_print "add_range_to_node ~height:%d ~lo:%d ~hi:%d\n%s"
      height lo hi (Sexp.to_string_hum (Tree.sexp_of_node node)) ();
  assert (height >= 1);
  if hi - lo + 1 = complete_length t ~height:height then
    fill_children t node height ~lo_offset:0 ~hi_offset:(t.degree - 1)
  else begin
    let lo_offset = offset t lo ~height in
    let hi_offset = offset t hi ~height in
    if height = 1 then
      fill_children t node height ~lo_offset ~hi_offset
    else if lo_offset = hi_offset then
      add_range_to_child t node height ~offset:lo_offset ~lo ~hi
    else begin
      let mask = complete_length t ~height:(height - 1) - 1 in
      add_range_to_child t node height ~offset:lo_offset ~lo ~hi:(lo lor mask);
      fill_children t node height
        ~lo_offset:(lo_offset + 1) ~hi_offset:(hi_offset - 1);
      add_range_to_child t node height
        ~offset:hi_offset ~lo:(hi land lnot mask) ~hi;
    end
  end
and add_range_to_child t node height ~offset ~lo ~hi =
  if debug then
    debug_print "add_range_to_child height:%d ~offset:%d ~lo:%d ~hi:%d\n%s"
      height offset lo hi
      (Sexp.to_string_hum (Tree.sexp_of_node node)) ();
  assert (height >= 1);
  let children = node.children in
  let descend_to node' =
    add_range_to_node t node' (height - 1) ~lo ~hi;
    if node'.num_full = t.degree then begin
      incr_num_full node;
      children.(offset) <- Full;
    end
  in
  match children.(offset) with
  | Full -> ()
  | Node node' -> descend_to node'
  | Empty ->
      if height = 1 then begin
        assert (hi = lo);
        t.length <- t.length + 1;
        incr_num_full node;
        children.(offset) <- Full;
      end else begin
        let node' = create_node t in
        children.(offset) <- Node node';
        descend_to node';
      end
;;

let add_range t ~lo ~hi =
  if debug then debug_print "add ~lo:%d ~hi:%d\n%s" lo hi (to_string t) ();
  if lo < 0 || lo > hi then
    invalid_argf "Int_set.add_range ~lo:%d ~hi:%d error: lo must be nonnegative \
                  and lo must be less than hi." lo hi ();
  ensure_can_hold t hi;
  begin match t.tree with
  | Full -> ()
  | Empty ->
      t.min_element <- lo;
      t.max_element <- hi;
      if lo = 0 && hi = t.max_for_height then begin
        t.length <- hi - lo + 1;
        t.tree <- Full
      end else begin
        let node = create_node t in
        t.tree <- Node node;
        add_range_to_node t node t.height ~lo ~hi;
      end
  | Node node ->
      if lo < t.min_element then t.min_element <- lo;
      if hi > t.max_element then t.max_element <- hi;
      add_range_to_node t node t.height ~lo ~hi;
      if node.num_full = t.degree then t.tree <- Full;
  end;
  if debug then
    debug_print "add ~lo:%d ~hi:%d yielded %s" lo hi (to_string t) ();
  if check_invariant then invariant t;
;;
  
let add t i = add_range t ~lo:i ~hi:i
