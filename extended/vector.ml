(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Vector"
open Core.Std

module Tree = struct
  type 'a t = Leaf of 'a array | Tree of 'a t array with sexp,bin_io
end
module T = Tree

type 'a t = {
  length      : int;
  height      : int;
  tail        : 'a array option;
  tail_offset : int;
  root        : 'a Tree.t option
} with bin_io

type 'a binable = 'a t
type 'a container = 'a t
type 'a sexpable = 'a t

let empty = 
  {
    length      = 0;
    height      = 0;
    tail        = None;
    tail_offset = 0;
    root        = None
  }
;;

let array_append arr v : 'a array =
  let length  = Array.length arr in
  let new_arr = Array.create (length + 1) arr.(0) in
  Array.blit ~src:arr ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len:length;
  new_arr.(length) <- v;
  new_arr
;;

let array_replace arr pos v : 'a array =
  let length  = Array.length arr in
  let new_arr = Array.create length arr.(0) in
  Array.blit ~src:arr ~src_pos:0 ~dst:new_arr ~dst_pos:0 ~len:length;
  new_arr.(pos) <- v;
  new_arr
;;

let out_of_bounds () = raise (Invalid_argument "index out of bounds")

let get t i =
  if i < 0 || i >= t.length then out_of_bounds ();
  if i >= t.tail_offset then begin
    match t.tail with
    | None -> assert false
    | Some arr -> arr.(i land 0x01f)
  end else begin
    let rec loop tree shift =
      assert (shift >= 0);
      match tree with
      | T.Tree arr -> 
          loop arr.((i lsr shift) land 0x01f) (shift - 5)
      | T.Leaf arr -> 
          assert (shift = 0);
          arr.(i land 0x01f)
    in
    match t.root with
    | None -> assert false
    | Some tree -> loop tree (5 * t.height)
  end
;;

let set t i v =
  if i < 0 || i >= t.length then out_of_bounds ();
  if i >= t.tail_offset then begin
    match t.tail with
    | None -> assert false
    | Some arr -> {t with tail = Some (array_replace arr (i land 0x01f) v)}
  end else begin
    let rec loop tree shift =
      assert (shift >= 0);
      match tree with
      | T.Tree arr -> 
          let index = (i lsr shift) land 0x01f in
          T.Tree (array_replace arr index (loop arr.(index) (shift - 5)))
      | T.Leaf arr -> 
          assert (shift = 0);
          T.Leaf (array_replace arr (i land 0x01f) v)
    in
    match t.root with
    | None -> assert false
    | Some tree -> {t with root = Some (loop tree (5 * t.height))}
  end
;;

let snoc t elem =
  match t.tail with
  | None -> 
      {t with 
        tail = Some [| elem |];
        length = t.length + 1
      }
  | Some tail ->
      let tail_length = Array.length tail in
      if tail_length < 31 then begin
        let new_tail = array_append tail elem in
        {t with 
          tail = Some new_tail; 
          length = t.length + 1
        }
      end else begin
        let new_tail = array_append tail elem in
        let rec extend_branch height =
          if height = 0 then T.Leaf new_tail
          else T.Tree [| extend_branch (height - 1) |]
        in
        let rec append_tail current_height tree =
          match tree with
          | T.Leaf _ -> assert false
          | T.Tree arr -> 
              let length = Array.length arr in
              match arr.(length - 1) with
              | T.Leaf _ ->
                  if length < 32 then Some (T.Tree (array_append arr (T.Leaf new_tail)))
                  else None
              | T.Tree _ as next ->
                  match append_tail (current_height - 1) next with
                  | None -> 
                      if length < 32 then
                        Some (T.Tree (array_append arr 
                          (extend_branch (current_height - 1))))
                      else None
                  | Some new_tree -> 
                      Some (T.Tree (array_replace arr (length - 1) new_tree))
        in
        match t.root with
        | None -> 
            {
              root = Some (T.Tree [| (T.Leaf new_tail) |]);
              length = t.length + 1;
              tail = None;
              tail_offset = t.tail_offset + 32;
              height = 1;
            }
        | Some tree ->
            let new_root,new_height =
              match append_tail t.height tree with
              | Some nt -> nt, t.height
              | None -> T.Tree [| tree; extend_branch t.height |], t.height + 1
            in
            {
              root = Some new_root;
              height = new_height;
              length = t.length + 1;
              tail = None;
              tail_offset = t.tail_offset + 32
            }
      end
;;

let fold t ~init ~f =
  let rec loop acc n =
    if n = t.length then acc
    else loop (f acc (get t n)) (n + 1)
  in
  loop init 0 
;;

let iter t ~f = fold t ~init:() ~f:(fun () v -> f v)
let iteri t ~f = fold t ~init:0 ~f:(fun i v -> f v i; (i + 1))
let of_list l = List.fold_left l ~init:empty ~f:(fun t v -> snoc t v)
let to_list t = List.rev (fold t ~init:[] ~f:(fun acc v -> v :: acc))
let length t = t.length
let is_empty t = length t = 0
let to_array t = Array.init t.length ~f:(fun i -> get t i)

let find t ~f =
  let rec loop n =
    if n = t.length then None
    else begin
      let v = get t n in
      if f v then Some v
      else loop (n + 1)
    end
  in
  loop 0
;;

let exists t ~f =
  match find t ~f with
  | Some _ -> true
  | None -> false
;;

let for_all t ~f =
  let f v = not (f v) in
  match find t ~f with
  | None -> true
  | Some _ -> false
;;

let sexp_of_t f t = List.sexp_of_t f (to_list t)
let t_of_sexp f sexp = of_list (List.t_of_sexp f sexp)

let container = {
  Container.
  length = length;
  is_empty = is_empty;
  iter = iter;
  fold = fold;
  exists = exists;
  for_all = for_all;
  find = find;
  to_list = to_list;
  to_array = to_array;
}

