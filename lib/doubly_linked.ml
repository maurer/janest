open Sexplib.Conv

let phys_equal = Caml.(==)

module List = Core_list

module Elt = struct
  module T = struct
    type 'a t = {
      value : 'a;
      mutable next : 'a t;
      mutable prev : 'a t;
    }
  end
  include T

  

  let next t = t.next
  let prev t = t.prev
  let value t = t.value

  let sexp_of_t sexp_of_a t = sexp_of_a t.value

  let create value =
    let rec t = {
      value = value;
      next = t;
      prev = t;
    } in
    t
  ;;

  let equal (t : 'a t) t' = phys_equal t t'

  let link t1 t2 =
    t1.next <- t2;
    t2.prev <- t1;
  ;;

  let unlink t =
    t.prev.next <- t.next;
    t.next.prev <- t.prev;
    t.next <- t;
    t.prev <- t;
  ;;
end

open Elt.T

type 'a t = {
  (* [first] is a pointer to the first element in the list.  It is an option
   * because it is [None] if the list is empty.  When the list is nonempty,
   * [first] is [Some r], where [!r] is the first element.  We use a reference
   * so that we can change the pointer without having to allocate a new
   * [Some] option.
   *)
  mutable first : 'a Elt.t ref option;
  mutable length : int;
}

type 'a container = 'a t

(* Compare lists using phys_equal, which makes sense because there is a mutable
 * field in the record.
 *)
let equal (t : 'a t) t' = phys_equal t t'

let invariant t =
  if t.length = 0 then
    assert (Option.is_none t.first)
  else begin
    match t.first with
    | None -> assert false
    | Some r ->
        let first_elt = !r in
        let rec loop i elt =
          if i = 0 then
            assert (Elt.equal elt first_elt)
          else begin
            assert (Elt.equal elt elt.next.prev);
            assert (Elt.equal elt elt.prev.next);
            assert (Elt.equal elt first_elt = (i = t.length));
            loop (i - 1) elt.next
          end
        in
        loop t.length first_elt
  end
;;

let maybe_check t =
  let check_invariant = false in        (* for debugging *)
  if check_invariant then invariant t

let create () =
  let t = {
    first = None;
    length = 0;
  }
  in
  maybe_check t;
  t
;;

let length t = t.length

let is_empty t = t.length = 0


let is_first t elt =
  match t.first with
  | None -> assert false
  | Some r -> Elt.equal elt !r
;;

let set_first t elt =
  match t.first with
  | None -> assert false
  | Some r -> r := elt
;;

let is_last t elt = is_first t elt.next

let first_elt t = Option.map t.first ~f:(fun r -> !r)

let first t = Option.map (first_elt t) ~f:Elt.value

let last_elt t = Option.map t.first ~f:(fun r -> !r.prev)

let last t = Option.map (last_elt t) ~f:Elt.value

let next t elt = if is_last t elt then None else Some elt.next

let prev t elt = if is_first t elt then None else Some elt.prev

let fold_left_elts t ~init ~f =
  match t.first with
  | None -> init
  | Some r ->
      let rec loop i elt ac =
        if i = 0 then ac else loop (i - 1) elt.next (f ac elt)
      in
      loop t.length !r init
;;

let fold_left t ~init ~f =
  fold_left_elts t ~init ~f:(fun ac elt -> f ac elt.value)
;;

let fold_right t ~init ~f =
  match t.first with
  | None -> init
  | Some r ->
      let rec loop i elt ac =
        if i = 0 then ac
        else
          let elt = elt.prev in
          loop (i - 1) elt (f ac elt.value)
      in
      loop t.length !r init
;;

let fold = fold_left

let iteri t ~f =
  match t.first with
  | None -> ()
  | Some r ->
      
      let length = t.length in
      let rec loop i elt =
        if i < length then (f i elt.value ; loop (i + 1) elt.next)
      in
      loop 0 !r
;;

let iter t ~f = iteri t ~f:(fun _ x -> f x)

let for_all t ~f =
  match t.first with
  | None -> true
  | Some r ->
      let rec loop i elt = i = 0 || (f elt.value && loop (i - 1) elt.next) in
      loop t.length !r
;;

let exists t ~f =
  match t.first with
  | None -> false
  | Some r ->
      let rec loop i elt = i > 0 && (f elt.value || loop (i - 1) elt.next) in
      loop t.length !r
;;

let find_elt t ~f =
  match t.first with
  | None -> None
  | Some r ->
      let rec loop i elt =
        if i = 0 then None
        else if f elt.value then Some elt
        else loop (i - 1) elt.next
      in
      loop t.length !r
;;

let find t ~f = Option.map ~f:Elt.value (find_elt t ~f)

let to_list t = fold_right t ~init:[] ~f:(fun ac x -> x :: ac)

let to_array t =
  match t.first with
  | None -> [||]
  | Some r ->
      let length = t.length in
      let first_elt = !r in
      let a = Array.create length (Elt.value first_elt) in
      let rec loop i elt =
        if i < length then (a.(i) <- elt.value ; loop (i + 1) elt.next)
      in
      loop 0 first_elt;
      a
;;


let insert_before t elt x =
  let elt' = Elt.create x in
  
  if is_first t elt then set_first t elt';
  Elt.link elt.prev elt';
  Elt.link elt' elt;
  t.length <- t.length + 1;
  maybe_check t;
  elt'
;;

let insert_after t elt x =
  let elt' = Elt.create x in
  Elt.link elt' elt.next;
  Elt.link elt elt';
  t.length <- t.length + 1;
  maybe_check t;
  elt'
;;

let insert_into_empty t x =
  assert (t.length = 0);
  let elt = Elt.create x in
  t.first <- Some (ref elt);
  t.length <- 1;
  elt
;;

let insert_first t x =
  match first_elt t with
  | None -> insert_into_empty t x
  | Some elt -> insert_before t elt x
;;

let insert_last t x =
  match last_elt t with
  | None -> insert_into_empty t x
  | Some elt -> insert_after t elt x
;;

let of_list l =
  let t = create () in
  List.iter l ~f:(fun x -> ignore (insert_last t x));
  maybe_check t;
  t
;;

type 'a sexpable = 'a t

let t_of_sexp a_of_sexp sexp = of_list (list_of_sexp a_of_sexp sexp)

let sexp_of_t sexp_of_a t = sexp_of_list sexp_of_a (to_list t)


let remove t elt =
  begin match t.first with
  | None -> failwith "Doubly_linked.remove from empty list"
  | Some r ->
      if t.length = 1 then t.first <- None
      else if Elt.equal elt !r then r := elt.next;
  end;
  Elt.unlink elt;
  t.length <- t.length - 1;
  maybe_check t;
;;

let remove_first t =
  match first_elt t with
  | None -> None
  | Some elt -> remove t elt; Some elt.value
;;

let remove_last t =
  match last_elt t with
  | None -> None
  | Some elt -> remove t elt; Some elt.value
;;

let clear t =
  t.length <- 0;
  t.first <- None;
;;

let copy t =
  let t' = create () in
  iter t ~f:(insert_last t');
  t'
;;

let transfer ~src ~dst =
  match src.first with
  | None -> ()
  | Some src_first ->
      begin match dst.first with
      | None ->
          dst.length <- src.length;
          dst.first <- src.first;
      | Some dst_first ->
          let src_first = !src_first in
          let src_last = src_first.prev in
          let dst_first = !dst_first in
          let dst_last = dst_first.prev in
          Elt.link src_last dst_first;
          Elt.link dst_last src_first;
          dst.length <- dst.length + src.length;
      end;
      clear src;
      maybe_check src;
      maybe_check dst;
;;

let filter_inplace t ~f =
  let elts_to_remove =
    fold_left_elts t ~init:[] ~f:(fun ac elt ->
      if not (f elt.value) then elt :: ac else ac)
  in
  List.iter elts_to_remove ~f:(fun elt -> remove t elt)
;;

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
