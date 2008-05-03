(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core.Interval"

module List = Core_list

type 'a t = Interval of 'a * 'a | Empty with bin_io, sexp
(* CRv2 YM: The s-xpression converters should enforce the invariant that the interval's
   bounds can't be crossed, and should throw an exception on a crossed interval or
   auto-convert it to empty.  Not clear which approach is better. *)
(* sweeks: I'd err toward noisy failure rather than silence. *)

let empty = Empty

let empty_cvt = function
  | Empty -> Empty
  | Interval (x,y) as i -> if y < x then Empty else i

let make x y =
  (* if x >= y, then this is just the Empty interval. *)
  empty_cvt (Interval (x,y))

let intersect i1 i2 = match i1,i2 with
  | Empty,_ | _,Empty -> Empty
  | Interval (l1,u1), Interval (l2,u2) -> empty_cvt (Interval (max l1 l2,min u1 u2))

let is_empty = function Empty -> true | _ -> false

let is_empty_or_singleton = function
  | Empty -> true
  | Interval (x,y) -> x = y

let lbound_opt = function Empty -> None | Interval (l, _) -> Some l
let ubound_opt = function Empty -> None | Interval (_, u) -> Some u

let lbound = function
  | Empty -> invalid_arg "Interval.lbound: empty interval"
  | Interval (l,_) -> l

let ubound = function
  | Empty -> invalid_arg "Interval.ubound: empty interval"
  | Interval (_,u) -> u

let contains i x = match i with
  | Empty -> false
  | Interval (l,u) -> l <= x && u >= x

let time_contains i x = match i with
  | Empty -> false
  | Interval (l,u) -> Time.(<=) l x && Time.(<=) x u

let contains_interval i1 i2 = match i1,i2 with
  | Interval (l1,u1), Interval (l2,u2) -> l1 <= l2 && u1 >= u2
  | _, Empty -> true
  | _ -> false

let map ~f = function
  | Empty -> Empty
  | Interval (l,u) -> empty_cvt (Interval (f l, f u))

let are_disjoint_gen ~are_disjoint intervals =
  let intervals = Array.of_list intervals in
  try
    for i = 0 to Array.length intervals - 1 do
      for j = i + 1 to Array.length intervals - 1 do
        if not (are_disjoint intervals.(i) intervals.(j)) then raise Exit
      done
    done;
    true
  with
    Exit -> false

let are_disjoint intervals =
  are_disjoint_gen intervals
    ~are_disjoint:(fun i1 i2 -> is_empty (intersect i1 i2))

let are_disjoint_as_open_intervals intervals =
  are_disjoint_gen intervals
    ~are_disjoint:(fun i1 i2 -> is_empty_or_singleton (intersect i1 i2))

let list_intersect ilist1 ilist2 =
  if not (are_disjoint ilist1) || not (are_disjoint ilist2) then
    invalid_arg "Interval.list_intersect: non-disjoint input list";
  let pairs = List.cartesian_product ilist1 ilist2 in
  List.filter_map ~f:(fun (i1,i2) ->
			let i = intersect i1 i2 in
			if is_empty i then None else Some i)
    pairs


