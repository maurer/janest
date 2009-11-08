(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(** Tail recursive version of list functions, plus a few additional
 * operations on lists.
 *)
TYPE_CONV_PATH "Core_list"

module List = StdLabels.List
module String = StdLabels.String

let invalid_argf = Core_printf.invalid_argf

type 'a t = 'a list with sexp, bin_io

type 'a binable = 'a t
type 'a container = 'a t
type 'a sexpable = 'a t

(* Standard functions *)
let length = List.length
let hd_exn = List.hd
let tl_exn = List.tl

let hd t =
  match t with
  | [] -> None
  | x :: _ -> Some x
;;

let tl t =
  match t with
  | [] -> None
  | _ :: t' -> Some t'
;;

let nth t n =
  if n < 0 then None else
  let rec nth_aux t n =
    match t with
    | [] -> None
    | a::t -> if n = 0 then Some a else nth_aux t (n-1)
  in nth_aux t n
;;

let nth_exn t n =
  match nth t n with
  | None ->
      invalid_argf "List.nth_exn %d called on list of length %d"
        n (List.length t) ()
  | Some a -> a
;;

let rev = List.rev
let rev_append = List.rev_append
let iter = List.iter
let rev_map = List.rev_map
let fold_left = List.fold_left
let iter2 = List.iter2
let rev_map2 = List.rev_map2
let fold_left2 = List.fold_left2
let for_all2 = List.for_all2
let exists2 = List.exists2
let mem = List.mem
let memq = List.memq
let filter = List.filter
let find_all = List.find_all

let assoc = List.assoc
let mem_assoc = List.mem_assoc
let remove_assoc = List.remove_assoc


let sort = List.sort
let stable_sort = List.stable_sort
let fast_sort = List.fast_sort


let find_map t ~f =
  let rec loop = function
    | [] -> None
    | x :: l ->
        match f x with
        | None -> loop l
        | Some _ as r -> r
  in
  loop t
;;

let find t ~f =
  let rec loop = function
    | [] -> None
    | x :: l -> if f x then Some x else loop l
  in
  loop t
;;

let find_exn t ~f = List.find t ~f

let findi t ~f =
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l -> if f i x then Some (i, x) else loop (i + 1) l
  in
  loop 0 t
;;

(** changing the order of arguments on some standard [List] functions. *)
let exists t ~f = List.exists t ~f
let for_all t ~f = List.for_all t ~f
let iter t ~f = List.iter t ~f

(** For the container interface. *)
let fold t ~init ~f = fold_left t ~f ~init
let to_array = Caml.Array.of_list
let to_list t = t

(** Tail recursive versions of standard [List] module *)

let append l1 l2 = List.rev_append (List.rev l1) l2
(* Rebind [@] so that uses below get our tail-recursive version rather than
   Pervasive's nontail version. *)
let (@) = append
let map l ~f = List.rev (List.rev_map ~f l)
let (>>|) l f = map l ~f
let map2 l1 l2 ~f = List.rev (List.rev_map2 l1 l2 ~f)

let rev_map3 l1 l2 l3 ~f =
  let rec loop l1 l2 l3 ac =
    match (l1, l2, l3) with
    | ([], [], []) -> ac
    | (x1 :: l1, x2 :: l2, x3 :: l3) -> loop l1 l2 l3 (f x1 x2 x3 :: ac)
    | _ -> invalid_arg "List.rev_map3"
  in
  loop l1 l2 l3 []
;;

let map3 l1 l2 l3 ~f = List.rev (rev_map3 l1 l2 l3 ~f)

let rec rev_map_append l1 l2 ~f =
  match l1 with
  | [] -> l2
  | h :: t -> rev_map_append ~f t (f h :: l2)

let fold_right l ~f ~init =
  List.fold_left ~f:(fun a b -> f b a) ~init (List.rev l)

let fold_right2 l1 l2 ~f ~init =
  List.fold_left2 ~f:(fun a b c -> f b c a) ~init (List.rev l1) (List.rev l2)

let split list =
  let rec loop list l1 l2 = match list with
      [] -> (List.rev l1,List.rev l2)
    | (x,y)::tl -> loop tl (x::l1) (y::l2)
  in
  loop list [] []

let combine l1 l2 = map2 ~f:(fun a b -> (a,b)) l1 l2

(** Additional list operations *)

let mapi l ~f =
  let (_,out) = List.fold_left ~f:(fun (i,list) x -> (i+1,(f i x)::list))
    ~init:(0,[]) l in
  List.rev out

let iteri l ~f =
  ignore (List.fold_left l ~init:0 ~f:(fun i x -> f i x; i + 1));
;;


let fold_lefti l ~f ~init =
  let (_,final_accum) =
    List.fold_left
      ~f:(fun (i,real_acc) v -> (i+1, f i real_acc v))
      ~init:(0,init) l
  in final_accum

let filteri l ~f =
  List.rev (fold_lefti l
               ~f:(fun pos acc x ->
                 if f pos x then x :: acc else acc)
               ~init:[])

let reduce l ~f = match l with
  | [] -> None
  | hd::tl -> Some (List.fold_left ~init:hd ~f tl)

let reduce_exn l ~f =
  match reduce l ~f with
  | None -> raise (Invalid_argument "List.reduce_exn")
  | Some v -> v

let groupi l ~break =
  let groups =
    fold_lefti l ~init:[] ~f:(fun i acc x ->
      match acc with
      | [] -> [[x]]
      | hd :: tl ->
          if break i x (hd_exn hd) then
            [x] :: hd :: tl
          else
            (x :: hd) :: tl)
  in
  match groups with
  | [] -> []
  | l -> rev_map l ~f:rev

let group l ~break = groupi l ~break:(fun _ x y -> break x y)

let concat_map l ~f =
  let rec aux acc = function
    | [] -> List.rev acc
    | hd::tl -> aux (rev_append (f hd) acc) tl
  in
  aux [] l

let merge l1 l2 ~cmp =
  let rec loop acc l1 l2 =
    match l1,l2 with
    | [], l2 -> rev_append acc l2
    | l1, [] -> rev_append acc l1
    | h1::t1, h2::t2 ->
        if cmp h1 h2 <= 0
        then loop (h1::acc) t1 l2
        else loop (h2::acc) l1 t2
  in
  loop [] l1 l2
;;


include struct
  (* We are explicit about what we import from the general Monad functor so that
   * we don't accidentally rebind more efficient list-specific functions.
   *)
  module Monad = Monad.Make (struct
    type 'a t = 'a list
    let bind x f = concat_map x ~f
    let return x = [x]
  end)
  open Monad
  module Monad_infix = Monad_infix
  type 'a monad = 'a t
  let unit = unit
  let ignore = ignore
  let join = join
  let bind = bind
  let (>>=) = bind
  let return = return
end

(** combines two lists, possibly of different length, returning a list the
    length of the min-length list *)
let min_combine l1 l2 =
  let rec loop l1 l2 accum = match (l1,l2) with
    | ([],_) | (_,[]) -> List.rev accum
    | (hd1::tl1,hd2::tl2) ->
        loop tl1 tl2 ( (hd1,hd2)::accum )
  in
  loop l1 l2 []

(** returns final element of list *)
let rec last_exn list = match list with
  | [x] -> x
  | _::tl -> last_exn tl
  | [] -> raise (Invalid_argument "Core_list.last")

(** optionally returns final element of list *)
let rec last list = match list with
  | [x] -> Some x
  | _::tl -> last tl
  | [] -> None

(** returns sorted version of list with duplicates removed *)

let dedup ?(compare=Pervasives.compare) list =
  let sorted = List.sort ~cmp:(fun x y -> compare y x) list in
  let rec loop list accum = match list with
      [] -> accum

    | hd::[] -> loop [] (hd::accum)
    | hd1::hd2::tl ->
        if compare hd1 hd2 = 0
        then loop (hd2::tl) accum
        else loop (hd2::tl) (hd1::accum)
  in
  loop sorted []

let stable_dedup lst =
  let rec dedup_order lst left set =
    match lst with
    | [] -> List.rev left
    | hd::tl ->
        if Core_set.mem set hd
        then dedup_order tl left set
        else dedup_order tl (hd::left) (Core_set.add set hd)
  in
  dedup_order lst [] Core_set.empty

let contains_dup lst = List.length (dedup lst) <> List.length lst

let find_a_dup l =
  let sorted = List.sort ~cmp:compare l in
  let rec loop l = match l with
      [] | [_] -> None
    | hd1::hd2::tl ->
        if hd1 = hd2 then Some (hd1) else loop (hd2::tl)
  in
  loop sorted

(** Returns number of elements in list for which test function returns true *)
let count list ~f =
  List.fold_left ~init:0 ~f:(fun count el -> if f el then count + 1 else count)
    list

(** [range low high] returns integers in range from [low](inclusive) to
    [high](exclusive).  [stride] is used to specify the step size *)

let range ?(stride=1) low high =
  if stride <= 0 then
    invalid_arg "Core_list.range: stride must be positive";
  let rec loop low high accum =
    if low >= high then accum
    else loop (low + stride) high (low::accum)
  in
  List.rev (loop low high [])

(** Like [range], but for floating point *)

let frange ?(stride=1.) low high =
  if Float_robust_compare.(<=.) stride 0. then
    invalid_arg "Core_list.frange: stride must be positive";
  let epsilon = stride /. 1000. in
  let rec loop low high accum =
    if low > high -. epsilon then accum
    else loop (low +. stride) high (low::accum)
  in
  List.rev (loop low high [])

let init n ~f =
  if n < 0 then invalid_argf "List.init %d" n ();
  let rec loop i accum =
    assert (i >= 0);
    if i = 0 then accum
    else loop (i-1) (f (i-1)::accum)
  in
  loop n []
;;

let rev_filter_map l ~f =
  let rec loop l accum = match l with
    | [] -> accum
    | hd :: tl ->
        match f hd with
        | Some x -> loop tl (x :: accum)
        | None -> loop tl accum
  in
  loop l []
;;

let filter_map l ~f = List.rev (rev_filter_map l ~f)

let filter_opt l = filter_map l ~f:(fun x -> x)

let partition_map t ~f =
  let rec loop t fst snd =
    match t with
    | [] -> (rev fst, rev snd)
    | x :: t ->
        match f x with
        | `Fst y -> loop t (y :: fst) snd
        | `Snd y -> loop t fst (y :: snd)
  in
  loop t [] []
;;

let partition t ~f =
  let f x = if f x then `Fst x else `Snd x in
  partition_map t ~f
;;


(** Reverses an association list. *)
let reverse_pairs lst =
  let rec loop lst acc =
    match lst with
    | [] -> acc
    | (x, y) :: tl -> loop tl ((y, x) :: acc)
  in
  List.rev (loop lst [])

let sub l ~pos ~len =

  if pos < 0 || len < 0 || pos + len > List.length l then invalid_arg "List.sub";
  List.rev
    (fold_lefti l ~init:[]
       ~f:(fun i acc el ->
             if i >= pos && i < (pos + len)
             then el :: acc
             else acc
          )
    )

let normalize a i =
  Ordered_collection_common.normalize ~length_fun:List.length a i
let slice start stop a =
  Ordered_collection_common.slice ~length_fun:List.length ~sub_fun:sub
    start stop a

let split_n list n =
  if n < 0 then invalid_arg "n < 0 in List.split_n"
  else if n >= List.length list then
    list,[]
  else
    let rec loop n list accum =
      if n = 0 then (accum, list)
      else match list with
      | [] -> assert false
      | hd::tl -> loop (n-1) tl (hd::accum)
    in
    let (rev_first, second) = loop n list [] in
    (List.rev rev_first, second)

let take list n = fst (split_n list n)
let drop list n = snd (split_n list n)

let rec drop_while lst ~f =
  match lst with
  | h :: t when f h -> drop_while t ~f
  | lst -> lst

let assoc_opt key list =
  try
    let el = List.assoc key list in
    Some el
  with Not_found -> None

let cartesian_product list1 list2 =
  if list2 = [] then [] else
    let rec loop l1 l2 accum = match l1 with
      | [] -> accum
      | (hd::tl) ->
          loop tl l2
            (List.rev_append
               (map ~f:(fun x -> (hd,x)) l2)
               accum)
    in
    List.rev (loop list1 list2 [])

let flatten l = fold_right l ~init:[] ~f:append
let flatten_no_order l = fold_left l ~init:[] ~f:rev_append

let concat = flatten

let cons x l = x :: l

let is_empty l = match l with [] -> true | _ -> false

let is_sorted l ~compare =
  let rec loop l =
    match l with
    | [] | [_] -> true
    | x1 :: ((x2 :: _) as rest) ->
        compare x1 x2 <= 0 && loop rest
  in loop l
;;

module Infix = struct
  let ( @ ) = append
end

let assoc' l x ~equal =
  Option.map (find l ~f:(fun (x', _) -> equal x x')) ~f:snd
;;

let assoc_exn' t x ~equal =
  match assoc' t x ~equal with
  | None -> failwith "assoc_exn"
  | Some b -> b
;;

let mem_assoc' t x ~equal = Option.is_some (assoc' t x ~equal)

let remove_assoc' t x ~equal =
  rev (fold t ~init:[] ~f:(fun ac ((x', _) as pair) ->
    if equal x x' then ac else pair :: ac))
;;


let permute ?random_state lst =
  let arr = Array.of_list lst in
  Array_permute.permute ?random_state arr;
  Array.to_list arr
;;

let to_string f x =
  Sexplib.Sexp.to_string
    (sexp_of_t (fun x -> Sexplib.Sexp.Atom x) (List.map ~f x))

let compare a b ~cmp =
  let rec loop a b =
    match a, b with
    | [], [] -> 0
    | [], _  -> -1
    | _ , [] -> 1
    | x::xs, y::ys ->
      let n = cmp x y in
      if n = 0 then loop xs ys
      else n
  in
  loop a b
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
