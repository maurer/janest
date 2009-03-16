(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(** Tail recursive version of list functions, plus a few additional
 * operations on lists.
 *)
TYPE_CONV_PATH "Core_list"

module List = StdLabels.List
module String = StdLabels.String
module Set = PSet

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
  if n < 0 then raise (Invalid_argument "List.nth") else
    match nth t n with
    | None -> raise (Failure "nth")
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
let for_all = List.for_all
let exists = List.exists
let for_all2 = List.for_all2
let exists2 = List.exists2
let mem = List.mem
let memq = List.memq
let filter = List.filter
let find_all = List.find_all
let partition = List.partition
let assoc = List.assoc
let mem_assoc = List.mem_assoc
let remove_assoc = List.remove_assoc
let sort = List.sort
let stable_sort = List.stable_sort
let fast_sort = List.fast_sort
let merge = List.merge

let rec find t ~f =
  let rec loop = function
    | [] -> None
    | x :: l -> if f x then Some x else loop l
  in
  loop t
;;

let find_exn t ~f = List.find t ~f

let exists t ~f = List.exists t ~f
let for_all t ~f = List.for_all t ~f
let fold t ~init ~f = fold_left t ~f ~init
let iter t ~f = List.iter t ~f
let to_array = Caml.Array.of_list
let to_list t = t

(** Tail recursive versions of standard [List] module *)

let append l1 l2 = List.rev_append (List.rev l1) l2
let map l ~f = List.rev (List.rev_map ~f l)
let map2 l1 l2 ~f = List.rev (List.rev_map2 ~f l1 l2)

let rev_map3 l1 l2 l3 ~f =
  let rec loop l1 l2 l3 ac =
    match (l1, l2, l3) with
    | ([], [], []) -> ac
    | (x1 :: l1, x2 :: l2, x3 :: l3) -> loop l1 l2 l3 (f x1 x2 x3 :: ac)
    | _ -> invalid_arg "List.rev_map3"
  in
  loop l1 l2 l3 []
;;

let map3 l1 l2 l3 ~f = List.rev (rev_map3 ~f l1 l2 l3)

let rec rev_map_append l1 ~f l2 =
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
  let _ = List.fold_left ~f:(fun i x -> f i x; i + 1) ~init:0 l in
  ()

let fold_lefti l ~f ~init =
  let (_,final_accum) =
    List.fold_left
      ~f:(fun (i,real_acc) v -> (i+1, f i real_acc v))
      ~init:(0,init) l
  in final_accum

let rec fold_left_term_internal list ~f ~acc continue =
  match continue,list with
  | false,_ | _,[] -> acc
  | true, v::tl ->
      let (continue,acc) = f acc v in
      fold_left_term_internal tl ~f ~acc continue

let fold_left_term list ~f ~init =
  fold_left_term_internal list ~f ~acc:init true

let reduce l ~f = match l with
  | [] -> raise (Invalid_argument "List.reduce")
  | hd::tl -> List.fold_left ~init:hd ~f tl

let best l ~f =
  try reduce ~f l
  with Invalid_argument "List.reduce" -> raise (Invalid_argument "List.best")

let concat_map l ~f =
  let rec aux acc = function
    | [] -> acc
    | hd::tl -> aux (f hd @ acc) tl
  in
  aux [] (List.rev l)

let map_aux = map
include (Monad.Make
           ( struct
               type 'a t = 'a list
               let bind x f = concat_map ~f x
               let return x = [x]
             end)
        )
let map = map_aux (*Avoid being overwritten by Monad.map*)


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
let rec last list = match list with
  | [x] -> x
  | _::tl -> last tl
  | [] -> raise (Invalid_argument "Core_list.last")

(** returns sorted version of list with duplicates removed *)
let dedup list =
  let sorted = List.sort ~cmp:(fun x y -> compare y x) list in
  let rec loop list accum = match list with
      [] -> accum
    | hd::[] -> loop [] (hd::accum)
    | hd1::hd2::tl ->
        if hd1 = hd2 then loop (hd2::tl) accum
        else loop (hd2::tl) (hd1::accum)
  in
  loop sorted []

let stable_dedup lst =
  let rec dedup_order lst left set =
    match lst with
    | [] -> List.rev left
    | hd::tl ->
        if Set.mem set hd
        then dedup_order tl left set
        else dedup_order tl (hd::left) (Set.add set hd)
  in
  dedup_order lst [] Set.empty

let contains_dup lst = List.length (dedup lst) <> List.length lst

let find_a_dup l =
  let sorted = List.sort ~cmp:compare l in
  let rec loop l = match l with
      [] | [_] -> None
    | hd1::hd2::tl ->
        if hd1 = hd2 then Some (hd1) else loop (hd2::tl)
  in
  loop sorted

(* let stable_dedup lst = fst
    (List.fold_right
      ~f:(fun x (uniq, set) ->
              if Set.mem x then (x::uniq, Set.add x set) else (uniq, set))
      ~init:([], set.empty)
    ) *)

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
  if Float.(<=.) stride 0. then
    invalid_arg "Core_list.frange: stride must be positive";
  let epsilon = stride /. 1000. in
  let rec loop low high accum =
    if low > high -. epsilon then accum
    else loop (low +. stride) high (low::accum)
  in
  List.rev (loop low high [])

(** [init n ~f] Creates a new list of length [n], using [f] to instantiate
    the elements. *)
(* CR sweeks: [init] currently returns the empty list if [n < 0].  I think it
   would be better to fail. *)
let init n ~f =
  if n < 0 then invalid_argf "List.init %d" n ();
  let rec loop i accum =
    if i <= 0 then accum
    else loop (i-1) (f (i-1)::accum)
  in
  loop n []

(** [rev_filter_map ~f l] applies [f] to [l], filtering out elements
    for which [f] returns [None], and unwrapping those that return [Some].
    Returns reversed list. *)
let rev_filter_map l ~f =
  let rec loop l accum = match l with
    | [] -> accum
    | hd :: tl ->
        match f hd with
        | Some x -> loop tl (x :: accum)
        | None -> loop tl accum
  in
  loop l []

(** [filter_map ~f l] applies [f] to [l], filtering out elements for which
    [f] returns [None], and unwrapping those that return [Some] *)
let filter_map l ~f = List.rev (rev_filter_map ~f l)

let filter_opt l = filter_map ~f:(fun x -> x) l

let partition_map l ~f = 
  let rec loop l pass fail = match l with
      [] -> (List.rev pass,List.rev fail)
    | hd::tl -> match f hd with
        `Pass x -> loop tl (x::pass) fail
      | `Fail x -> loop tl pass (x::fail)
  in
  loop l [] []

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


(** [split_n n l] returns two lists, the first containing the first n elements of the
    list, the second containing the rest of the list *)
let split_n t n = 
  let rec loop n list accum = 
    if n <= 0 then (accum,list)
    else match list with
      [] -> (accum,list)
    | hd::tl -> loop (n-1) tl (hd::accum)
  in
  let (rev_first,second) = (loop n t []) in
  (List.rev rev_first,second)

(** [first_n n l] Returns the first [n] elements of list [l] *)
let first_n t n = fst (split_n t n)

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

let concat = flatten

let cons x l = x :: l

let is_empty l = match l with [] -> true | _ -> false

let to_string x_to_string t =
  String.concat ~sep:""
    ["[";
     String.concat ~sep:"; " (map t ~f:x_to_string);
     "]";]
;;
  
module Infix = struct
  let ( @ ) = append
end

let assoc' l f = Option.map (find l ~f:(fun (a, _) -> f a)) ~f:snd

let assoc_exn' t f =
  match assoc' t f with
  | None -> failwith "assoc_exn"
  | Some b -> b
;;

let mem_assoc' t f = Option.is_some (assoc' t f)

let remove_assoc' t f =
  rev (fold t ~init:[] ~f:(fun ac ((a, _) as pair) ->
    if f a then ac else pair :: ac))
;;

(*
  other non tail-recursive functions to fix (eventually)

  let remove_assoc
*)

let shuffle lst =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  Array.iteri (fun i tile ->
    let ran = Random.int (len - i) in
    arr.(i) <- arr.(ran + i);
    arr.(ran + i) <- tile
  ) arr;
  Array.to_list arr
;;

