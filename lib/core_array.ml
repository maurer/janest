(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core_array"

module Array = StdLabels.Array

type 'a t = 'a array with sexp, bin_io

type 'a binable = 'a t
type 'a container = 'a t
type 'a sexpable = 'a t

(* Standard functions *)
let append = Array.append
let blit = Array.blit
let concat = Array.concat
let copy = Array.copy
let create_matrix = Array.create_matrix
let fast_sort = Array.fast_sort
let fill = Array.fill
let fold_left = Array.fold_left
let fold_right = Array.fold_right
let init = Array.init
let iteri = Array.iteri
let make_matrix = Array.make_matrix
let map = Array.map
let mapi = Array.mapi
let of_list = Array.of_list
let sort = Array.sort
let stable_sort = Array.stable_sort
let sub = Array.sub
let to_list = Array.to_list
external create : int -> 'a -> 'a array = "caml_make_vect"
external get : 'a array -> int -> 'a = "%array_safe_get"
external length : 'a array -> int = "%array_length"
external make : int -> 'a -> 'a array = "caml_make_vect"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"

let to_array t = t

let is_empty t = length t = 0

let fold t ~init ~f = fold_left t ~init ~f

let iter t ~f = Array.iter t ~f

(** [normalize array index] returns a new index into the array such that if index is less
    than zero, the returned index will "wrap around" -- i.e. array.(normalize array (-1))
   returns the last element of the array. *)
let normalize a i =
  Ordered_collection_common.normalize ~length_fun:length a i

(** [slice array start stop] returns a fresh array including elements [array.(start)] 
    through [array.(stop-1)] with the small tweak that the start and stop positions are 
    normalized and a stop index of 0 means the same thing a stop index of 
    [Array.length array].  In summary, it's like the slicing in Python or Matlab. *)
let slice a start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub
    a start stop

(** [nget array index] "normalizes" the index to {!Array.get} -- see normalize *)
let nget x i = 
  x.(normalize x i)

(** [nset array index value] "normalizes" the index to {!Array.set} -- see normalize *)
let nset x i v = 
  x.(normalize x i) <- v

(** [filter_opt array] returns a new array where [None] entries are omitted and [Some x]
    entries are replaced with [x]. Note that this changes the index at which elements
    will appear. *)
let filter_opt ar =
  let result = ref [] in
  iter ar ~f:(function
    | None -> ()
    | Some x -> result := x :: !result
  );
  of_list (List.rev !result)

(* The following implementation of filter_opt is faster, and as far as we know it's
   correct, but we don't want to code review it yet until we actually need the speed.
let filter_opt ar =
  let lix = length ar - 1 in
  let rec outer_loop outer_i =
    if outer_i < 0 then [||]
    else
      match ar.(outer_i) with
      | None -> outer_loop (outer_i - 1)
      | Some el ->
          let rec loop i n =
            if i < 0 then
              let res = make n el in
              let rec inner_loop i pos =
                if i < 0 then res
                else
                  match ar.(i) with
                  | None -> inner_loop (i - 1) pos
                  | Some el ->
                      res.(pos) <- el;
                      inner_loop (i - 1) (pos - 1)
              in
              inner_loop (outer_i - 1) (n - 2)
            else
              match ar.(i) with
              | None -> loop (i - 1) n
              | Some _ -> loop (i - 1) (n + 1)
          in
          loop (outer_i - 1) 1
  in
  outer_loop lix
*)

(** [filter_map ~f array] maps [f] over [array] and filters [None] out of the results. *)
let filter_map ~f ar = filter_opt (map ~f ar)
(** Same as {!filter_map} but uses {!Array.mapi}. *)
let filter_mapi ~f ar = filter_opt (mapi ~f ar)

let map2 ~f ar1 ar2 =
  let len = length ar1 in
  if length ar2 <> len then invalid_arg "Array.map2";
  init len ~f:(fun i -> f ar1.(i) ar2.(i))

(** [filter ~f array] removes the elements for which [f] returns false.  *)
let filter ~f =  filter_map ~f:(fun x -> if f x then Some x else None)
(** Like {!filter} except [f] also receives the index. *)
let filteri ~f = filter_mapi ~f:(fun i x -> if f i x then Some x else None)

let swap ar i j =
  let tmp = ar.(i) in
  ar.(i) <- ar.(j);
  ar.(j) <- tmp

let exists t ~f =
  let rec loop i =
    if i < 0
    then false
    else if f t.(i)
    then true
    else loop (i - 1)
  in
  loop (length t - 1)

let mem el ar =
  let rec loop i =
    if i < 0
    then false
    else if ar.(i) = el
    then true
    else loop (i - 1)
  in
  loop (length ar - 1)

let for_all t ~f =
  let rec loop i =
    if i < 0
    then true
    else if f t.(i)
    then loop (i - 1)
    else false
  in
  loop (length t - 1)

(** reverses an array in place. *)
let rev ar =
  if length ar > 0 then
    for i = 0 to (length ar - 1) / 2 do
      let j = length ar - 1 - i in
      swap ar i j
    done

let replace t i ~f = t.(i) <- f t.(i)
      
(** modifies an array in place -- [ar.(i)] will be set to [f(ar.(i))] *)
let replace_all t ~f =
  for i = 0 to length t - 1 do
    t.(i) <- f t.(i)
  done

let findi t ~f =
  let length = length t in
  let rec loop i =
    if i >= length then None
    else if f t.(i) then Some i
    else loop (i + 1)
  in
  loop 0
;;

let findi_exn t ~f =
  match findi t ~f with
  | None -> raise Not_found
  | Some x -> x
;;

let find_exn t ~f =
  match findi t ~f with
  | None -> raise Not_found
  | Some i -> t.(i)
;;

let find t ~f = Option.map (findi t ~f) ~f:(fun i -> t.(i))

let reduce ~f a =
  if length a = 0 then invalid_arg "Array.reduce";
  let r = ref a.(0) in
  for i = 1 to length a - 1 do
    r := f !r a.(i)
  done;
  !r
      
let best ~f l =
  try reduce ~f l
  with Invalid_argument "Array.reduce" -> invalid_arg "Array.best"

(** randomly permute an array. *)
let permute ?(random_state=Random.get_state ()) ar = 
  let len = length ar in
  let rec loop i =
    if i + 1 < len then
      begin
        let j = i + Random.State.int random_state (len - i) in
        if j <> i then swap ar i j;
        loop (i + 1)
      end
  in
  loop 0

let combine ar1 ar2 =
  let len = length ar1 in
  if length ar2 <> len then failwith "Array.combine";
  if len = 0 then [||]
  else
    let first = ar1.(0), ar2.(0) in
    let res = make len first in
    for i = 1 to len - 1 do
      res.(i) <- ar1.(i), ar2.(i)
    done;
    res

let sorted_copy a ~cmp =
  let b = copy a in
  sort b ~cmp;
  b

let last t = t.(length t - 1)
    
module Infix = struct
  let ( <|> ) ar (start,stop) = slice ar start stop
end

let empty () = init 0 ~f:(fun _ -> assert false)

let cartesian_product t1 t2 =
  if is_empty t1 || is_empty t2 then
    empty ()
  else
    let n1 = length t1 in
    let n2 = length t2 in
    let t = create (n1 * n2) (t1.(0), t2.(0)) in
    let r = ref 0 in
    for i1 = 0 to n1 - 1 do
      let x1 = t1.(i1) in
      for i2 = 0 to n2 - 1 do
        t.(!r) <- (x1, t2.(i2));
        incr r;
      done
    done;
    t
;;
