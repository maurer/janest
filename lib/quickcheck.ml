(** Module for easily generating unit tests.  Based on code posted by
    padiolea\@irisa.fr to the caml mailing list. *)



open Std_internal

let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let sum_int = List.fold_left ~f:(+) ~init:0

(*------------------------------------------------------------------------------*)
(* generator *)
(*------------------------------------------------------------------------------*)
type 'a gen = unit -> 'a

let pfg () = exp (Random.float 30. -. 15.)

let fg () =
  pfg () *. (if Random.bool () then 1. else -1.)

(* natural number generator *)
let nng () =
  let p = Random.float 1. in
  if p < 0.5 then Random.int 10
  else if p < 0.75 then Random.int 100
  else if p < 0.95 then Random.int 1_000
  else Random.int 10_000

(* Below uniform random in range min_int, min_int+1,...,max_int.  Here's why:
 *   bound = max_int + 1
 *   0 <= r <= max_int
 *   0 <= r <= max_int  &&  -max_int -1 <= -r - 1 <= -1
 *   -max_int -1 <= result <= max_int
 *   min_int <= result <= max_int
 *)
let uig =
  let bound = Int64.add 1L (Int64.of_int max_int) in
  fun () ->
    let r = Int64.to_int_exn (Random.int64 bound) in
    if Random.bool () then r else -r - 1

let lg gen ?(size_gen=nng) () =
  foldn ~f:(fun acc _ -> (gen ())::acc) ~init:[] (size_gen ())

let pg gen1 gen2 () = (gen1 (), gen2 ())

let tg g1 g2 g3 () = (g1 (),g2 (), g3 ())

let cg () = char_of_int (Random.int 256)

let sg ?(char_gen = cg) ?(size_gen = nng) () =
  let s = String.create (size_gen ()) in
  for i = 0 to String.length s - 1 do
    s.[i] <- char_gen ()
  done;
  s

(** Given a list of generators, returns generator that randomly uses one of the generators
    from the list *)
let oneof xs =
  List.nth_exn xs (Random.int (List.length xs))

(** generator that always returns given value *)
let always x () = x

(** Given list of [(frequency,value)] pairs, returns value with probability proportional
    to given frequency *)
let frequency xs =
  let sums = sum_int (List.map ~f:fst xs) in
  let i = Random.int sums in
  let rec aux acc = function
    | ((x,g)::xs) -> if i < acc+x then g else aux (acc+x) xs
    | _ -> failwith "frequency"
  in
  aux 0 xs

(** like frequency, but returns generator *)
let frequencyl l = frequency (List.map ~f:(fun (i,e) -> (i,always e)) l)

(** [laws iter gen func] applies [func] repeatedly ([iter] times) on output of [gen], and
    if [func] ever returns false, then the input that caused the failure is returned
    optionally.  *)
let rec laws iter gen func =
  if iter <= 0 then None
  else
    let input = gen () in
    try
      if not (func input) then Some input
      else laws (iter-1) gen func
    with _ -> Some input

(** Like laws, but throws an exception instead of returning an option.  *)
let laws_exn name iter gen func =
  match laws iter gen func with
    None -> ()
  | Some _ -> failwith (Printf.sprintf "law %s failed" name)

let rec statistic_number = function
  | []    -> []
  | x::xs -> let (splitg, splitd) = List.partition ~f:(fun y -> y = x) xs in
    (1 + List.length splitg, x) :: statistic_number splitd

(* in percentage *)
let statistic xs =
  let stat_num = statistic_number xs in
  let totals = sum_int (List.map ~f:fst stat_num) in
  List.map ~f:(fun (i, v) -> ((i * 100) / totals), v) stat_num


let laws2 iter func gen =
  let res = foldn ~init:[] iter
    ~f:(fun acc _ -> let n = gen () in (n, func n) :: acc)
  in
  let stat = statistic (List.map ~f:(fun (_, (_, v)) -> v) res) in
  let res = List.filter ~f:(fun (_, (b, _)) -> not b) res in
  if res = [] then (None, stat) else (Some (fst (List.hd_exn res)), stat)

let repeat times test gen =
  for i = 1 to times do test (gen()) done
