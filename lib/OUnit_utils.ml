(*pp camlp4o -I `ocamlfind query type-conv` pa_type_conv.cmo *)
TYPE_CONV_PATH "Core.OUnit_utils"

open Std_internal

module Random = struct
  let rnd_state = Random.State.make_self_init ()
  let float x = Random.State.float rnd_state x
  let int x = Random.State.int rnd_state x
  let bits () = Random.State.bits rnd_state
end

let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let sum_int = List.fold_left ~f:(+) ~init:0

(** float generator *)
let fg () = 
  exp (Random.float 15. *. (if Random.float 1. < 0.5 then 1. else -1.))
  *. (if Random.float 1. < 0.5 then 1. else -1.)

(** positive float generator *)
let pfg () = abs_float (fg ())

(** natural number generator *)
let nng () = 
  let p = Random.float 1. in
  if p < 0.5 then Random.int 10
  else if p < 0.75 then Random.int 100
  else if p < 0.95 then Random.int 1_000
  else Random.int 10_000

let png () = nng () + 1

(** Uniform random int generator *)
let uig () = Random.bits () - max_int / 2

(** list generator *)
let lg gen ?(size=nng) () =
  foldn ~f:(fun acc _ -> (gen ())::acc) ~init:[] (size ())

(** pair generator *)
let pg gen1 gen2 () = (gen1 (), gen2 ())

(** triple generator *)
let tg g1 g2 g3 () = (g1 (),g2 (), g3 ())

(** char generator *)
let cg () = char_of_int (Random.int 255)

(** string generator *)
let sg ?(char = cg) ?(size = nng) () = 
  let s = String.create (size ()) in
  for i = 0 to String.length s - 1 do
    s.[i] <- char ()
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
let frequencyg l = frequency (List.map ~f:(fun (i,e) -> (i,always e)) l)

let repeat times test gen =
  for i = 1 to times do test (gen()) done

