(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` pa_type_conv.cmo pa_sexp_conv.cmo *)
TYPE_CONV_PATH "Piecewise_linear"

open Std_internal

module type Key = sig
  type t
  include Floatable with type floatable = t
  include Sexpable with type sexpable = t
end

module type S = sig
  type key
      
  type t
  include Sexpable with type sexpable = t

  val create : (key * float) list -> [ `Succeed of t | `Fail of string ]
  val get : t -> key -> float
end

module Make (Key : Key) = struct

  (* todo: consider binary search in array for efficiency *)
  type t = (float * float) list

  type key = Key.t

  let create knots =
    let t = List.map knots ~f:(fun (x, y) -> (Key.to_float x, y)) in
    let x_values = List.map knots ~f:fst in
    let sorted_x_values = List.sort x_values ~cmp:ascending in
    if t = [] then `Fail "no knots given"
    else if x_values <> sorted_x_values then `Fail "knots are unsorted"
    else if List.contains_dup x_values then `Fail "duplicated x-values in knots"
    else `Succeed t

  type knots = (Key.t * float) list with sexp

  type sexpable = t
      
  let t_of_sexp sexp =
    let knots = knots_of_sexp sexp in
    match create knots with
    | `Fail str -> raise (Sexplib.Conv.Of_sexp_error (str, sexp))
    | `Succeed t -> t

  let sexp_of_t t =
    let knots = List.map t ~f:(fun (x, y) -> (Key.of_float x, y)) in
    sexp_of_knots knots
      
  let linear x (x1, y1) (x2, y2) =
    let weight =
      if x1 =. x2 then 0.5 (* for numerical stability *)
      else (x -. x1) /. (x2 -. x1)
    in
    (1. -. weight) *. y1 +. weight *. y2

  let rec get t x =
    let x = Key.to_float x in
    let rec loop = function
      | left :: ((right :: _) as rest) ->
          if x <= fst left then snd left
          else if x <= fst right then linear x left right
          else loop rest
      | last :: [] -> snd last
      | [] -> failwith "Bug in Piecewise_linear.get"
    in
    loop t
    
end

module Time = Make (Time)
module Float = Make (Float)
module Int = Make (Int)
