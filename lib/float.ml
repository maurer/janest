(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)

TYPE_CONV_PATH "Float"

module Sexp = Sexplib.Sexp
module String = Core_string
open Core_printf

module T = struct
  type t = float with sexp, bin_io

  type binable = t
  type sexpable = t

  
  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y
  let hash (x : t) = Hashtbl.hash_param 1 1 x
end

include T

type floatable = t
let to_float x = x
let of_float x = x

type stringable = t
let of_string = float_of_string
let to_string = string_of_float

let max_value = infinity
let min_value = neg_infinity
let zero = 0.

let is_nan x = (x : t) <> x
include Float_robust_compare

include Hashable.Make (T)

let of_int = Pervasives.float_of_int

let to_int f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> int_of_float f
  | FP_infinite | FP_nan -> failwith "Float.to_int on nan or inf"

let of_int64 i = Int64.to_float i

let to_int64 f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> Int64.of_float f
  | FP_infinite | FP_nan -> failwith "Float.to_int64 on nan or inf"

let truncate f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> truncate f
  | FP_infinite | FP_nan -> failwith "truncate on nan or inf"

(* max_int/min_int are architecture dependent, e.g. +/- 2^30, +/- 2^62 if 32-bit, 64-bit
   (respectively) while float is IEEE standard for double (52 significant bits).  We want
   to avoid losing information as we round from float to int, so we "cap" what we will
   round based on the less of int and float significant digits.
*)
let float_round_lb = max (float_of_int min_int) (-1.0 *. 2.0 ** 52.0)
let float_round_ub = min (float_of_int max_int) (2.0 ** 52.0)
let int_round_lb = int_of_float float_round_lb
let int_round_ub = int_of_float float_round_ub

let round x = floor (x +. 0.5)

let iround x =
  if float_round_lb < x && x < float_round_ub then
    Some (int_of_float (round x))
  else None (* float too big to round reliably to int *)

let iround_exn x =
  match iround x with
  | None -> failwithf "Float.iround_exn: argument out of bounds (%f)" x ()
  | Some n -> n

let is_inf x = (classify_float x = FP_infinite);;

let min_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x < y then x else y

let max_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x > y then x else y

let add = (+.)
let sub = (-.)
let abs = abs_float
let scale = ( *. )

type comparable = t
let min (x : t) y =
  if is_nan x || is_nan y then nan
  else if x < y then x else y
let max (x : t) y =
  if is_nan x || is_nan y then nan
  else if x > y then x else y

let modf = modf
let floor = floor
let ceil = ceil
let mod_float = mod_float

module Class = struct
  type t =
  | Infinite
  | Nan
  | Normal
  | Subnormal
  | Zero
  with sexp, bin_io

  type binable = t
  type sexpable = t
  type stringable = t

  let to_string t = Sexp.to_string (sexp_of_t t)
  let of_string s = t_of_sexp (Sexp.Atom s)
end

module C = Class

let classify t =
  match Pervasives.classify_float t with
  | FP_normal -> C.Normal
  | FP_subnormal -> C.Subnormal
  | FP_zero -> C.Zero
  | FP_infinite -> C.Infinite
  | FP_nan -> C.Nan
;;

let compare (x : t) y = compare x y
let ascending = compare
let descending x y = compare y x

let ( >= ) (x : t) y = x >= y
let ( <= ) (x : t) y = x <= y
let ( = ) (x : t) y = x = y
let ( > ) (x : t) y = x > y
let ( < ) (x : t) y = x < y
let ( <> ) (x : t) y = x <> y

let (+) t t' = t +. t'
let (-) t t' = t -. t'
let ( * ) t t' = t *. t'
let (/) t t' = t /. t'

module Set = Core_set.Make (T)
module Map = Core_map.Make (T)

module Sign = struct
  type t = Neg | Zero | Pos
end

let sign t =
  if t >. 0. then Sign.Pos
  else if t <. 0. then Sign.Neg
  else Sign.Zero
