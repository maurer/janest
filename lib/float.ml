(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)

TYPE_CONV_PATH "Float"

module Sexp = Sexplib.Sexp
module String = Core_string
open Core_printf

type t = float with sexp, bin_io

type sexpable = t
type binable = t

type floatable = t
let to_float x = x
let of_float x = x

type stringable = t
let of_string = float_of_string
let to_string = string_of_float

let max_value = infinity
let min_value = neg_infinity
let zero = 0.
let epsilon = 0.000001

let is_nan x = (x : t) <> x

type robustly_comparable = t
let ( >=. ) x y = x >= y -. epsilon
let ( <=. ) x y = y >=. x
let ( =. ) x y = x >=. y && y >=. x
let ( >. ) x y = x > y +. epsilon
let ( <. ) x y = y >. x
let ( <>. ) x y = not (x =. y)

include Hashable.Make (struct
  type t = float
  let equal (x : t) y = x = y
  let hash (x : t) = Hashtbl.hash_param 1 1 x
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
end)

let of_int = Pervasives.float_of_int

let to_int f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> int_of_float f
  | FP_infinite | FP_nan -> failwith "int_of_float on nan or inf"

let to_string_hum f =
  let s_rev s = (* In place reversal *)
    if s = "" then
      ""
    else
      let n = (String.length s - 1)in
      for i = 0 to n/2 do
        let c1 = s.[i]
        and c2 = s.[n-i] in
        s.[i] <- c2;
        s.[n-i] <- c1
      done;
      s
  in
  let put_ s =
    let l = String.length s in
    let nu = (*The number of underscores to insert *)
      (l/3) + (if (l mod 3) > 1 then 0 else -1)
    in
    if nu <= 0 then
      s
    else begin
      let l' = (l+nu) in
      let res = String.create l' in
      for i = 1 to nu do
        String.blit ~src:s ~src_pos:(l-3*i) ~dst_pos:(l'+1-4*i) ~len:3 ~dst:res;
        res.[l'-4*i] <- '_'
      done;
      let len =
        match (l mod 3) with
        | (0|1) as i -> i+3
        | i -> i
      in
      String.blit ~src:s ~src_pos:0 ~dst_pos:0 ~len:len ~dst:res;
      res
    end
  in
  let s = string_of_float f in
  match String.split2 s '.' with
  | Some (ip,fp) -> (put_ ip) ^ "." ^ (s_rev (put_ (s_rev fp)))
  | None -> s (*nan,infinity...*)

let truncate f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> truncate f
  | FP_infinite | FP_nan -> failwith "truncate on nan or inf"

let round x = int_of_float (floor (x +. 0.5));;

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

let min_normal_pos =
  lazy
    (let rec loop x =
      let x' = x / 3. in
      match classify x' with
      | C.Subnormal -> x
      | C.Normal -> loop x'
      | C.Infinite | C.Nan | C.Zero ->
          failwithf "min_normal_pos bug %f %f" x x' ()
      in
     loop 1.)
;;
