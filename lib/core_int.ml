(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Int"

type t = int with bin_io

type sexpable = t
let sexp_of_t = Sexplib.Conv.sexp_of_int
let t_of_sexp = Sexplib.Conv.int_of_sexp

type binable = t

type stringable = t
let of_string = int_of_string
let to_string = string_of_int

type floatable = t
let of_float = int_of_float
let to_float = float_of_int

type comparable = t
let min (x : t) y = if x < y then x else y
let max (x : t) y = if x > y then x else y
(* CRv2 cfalls: Someone on the Caml list reported the following times, which if correct
   suggest that we should change our implementation of compare.

   let comp = compare
   -> 1,54s

   let comp (i:int) (j:int) = compare i j
   -> 0.7s

   let comp (i:int) (j:int) =
     if i = j then 0 else if i < j then -1 else 1
   -> 0.18s
*)
let compare (x : t) y = compare x y
let ascending = compare
let descending x y = compare y x
let equal (x : t) y = x = y
let ( >= ) (x : t) y = x >= y
let ( <= ) (x : t) y = x <= y
let ( = ) (x : t) y = x = y
let ( > ) (x : t) y = x > y
let ( < ) (x : t) y = x < y
let ( <> ) (x : t) y = x <> y

include Hashable.Make (struct
  type t = int
  let equal x y = equal x y
  let hash (x : t) = x
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
end)

include Setable.Make (struct
  type t = int
  let compare x y = compare x y
end)

let zero = 0
let one = 1
let minus_one = -1

let pred i = i - 1
let succ i = i + 1

let to_int i = i
let of_int i = i

let max_int = max_int
let min_int = min_int

module Conv = Int_conversions
let of_int32 = Conv.int32_to_int
let of_int32_exn = Conv.int32_to_int_exn
let to_int32 = Conv.int_to_int32
let to_int32_exn = Conv.int_to_int32_exn
let of_int64 = Conv.int64_to_int
let of_int64_exn = Conv.int64_to_int_exn
let to_int64 = Conv.int_to_int64
let of_nativeint = Conv.nativeint_to_int
let of_nativeint_exn = Conv.nativeint_to_int_exn
let to_nativeint = Conv.int_to_nativeint

let abs x = abs x

let (+) x y = (+) x y
let (-) x y = (-) x y
let ( * ) x y = ( * ) x y
let (/) x y = (/) x y
 
let neg x = -x

let add x y = (+) x y
let sub x y = (-) x y
let mul x y = ( * ) x y
let div x y = ( / ) x y
let rem a b = a mod b

let incr = Pervasives.incr
let decr = Pervasives.decr

let rec to_string_hum i =
  if i < 9999 then
    string_of_int i
  else
    Printf.sprintf "%s_%03i"
      (to_string_hum (i/1000))
      (i mod 1000)

let shift_right a b = a asr b
let shift_right_logical a b = a lsr b
let shift_left a b = a lsl b
(* CRv2 yminsky: If it's possible, lnot seems better than lognot, since the latter
   suggests a logarithm, which is just confusing... *)
let lognot a = lnot a
let logor a b = a lor b
let logand a b = a land b
let logxor a b = a lxor b
