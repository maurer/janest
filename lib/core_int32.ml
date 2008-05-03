(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core_int32"

open Int32

type t = int32 with sexp, bin_io
type t_ = t

type floatable = t
type stringable = t
type sexpable = t
type binable = t

let float_of_bits = float_of_bits
let bits_of_float = bits_of_float
let shift_right_logical = shift_right_logical
let shift_right = shift_right
let shift_left = shift_left
let lognot = lognot
let logxor = logxor
let logor = logor
let logand = logand
let min_int = min_int
let max_int = max_int
let abs = abs
let pred = pred
let succ = succ
let sub = sub
let rem = rem
let neg = neg
let mul = mul
let div = div
let add = add
let minus_one = minus_one
let one = one
let zero = zero
let compare = compare
let to_float = to_float
let of_float = of_float
let to_string = to_string
let of_string = of_string

type comparable = t
let ascending = compare
let descending x y = compare y x
let min (x : t) y = if x < y then x else y
let max (x : t) y = if x > y then x else y
let equal (x : t) y = x = y
let ( >= ) (x : t) y = x >= y
let ( <= ) (x : t) y = x <= y
let ( = ) (x : t) y = x = y
let ( > ) (x : t) y = x > y
let ( < ) (x : t) y = x < y
let ( <> ) (x : t) y = x <> y

include Hashable.Make (struct
  type t = t_
  let equal = equal
  let hash (x : t) = Hashtbl.hash x
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
end)

include Setable.Make (struct
  type t = t_
  let compare = compare
end)

let ( / ) = div
let ( * ) = mul
let ( - ) = sub
let ( + ) = add

let incr r = r := !r + one
let decr r = r := !r - one

let of_int32 t = t
let to_int32 t = t

module Conv = Int_conversions
let of_int = Conv.int_to_int32
let of_int_exn = Conv.int_to_int32_exn
let to_int = Conv.int32_to_int
let to_int_exn = Conv.int32_to_int_exn
let of_int64 = Conv.int64_to_int32
let of_int64_exn = Conv.int64_to_int32_exn
let to_int64 = Conv.int32_to_int64
let of_nativeint = Conv.nativeint_to_int32
let of_nativeint_exn = Conv.nativeint_to_int32_exn
let to_nativeint = Conv.int32_to_nativeint
