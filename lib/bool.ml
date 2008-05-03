type t = bool

type sexpable = t
let sexp_of_t = Sexplib.Conv.sexp_of_bool
let t_of_sexp = Sexplib.Conv.bool_of_sexp

type stringable = t
let of_string = bool_of_string
let to_string = string_of_bool

type comparable = t
let min (x : t) y = if x < y then x else y
let max (x : t) y = if x > y then x else y
let compare x y = compare (x : t) y
let ascending = compare
let descending x y = compare y x 

let equal (x : t) y = x = y
let ( >= ) (x : t) y = x >= y
let ( <= ) (x : t) y = x <= y
let ( = ) (x : t) y = x = y
let ( > ) (x : t) y = x > y
let ( < ) (x : t) y = x < y
let ( <> ) (x : t) y = x <> y

(* Making bool hashable may seem frivolous, but consider an aggregate type with
   a bool in it that needs a custom hash function. *)
include Hashable.Make (struct
  type t = bool
  let equal = equal
  let hash x = if x then 1 else 0
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
end)

include Setable.Make (struct
  type t = bool
  let compare = compare
end)

let not = Pervasives.not

let of_int i =
  match i with
  | 0 -> false
  | _ -> true
;;

let to_int t = if t then 1 else 0
