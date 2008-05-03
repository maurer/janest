(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Month"

module Array = Core_array
module Int = Core_int
module String = Core_string

let failwithf = Core_printf.failwithf

let num_months = 12

type t = int

let invariant t =
  assert (0 <= t && t < num_months);
;;

let of_int i =
  if 1 <= i && i <= num_months then
    Some (i - 1)
  else
    None
;;

let of_int_exn i =
  if 1 <= i && i <= num_months then
    i - 1
  else
    failwithf "Month.of_int_exn %d" i ()
;;

let to_int t = t + 1

let jan = 0
let feb = 1
let mar = 2
let apr = 3
let may = 4
let jun = 5
let jul = 6
let aug = 7
let sep = 8
let oct = 9
let nov = 10
let dec = 11

type variant = [ `Jan | `Feb | `Mar | `Apr | `May | `Jun
               | `Jul | `Aug | `Sep | `Oct | `Nov | `Dec ]

type rep = {
  string : string;
  t : t;
  variant : variant;
}

let reps =
  Array.map  
    [|("JAN", jan, `Jan);
      ("FEB", feb, `Feb);
      ("MAR", mar, `Mar);
      ("APR", apr, `Apr);
      ("MAY", may, `May);
      ("JUN", jun, `Jun);
      ("JUL", jul, `Jul);
      ("AUG", aug, `Aug);
      ("SEP", sep, `Sep);
      ("OCT", oct, `Oct);
      ("NOV", nov, `Nov);
      ("DEC", dec, `Dec)|]
    ~f:(fun (string, t, variant) ->
      { string = string; t = t; variant = variant; })
;;

type stringable = t

let to_string t = reps.(t).string

let of_string =
  let table =
    String.Table.of_alist
      (Array.to_list (Array.map reps ~f:(fun r -> (r.string, r.t))))
  in
  fun str ->
    match String.Table.find table (String.uppercase str) with
    | None -> failwithf "Invalid month: %s" str ()
    | Some x -> x
;;

include Sexpable.Of_stringable (struct
  type stringable = t
  let of_string = of_string
  let to_string = to_string
end)

let get t = reps.(t).variant

let create = function
  | `Jan -> 0
  | `Feb -> 1
  | `Mar -> 2
  | `Apr -> 3
  | `May -> 4
  | `Jun -> 5
  | `Jul -> 6
  | `Aug -> 7
  | `Sep -> 8
  | `Oct -> 9
  | `Nov -> 10
  | `Dec -> 11
;;

let shift t i = (t + i) mod num_months

include (Int : sig
  include Binable.S with type binable = t
  include Comparable.S with type comparable = t
  include Hashable.S with type hashable = t
end)
