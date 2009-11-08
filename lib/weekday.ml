(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Weekday"

module Array = Core_array
module Int = Core_int
module String = Core_string

let failwithf = Core_printf.failwithf

let num_days = 7

type t = int

let invariant t =
  assert (0 <= t && t < num_days);
;;

let of_int i =
  if 0 <= i && i < num_days then
    Some i
  else
    None
;;

let of_int_exn i =
  if 0 <= i && i < num_days then
    i
  else
    failwithf "Weekday.of_int_exn %d" i ()
;;

let to_int t = t

let sun = 0
let mon = 1
let tue = 2
let wed = 3
let thu = 4
let fri = 5
let sat = 6

type variant = [ `Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat ]

type rep = {
  string : string;
  t : t;
  variant : variant;
}

let reps =
  Array.map
    [|("SUN", sun, `Sun);
      ("MON", mon, `Mon);
      ("TUE", tue, `Tue);
      ("WED", wed, `Wed);
      ("THU", thu, `Thu);
      ("FRI", fri, `Fri);
      ("SAT", sat, `Sat)|]
    ~f:(fun (string, t, variant) ->
      { string = string; t = t; variant = variant; })
;;

type stringable = t

let to_string t = reps.(t).string

let of_string =
  let table =
    String.Table.of_alist_exn
      (Array.to_list (Array.map reps ~f:(fun r -> (r.string, r.t))))
  in
  fun str ->
    match String.Table.find table (String.uppercase str) with
    | None -> failwithf "Invalid weekday: %s" str ()
    | Some x -> x
;;

include Sexpable.Of_stringable (struct
  type stringable = t
  let of_string = of_string
  let to_string = to_string
end)

let get t = reps.(t).variant

let create = function
  | `Sun -> 0
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 4
  | `Fri -> 5
  | `Sat -> 6
;;

let shift t i = (t + i) mod num_days

let is_sun_or_sat t =
  t = 0 || t = 6

include (Int : sig
  include Binable.S with type binable = t
  include Hashable.S with type hashable = t
end)
