(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core_string"




module Array = Caml.ArrayLabels
module Char = Core_char
module String = Caml.StringLabels

let phys_equal = Caml.(==)

let invalid_argf = Core_printf.invalid_argf

module T = struct
  type t = string with sexp, bin_io

  type binable = t
  type sexpable = t

  let compare = String.compare
  (* = on two strings avoids calling compare_val, which is what happens
     with String.compare *)
  let equal (x : string) y = x = y
end

include T

type elt = char

type container = t
type stringable = t

(* Standard functions *)
let blit = String.blit
let capitalize = String.capitalize
let compare = String.compare
let concat ?(sep="") l = String.concat ~sep l
let contains = String.contains
let contains_from = String.contains_from
let copy = String.copy
let escaped = String.escaped
let fill = String.fill
let index = String.index
let index_from = String.index_from
let length = String.length
let lowercase = String.lowercase
let make = String.make
let rcontains_from = String.rcontains_from
let rindex = String.rindex
let rindex_from = String.rindex_from
let sub = String.sub
let uncapitalize = String.uncapitalize
let uppercase = String.uppercase
external create : int -> string = "caml_create_string"
external get : string -> int -> char = "%string_safe_get"
external length : string -> int = "%string_length"
external set : string -> int -> char -> unit = "%string_safe_set"

let id x = x
let of_string = id
let to_string = id

let iter t ~f = String.iter t ~f

let init n ~f =
  if n < 0 then invalid_argf "String.init %d" n ();
  let t = create n in
  for i = 0 to n - 1 do
    t.[i] <- f i;
  done;
  t
;;

(** See {!Core_array.normalize} for the following 4 functions. *)
let normalize s i =
  Ordered_collection_common.normalize ~length_fun:String.length s i
let slice start stop s =
  Ordered_collection_common.slice ~length_fun:String.length ~sub_fun:String.sub
  start stop s

let nget x i =
  x.[normalize x i]
let nset x i v =
  x.[normalize x i] <- v

let invalid_argf = Core_printf.invalid_argf

let to_list s =
  let rec loop acc i =
    if i < 0 then
      acc
    else
      loop (s.[i] :: acc) (i-1)
  in
  loop [] (String.length s - 1)

let to_list_rev s =
  let len = String.length s in
  let rec loop acc i =
    if i = len then
      acc
    else
      loop (s.[i] :: acc) (i+1)
  in
  loop [] 0

(** Efficient string splitting *)

let lsplit2_exn line ~on:delim =
  let pos = String.index line delim in
  (String.sub line ~pos:0 ~len:pos,
   String.sub line ~pos:(pos+1) ~len:(String.length line - pos - 1)
  )

let rsplit2_exn line ~on:delim =
  let pos = String.rindex line delim in
  (String.sub line ~pos:0 ~len:pos,
   String.sub line ~pos:(pos+1) ~len:(String.length line - pos - 1)
  )

let lsplit2 line ~on =
  try Some (lsplit2_exn line ~on) with Not_found -> None

let rsplit2 line ~on =
  try Some (rsplit2_exn line ~on) with Not_found -> None

let split_gen str ~on =
  let is_delim on c =
    match on with
    | `char c' -> c = c'
    | `chars s -> Char.Set.mem s c
  in
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      String.sub str ~pos:0 ~len:last_pos :: acc
    else
      if is_delim on str.[pos] then
        let pos1 = pos + 1 in
        let sub_str = String.sub str ~pos:pos1 ~len:(last_pos - pos1) in
        loop (sub_str :: acc) pos (pos - 1)
      else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)
;;

let split str ~on = split_gen str ~on:(`char on) ;;

let split_on_chars str ~on:chars =
  
  let chars = Char.Set.of_list chars in
  split_gen str ~on:(`chars chars)
;;

(* [is_suffix s ~suff] returns [true] if the string [s] ends with the suffix [suff] *)
let is_suffix s ~suffix =
  let len_suff = String.length suffix in
  let len_s = String.length s in
  len_s >= len_suff
  && (let rec loop i =
        i = len_suff || (suffix.[len_suff - 1 - i] = s.[len_s - 1 - i] && loop (i + 1))
      in
      loop 0)

let is_prefix s ~prefix =
  let len_pref = String.length prefix in
  String.length s >= len_pref
  && (let rec loop i =
        i = len_pref || (prefix.[i] = s.[i] && loop (i + 1))
      in
      loop 0)
;;

let wrap_sub_n t n ~name ~pos ~len ~on_error =
  if n < 0 then
    raise (Invalid_argument (name ^ " expecting nonnegative argument"))
  else
    try
      sub t ~pos ~len
    with _ ->
      on_error
      
let drop_prefix t n = wrap_sub_n ~name:"drop_prefix" t n ~pos:n ~len:(length t - n) ~on_error:""
let drop_suffix t n = wrap_sub_n ~name:"drop_suffix" t n ~pos:0 ~len:(length t - n) ~on_error:""
let prefix t n = wrap_sub_n ~name:"prefix" t n ~pos:0 ~len:n ~on_error:t
let suffix t n = wrap_sub_n ~name:"suffix" t n ~pos:(length t - n) ~len:n ~on_error:t

let lfindi t ~f =
  let n = length t in
  let rec loop i =
    if i = n then None
    else if f i t.[i] then Some i
    else loop (i + 1)
  in
  loop 0
;;

let find t ~f =
  match lfindi t ~f:(fun _ c -> f c) with
  | None -> None | Some i -> Some t.[i]

let rfindi t ~f =
  let rec loop i =
    if i = 0 then None
    else begin
      let i = i - 1 in
      if f i t.[i] then Some i
      else loop i
    end
  in
  loop (length t)
;;

let last_non_whitespace t = rfindi t ~f:(fun _ c -> not (Char.is_whitespace c))

let rstrip t =
  match last_non_whitespace t with
  | None -> ""
  | Some i ->
      if i = length t - 1
      then t
      else prefix t (i + 1)
;;

let first_non_whitespace t = lfindi t ~f:(fun _ c -> not (Char.is_whitespace c))

let lstrip t =
  match first_non_whitespace t with
  | None -> ""
  | Some 0 -> t
  | Some n -> drop_prefix t n
;;

(* [strip t] could be implemented as [lstrip (rstrip t)].  The implementatiom
   below saves (at least) a factor of two allocation, by only allocating the
   final result.  This also saves some amount of time. *)
let strip t =
  let length = length t in
  if length = 0
    || not (Char.is_whitespace t.[0] || Char.is_whitespace t.[length - 1])
  then t
  else
    match first_non_whitespace t with
    | None -> ""
    | Some first ->
        match last_non_whitespace t with
        | None -> assert false
        | Some last -> sub t ~pos:first ~len:(last - first + 1)
;;

let map ~f s =
  let l = String.length s in
  let s' = String.create l in
  for i = 0 to l - 1 do
    s'.[i] <- f s.[i]
  done;
  s'

let to_array s = Array.init (String.length s) ~f:(fun i -> s.[i])

let tr ~target ~replacement s = map ~f:(fun c -> if c = target then replacement else c) s

let tr_inplace ~target ~replacement s = (* destructive version of tr *)
  for i = 0 to String.length s - 1 do
    if s.[i] = target then s.[i] <- replacement
  done

let exists s ~f =
  let rec loop i = i > 0 && (let i = i - 1 in f s.[i] || loop i) in
  loop (length s)
;;

let for_all s ~f =
  let rec loop i = i = 0 || (let i = i - 1 in f s.[i] && loop i) in
  loop (length s)
;;

let fold t ~init ~f =
  let n = length t in
  let rec loop i ac = if i = n then ac else loop (i + 1) (f ac t.[i]) in
  loop 0 init
;;

let is_empty t =
  String.length t = 0
;;

let concat_array ?sep ar = concat ?sep (Array.to_list ar)

(* Maybe we'll add this later ... *)
(* let tc_concat tc ~sep c = *)
(*   let module C = Container in *)
(*   if tc.C.is_empty c then "" *)
(*   else begin *)
(*     let sep_len = String.length sep in *)
(*     let len = *)
(*       tc.C.fold ~init:(- sep_len) c ~f:(fun len s -> *)
(*         len + length s + sep_len) *)
(*     in *)
(*     let dst = create len in *)
(*     ignore *)
(*       (tc.C.fold ~init:0 c ~f:(fun dst_pos src -> *)
(*         let dst_pos = *)
(*           if dst_pos = 0 then 0 *)
(*           else begin *)
(*             blit ~src:sep ~src_pos:0 ~dst_pos ~len:sep_len ~dst; *)
(*             dst_pos + sep_len *)
(*           end *)
(*         in *)
(*         let src_len = String.length src in *)
(*         blit ~src ~src_pos:0 ~len:src_len ~dst_pos ~dst; *)
(*         dst_pos + src_len)); *)
(*     dst *)
(*   end *)

 let concat_map ?sep s ~f = concat_array ?sep (Array.map (to_array s) ~f)

let chop_prefix_opt s ~prefix =
  if is_prefix s ~prefix then
    Some (drop_prefix s (String.length prefix))
  else
    None

let chop_prefix s ~prefix =
  match chop_prefix_opt s ~prefix with
  | Some str -> str
  | None ->
      raise (Invalid_argument
               (Printf.sprintf "Core_string.chop_prefix %S %S" s prefix))

let chop_suffix_opt s ~suffix =
  if is_suffix s ~suffix then
    Some (drop_suffix s (String.length suffix))
  else
    None

let chop_suffix s ~suffix =
  match chop_suffix_opt s ~suffix with
  | Some str -> str
  | None ->
      raise (Invalid_argument
               (Printf.sprintf "Core_string.chop_suffix %S %S" s suffix))

(* The following function returns exactly the same results than
   the standard hash function on strings (it performs exactly the
   same computation), but it is faster on short strings (because we
   don't have to call the generic C function). For random strings
   of length 4 to 6 (typical rics), it is 40% faster. For strings
   of length 30 or more, the standard hash function is faster.
*)
let hash s =
  let len = String.length s in
  if len = 0 then 0
  else if len > 30 then Hashtbl.hash_param 1 1 s
  else
    let res = ref (int_of_char (String.unsafe_get s 0)) in
    for i = 1 to len - 1 do
      res := !res * 19 + int_of_char (String.unsafe_get s i)
    done;
    !res land 0x3FFFFFFF

module Infix = struct
  let ( </> ) str (start,stop) = slice str start stop
end

include Hashable.Make_binable (struct
  include T
  let hash = hash
end)
module Map = Core_map.Make (T)
module Set = Core_set.Make (T)

(* for interactive top-levels -- modules deriving from String should have String's pretty
   printer. *)
let pp ppf s = Format.fprintf ppf "%s" s

(* fast version, if we ever need it:
  let concat_array ~sep ar =
  let ar_len = Array.length ar in
  if ar_len = 0 then ""
  else
    let sep_len = String.length sep in
    let res_len_ref = ref (sep_len * (ar_len - 1)) in
    for i = 0 to ar_len - 1 do
      res_len_ref := !res_len_ref + String.length ar.(i)
    done;
    let res = String.create !res_len_ref in
    let str_0 = ar.(0) in
    let len_0 = String.length str_0 in
    String.blit ~src:str_0 ~src_pos:0 ~dst:res ~dst_pos:0 ~len:len_0;
    let pos_ref = ref len_0 in
    for i = 1 to ar_len - 1 do
      let pos = !pos_ref in
      String.blit ~src:sep ~src_pos:0 ~dst:res ~dst_pos:pos ~len:sep_len;
      let new_pos = pos + sep_len in
      let str_i = ar.(i) in
      let len_i = String.length str_i in
      String.blit ~src:str_i ~src_pos:0 ~dst:res ~dst_pos:new_pos ~len:len_i;
      pos_ref := new_pos + len_i
    done;
    res
  *)

type comparable = t
let min (x : t) y = if x < y then x else y
let max (x : t) y = if x > y then x else y
let compare (x : t) y = compare x y
let ascending = compare
let descending x y = compare y x
let ( >= ) x y = (x : t) >= y
let ( <= ) x y = (x : t) <= y
let ( = ) x y = (x : t) = y
let ( > ) x y = (x : t) > y
let ( < ) x y = (x : t) < y
let ( <> ) x y = (x : t) <> y

let of_char c = String.make 1 c

let container = {
  Container.
  length = length;
  is_empty = is_empty;
  iter = iter;
  fold = fold;
  exists = exists;
  for_all = for_all;
  find = find;
  to_list = to_list;
  to_array = to_array;
}
