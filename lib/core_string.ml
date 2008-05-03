(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core_string"

module Array = Caml.ArrayLabels
module Char = Core_char
module String = Caml.StringLabels

let invalid_argf = Core_printf.invalid_argf

type t = string with sexp, bin_io

type elt = char
  
type binable = t
type container = t
type sexpable = t
type stringable = t

(* Standard functions *)
let blit = String.blit
let capitalize = String.capitalize
let compare = String.compare
let concat = String.concat
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
let normalize =
  Ordered_collection_common.normalize ~length_fun:String.length
let slice =
  Ordered_collection_common.slice ~length_fun:String.length ~sub_fun:String.sub

let nget x i =
  x.[normalize x i]
let nset x i v =
  x.[normalize x i] <- v

let invalid_argf = Core_printf.invalid_argf

(**
   Inverse operation of [String.escaped]
*)
exception Unescape_error of bool*int*string

(* CRv2 tvaroquaux the stdlib's escaped does a lot of fancy wazoo magic to avoid
   using a buffer:
   It works in two passes, the first one calculates the length of the string to
   allocate and the second one does the actual escaping.

   This would be more cumbersome to do here but might be worth the hassle if
   performance ever gets to be an issue *)
let unescaped' ?(strict=true) s =
  let len = String.length s in
  let pos = ref 0 in
  let error ?(fatal=false) message =
    raise (Unescape_error (fatal,!pos,message))
  in
  let consume () =
    let i = !pos in
    if i = len then error "unexpectedly reached end of string";
    let c = s.[i] in
    pos := i + 1;
    c
  in
  let res = Buffer.create len in
  let emit c = Buffer.add_char res c in
  let emit_code code =
    match Char.of_int code with
    | Some c -> emit c
    | None -> error ~fatal:true
        (Printf.sprintf "got invalid escape code %d" code)
  in
  let rec loop () =
    if !pos < len then begin
      let c = consume () in
      if c <> '\\' then
        emit c
      else begin
        let mark = !pos in
        try
          let c = consume () in
          match c with
          | '\\' | '\"' -> emit c
          | 'b' -> emit '\b'
          | 'n' -> emit '\n'
          | 'r' -> emit '\r'
          | 't' -> emit '\t'
          | 'x' ->
              let c2hex c =
                if (c >= 'A') && (c <= 'Z' ) then
                  (Char.to_int c) + 10 - Char.to_int 'A'
                else if (c >= 'a') && (c <= 'z' ) then
                  (Char.to_int c) + 10 - Char.to_int 'a'
                else if (c >= '0') && (c <= '9') then
                  (Char.to_int c) - Char.to_int '0'
                else
                  error (Printf.sprintf "expected hex digit, got: %c" c);
              in
              let c1 = consume () in
              let c2 = consume () in
              emit_code (16 * c2hex c1 + c2hex c2);
          | c when Char.is_digit c ->
              let char_to_num c =
                match Char.get_digit c with
                | None -> error (Printf.sprintf "expected digit,got: %c" c);
                | Some i -> i
              in
              let i1 = char_to_num c in
              let i2 = char_to_num (consume ()) in
              let i3 = char_to_num (consume ()) in
              emit_code (100 * i1 + 10 * i2 + i3);
          | c -> error (Printf.sprintf "got invalid escape character: %c" c);
        with Unescape_error (false,_,_) when not strict ->
          emit '\\';
          pos := mark
      end;
      loop ()
    end else
      Buffer.contents res;
  in
  loop ();
;;

let unescaped ?strict s =
  try
    unescaped' ?strict s
  with Unescape_error (_,pos,message) ->
    invalid_argf "String.unescaped error at position %d of %s: %s"
      pos s message ()

let unescaped_res ?strict s =
  try
    Result.Ok (unescaped' ?strict s)
  with Unescape_error (_,pos,message) ->
    Result.Error (pos,message)

(** if [string] contains the character [char], then [split2_exn string char]
    returns a pair containing [string] split around the first appearance of
    [char] (from the left)
    @raise Not_found When [char] cannot be found in [string]
*)
let split2_exn line delim =
  let pos = String.index line delim in
  (String.sub line ~pos:0 ~len:pos,
   String.sub line ~pos:(pos+1) ~len:(String.length line - pos - 1)
  )

  (** if [string] contains the character [char], then [rsplit2_exn string char]
      returns a pair containing [string] split around the first appearance of
      [char] (from the right)
      @raise Not_found When [char] cannot be found in [string]
  *)
let rsplit2_exn line delim =
  let pos = String.rindex line delim in
  (String.sub line ~pos:0 ~len:pos,
   String.sub line ~pos:(pos+1) ~len:(String.length line - pos - 1)
  )

  (** [split2 line delim] optionally returns [line] split into two strings around the
      first appearance of [delim] from the left *)
let split2 line delim =
  try Some (split2_exn line delim) with Not_found -> None

  (** [rsplit2 line delim] optionally returns [line] split into two strings around the
      first appearance of [delim] from the right *)
let rsplit2 line delim =
  try Some (rsplit2_exn line delim) with Not_found -> None

(** Efficient string splitting *)

let split_on_char str c =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      String.sub str ~pos:0 ~len:last_pos :: acc
    else
      if str.[pos] = c then
        let pos1 = pos + 1 in
        let sub_str = String.sub str ~pos:pos1 ~len:(last_pos - pos1) in
        loop (sub_str :: acc) pos (pos - 1)
      else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)
;;

(* convinience functions, because it covers the majority of our use of the above
    function  *)
let split_on_pipe str = split_on_char str '|';;
let split_on_dot str = split_on_char str '.';;
let split_on_comma str = split_on_char str ',';;
let split_on_slash str = split_on_char str '/';;

let split_on_chars str chars =
  let len = String.length str in
  let chars = PSet.of_list chars in
  let rec loop acc current_start pos =
    let update_acc () =
      match current_start with
      | None -> acc
      | Some s -> (String.sub str ~pos:s ~len:(pos - s)) :: acc      
    in
    if pos = len then List.rev (update_acc ())
    else if PSet.mem str.[pos] chars then loop (update_acc ()) None (pos + 1)
    else
      match current_start with
      | None -> loop acc (Some pos) (pos + 1)
      | Some _ -> loop acc current_start (pos + 1)
  in
  loop [] None 0
;;



(* [check_suffix s suff] returns [true] if the string [s] ends with the suffix [suff] *)
let check_suffix name suff =
  let len_name = String.length name in
  let len_suff = String.length suff in
  if len_name < len_suff then false
  else
    try
      for i = 1 to len_suff do
        if suff.[len_suff - i] <> name.[len_name - i] then raise Exit
      done;
      true
    with Exit -> false

(* CRv2 ogunden: why is this different in style than check_suffix?  It would be easier
   to think about if we decided on a style and stuck with it, here. *)
(* sweeks: I agree, but am v2'ing this since it's not a bug.  The code for
   [check_prefix] has been reformatted in main, and I think we should change
   [check_suffix] there to match.
*)
let check_prefix s pref =
  let len_pref = String.length pref in
  String.length s >= len_pref
  && (let rec loop i =
        i = len_pref || (pref.[i] = s.[i] && loop (i + 1)) 
      in
      loop 0)
;;

let drop_prefix_n t n = sub t ~pos:n ~len:(length t - n)
let drop_suffix_n t n = sub t ~pos:0 ~len:(length t - n)
let keep_prefix_n t n = sub t ~pos:0 ~len:n
let keep_suffix_n t n = sub t ~pos:(length t - n) ~len:n

let findi t ~f =
  let n = length t in
  let rec loop i =
    if i = n then None
    else if f i t.[i] then Some i
    else loop (i + 1)
  in
  loop 0
;;

let find t ~f = Option.map (findi t ~f:(fun _ c -> f c)) ~f:(fun i -> t.[i])

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
  | Some i -> keep_prefix_n t (i + 1)
;;

let first_non_whitespace t = findi t ~f:(fun _ c -> not (Char.is_whitespace c))

let lstrip t =
  match first_non_whitespace t with
  | None -> ""
  | Some n -> drop_prefix_n t n
;;

let strip t =
  match first_non_whitespace t with
  | None -> ""
  | Some first ->
      match last_non_whitespace t with
      | None -> assert false
      | Some last -> String.sub t ~pos:first ~len:(last - first + 1)
;;

let map ~f s =
  let s' = String.create (String.length s) in
  for i = 0 to String.length s - 1 do
    s'.[i] <- f s.[i]
  done;
  s'

let to_array s = Array.init (String.length s) ~f:(fun i -> s.[i])

let to_list t =
  let rec loop i ac =
    if i = 0 then ac
    else
      let i = i - 1 in
      loop i (t.[i] :: ac)
  in
  loop (length t) []
;;

let tr ~target ~replacement = map ~f:(fun c -> if c = target then replacement else c)

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

let is_empty t = 0 = String.length t

let concat_array ~sep ar = String.concat ~sep (Array.to_list ar)

let translate ~f s = concat_array ~sep:"" (Array.map (to_array s) ~f)

(*
  CRv2 tvaroquaux should we name those `drop_*` and not follow the standard of
  filename.ml?
*)
let chop_prefix s pref =
  if not (check_prefix s pref) then
    raise (Invalid_argument
             (Printf.sprintf "Core_string.chop_prefix %s %s" s pref));
  drop_prefix_n s (String.length pref)

let chop_suffix s suff =
  if not (check_suffix s suff) then
    raise (Invalid_argument
             (Printf.sprintf "Core_string.chop_suffix %s %s" s suff));
  drop_suffix_n s (String.length suff)

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

(* = on two string avoids calling compare_val, which is what happens
   with String.compare *)
(* CRv2 | PZ: with OCaml 3.10, we can remove the physical equality test, as it is now also
   checked in the standard library *)
let equal (x : string) y = x == y || x = y


module Infix = struct
  let ( </> ) str (start,stop) = slice str start stop
end


include Hashable.Make (struct
  type t = string
  let hash = hash
  let equal = equal
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
end)

include Setable.Make (struct
  type t = string
  let compare = String.compare
end)

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
(* let equal x y = (x : t) = y *) (* already provided in String *)
let ( >= ) x y = (x : t) >= y
let ( <= ) x y = (x : t) <= y
let ( = ) x y = (x : t) = y
let ( > ) x y = (x : t) > y
let ( < ) x y = (x : t) < y
let ( <> ) x y = (x : t) <> y

let of_char c = String.make 1 c
