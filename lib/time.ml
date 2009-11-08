(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Time"

open Std_internal

module type Constrained_float = sig
  type t
  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Comparable with type comparable = t
  include Hashable with type hashable = t
  include Robustly_comparable with type robustly_comparable = t
  include Stringable with type stringable = t
  include Floatable with type floatable = t
  val max_value : t
  val min_value : t
end


(**
 * XXX disgusting hack! XXX
 * round to some arbitrary level of precision because when we print we're going
 * to floor eventually.
 *  So e.g. 6.045 might turn into * 0.0449999999999aosneuthsaoenuth
 * after a modf--this will then get printed as 6.044 which is DAMN WRONG.
 **)
let round x = floor (x +. 0.5)
let canonical_multiplier = 1_000_000.
let canonicalize x = round (x *. canonical_multiplier) /. canonical_multiplier




module Span = struct
  
  module Parts = struct
    type t = {
        hr: int;
        min: int;
        sec: int;
        ms: int
      }
  end

  module Z = struct
    (* this prevents any worry about having these very common names redefined below and
      makes their usage within this module safer.  Constant is included at the very bottom
      to re-export these constants in a more convenient way *)
    module Constant = struct
      (* spans are stored as a float in seconds *)
      let millisecond = 1. /. 1000.
      let second      = 1.
      let minute      = 60.
      let hour        = float (60 * 60)
      let day         = float (24 * 60 * 60)
    end

    let to_ms x    = x /. Constant.millisecond
    let to_sec     = ident
    let to_min x   = x /. Constant.minute
    let to_hr x    = x /. Constant.hour
    let to_day x   = x /. Constant.day

    let of_ms x    = x *. Constant.millisecond
    let of_sec x   = x
    let of_int_sec = float_of_int
    let of_min x   = x *. Constant.minute
    let of_hr x    = x *. Constant.hour
    let of_day x   = x *. Constant.day

    let of_string s =
      try
        if String.is_empty s then failwith "empty string"
        else
          let float n = float_of_string (String.drop_suffix s n) in
          match s.[String.length s - 1] with
          | 's' ->
              if String.length s = 1 then failwith "no number given"
              else if s.[String.length s - 2] = 'm' then of_ms (float 2)
              else float 1
          | 'm' -> of_min (float 1)
          | 'h' -> of_hr (float 1)
          | 'd' -> of_day (float 1)
          | _ -> float 0  (* default case, treat as seconds *)
      with exn ->
        invalid_argf "Time.Span.of_string could not parse '%s': %s"
          s (Exn.to_string exn) ()
    ;;

    let t_of_sexp = function
      | Sexp.Atom x -> of_string x
      | Sexp.List _ as x ->
          Sexplib.Conv.of_sexp_error "Time.Span.of_sexp: expected Atom" x

    let to_string t =
      match classify_float t with
      | FP_subnormal | FP_zero -> "0s"
      | FP_infinite -> if t > 0. then "inf" else "-inf"
      | FP_nan -> "nan"
      | FP_normal ->
          let abs_t = abs_float t in
          if abs_t < Constant.second then sprintf "%gms" (to_ms t)
          else if abs_t < Constant.minute then sprintf "%gs" (to_sec t)
          else if abs_t < Constant.hour then sprintf "%gm" (to_min t)
          else if abs_t < Constant.day then sprintf "%gh" (to_hr t)
          else sprintf "%gd" (to_day t)

    let to_string_hum t =
      match classify_float t with
      | FP_subnormal | FP_zero -> "0:00:00.000"
      | FP_infinite -> if t > 0. then "inf" else "-inf"
      | FP_nan -> "nan"
      | FP_normal ->
          
          let sign, absval = if t < 0. then "-", (-. t) else "", t in
          let intval  = int_of_float absval in
          let minutes = intval / 60 in
          let seconds = absval -. (float (60 * minutes)) in
          let hours   = minutes / 60 in
          let minutes = minutes mod 60 in
          sprintf "%s%d:%02d:%06.3f" sign hours minutes seconds

    let sexp_of_t t = Sexp.Atom (to_string t)

    let pp ppf t = Format.fprintf ppf "%s" (to_string t)
    let () = Pretty_printer.register "Core.Time.Span.pp"
  end

  let to_parts t =
    let intval = Float.truncate (Float.abs t) in
    let min = intval / 60 in
    let sec = intval mod 60 in
    let hr  = min / 60 in
    let min = min mod 60 in
    let ms  = Float.truncate ((t -. (float intval)) *. 1000.) in
    {Parts.
      hr = hr;
      min = min;
      sec = sec;
      ms = ms
    }

  include (Float : sig
    include Constrained_float with type t = float
    val add : t -> t -> t
    val sub : t -> t -> t
    val zero : t
    val epsilon : t
    val abs : t -> t
    val scale : t -> float -> t
    val (/) : t -> t -> float
  end)
  include Z

  let randomize t ~percent =
    let upperbound = percent *. t in
    let distance = Random.float (2. *. upperbound) -. upperbound in
    t +. distance
  ;;

  let create ?(day = 0) ?(hr = 0) ?(min = 0) ?(sec = 0) ?(ms = 0) () =
    of_day (float day)
    +. of_hr (float hr)
    +. of_min (float min)
    +. of_sec (float sec)
    +. of_ms (float ms)
  ;;

  include Z.Constant
end

let parse_two_digits str pos =
  let d1 = Char.get_digit_exn str.[pos] in
  let d2 = Char.get_digit_exn str.[pos + 1] in
  10 * d1 + d2

let parse_four_digits str pos =
  parse_two_digits str pos * 100 + parse_two_digits str (pos + 2)

module Ofday = struct
  (* Create an abstract type for Ofday to prevent us from confusing it with
     other floats.
  *)
  module Ofday : sig
    include Constrained_float

    val of_sec : float -> t
    val to_sec : t -> float
    val of_span_since_midnight : Span.t -> t
    val to_span_since_midnight : t -> Span.t
    val start_of_day : t
    val end_of_day : t
    val add : t -> Span.t -> t option
    val sub : t -> Span.t -> t option
  end = struct
    (* Number of seconds since midnight. *)
    
    include Float

    let to_sec = ident

    
    let of_sec s =
      match classify_float s with
      | FP_infinite -> invalid_arg "Time.Ofday.of_sec: infinite value"
      | FP_nan -> invalid_arg "Time.Ofday.of_sec: NaN value"
      | FP_normal | FP_subnormal | FP_zero ->
          if s >= Span.day || s < Span.zero
          then invalid_argf "Time.Ofday.of_sec: out of range: %f" s ()
          else canonicalize s

    let to_span_since_midnight = ident
    let of_span_since_midnight = of_sec

    let start_of_day = 0.
    let end_of_day = Span.day - epsilon

    let is_valid t = 0. <= t && t < Span.day

    let add t span =
      let t = t +. span in
      if is_valid t then Some t else None
    ;;

    let sub t span =
      let t = t -. span in
      if is_valid t then Some t else None
    ;;
  end

  module Z = struct
    let create ?hr ?min ?sec ?ms () =
      Ofday.of_span_since_midnight (Span.create ?hr ?min ?sec ?ms ())
    ;;

    let to_parts t = Span.to_parts (Ofday.to_span_since_midnight t)

    let of_sec = Ofday.of_sec

    
    let to_string_gen ~drop_ms ~trim x =
      let x = Ofday.to_sec x in
      
      if x = neg_infinity then "mintime"
      else if x = infinity then "maxtime"
      else
        let tot_ms = int_of_float (x *. 1000. +. 1.E-4) in
        let ms = tot_ms mod 1000 in
        let tot_s = tot_ms / 1000 in
        let s = tot_s mod 60 in
        let tot_m = tot_s / 60 in
        let m = tot_m mod 60 in
        let h = tot_m / 60 in
        sprintf "%02d:%02d%s" h m
          (if (trim && ms = 0) || drop_ms then
            if s = 0 && trim then "" else sprintf ":%02d" s
           else sprintf ":%02d.%03d" s ms)

    let to_string t = to_string_gen ~drop_ms:false ~trim:false t

    let to_string_trimmed t = to_string_gen ~drop_ms:false ~trim:true t

    let to_sec_string t =  to_string_gen ~drop_ms:true ~trim:false t

    
    let of_string_iso8601_extended ?pos ?len str =
      let (pos, len) =
        match (Ordered_collection_common.get_pos_len ?pos ?len
                  ~length:(String.length str))
        with
        | Result.Ok z -> z
        | Result.Error s ->
            failwithf "Time.Ofday.of_string_iso8601_extended: %s" s ()
      in
      try
        if len < 2 then failwith "len < 2"
        else
          Ofday.of_span_since_midnight
            (
              let hour = parse_two_digits str pos in
              if hour > 24 then failwith "hour > 24";
              let span = Span.of_hr (float hour) in
              if len = 2 then span
              else if len < 5 then failwith "2 < len < 5"
              else if str.[pos + 2] <> ':' then failwith "first colon missing"
              else
                let minute = parse_two_digits str (pos + 3) in
                if minute >= 60 then failwith "minute > 60";
                let span = Span.add span (Span.of_min (float minute)) in
                if hour = 24 && minute <> 0 then
                  failwith "24 hours and non-zero minute";
                if len = 5 then span
                else if len < 8 then failwith "5 < len < 8"
                else if str.[pos + 5] <> ':' then failwith "second colon missing"
                else
                  let second = parse_two_digits str (pos + 6) in
                  if second >= 60 then failwith "second > 60";
                  let span = Span.add span (Span.of_sec (float second)) in
                  if hour = 24 && second <> 0 then
                    failwith "24 hours and non-zero seconds";
                  if len = 8 then span
                  else if len = 9 then failwith "length = 9"
                  else
                    match str.[pos + 8] with
                    | '.' | ',' ->
                        let last = pos + len - 1 in
                        let rec loop pos subs =
                          let subs = subs * 10 + Char.get_digit_exn str.[pos] in
                          if pos = last then subs else loop (pos + 1) subs
                        in
                        let subs = loop (pos + 9) 0 in
                        if hour = 24 && subs <> 0 then
                          failwith "24 hours and non-zero subseconds"
                        else
                          span +. float subs /. (10. ** float (len - 9))
                    | _ -> failwith "missing subsecond separator"
            )
      with exn ->
        invalid_argf "Time.Ofday.of_string_iso8601_extended(%s): %s"
          (String.sub str ~pos ~len) (Exn.to_string exn) ()
    ;;

    let of_string s =
      try
        
        if s = "mintime" then Ofday.min_value
        else if s = "maxtime" then Ofday.min_value
        else
          let create h m s =
            of_sec
              (float (int_of_string h * 60 * 60 + int_of_string m * 60) +. s)
          in
          match String.split s ~on:':' with
          | [h; m; s] -> create h m (float_of_string s)
          | [h; m] -> create h m 0.
          | [hm] ->
              if String.length hm = 4 then
                create
                  (String.sub hm ~pos:0 ~len:2) (String.sub hm ~pos:2 ~len:2) 0.
              else failwith "No colon, expected string of length four"
          | _ -> failwith "More than two colons"
      with exn ->
        invalid_argf "Time.Ofday.of_string (%s): %s" s (Exn.to_string exn) ()
    ;;

    let t_of_sexp sexp =
      match sexp with
      | Sexp.Atom s ->
          (try of_string s
          with Invalid_argument s ->
            of_sexp_error ("Time.Ofday.t_of_sexp: " ^ s) sexp)
      | _ -> of_sexp_error "Time.Ofday.t_of_sexp" sexp

    let sexp_of_t span = Sexp.Atom (to_string span)

    let of_int_sec s = of_sec (float s)
    let of_int_ms ms = of_sec ((float ms) /. 1000.)
    let of_sec_ms sec ms = of_sec ((float sec) +. (float ms) /. 1000.)

    let hour = 3600.
    let small_diff ofday1 ofday2 =
      let ofday1 = Ofday.to_sec ofday1 in
      let ofday2 = Ofday.to_sec ofday2 in
      let diff = ofday1 -. ofday2 in
      (* if one of [ofdayN] is either [(neg_)infinity] or [nan], so is [diff] *)
      match classify_float diff with
      | FP_infinite | FP_nan -> None
      | _ ->
          (*  d1 is in (-hour; hour) *)
          let d1 = mod_float diff hour in
          (*  d2 is in (0;hour) *)
          let d2 = mod_float (d1 +. hour) hour in
          let d = if d2 > hour /. 2. then d2 -. hour else d2 in
          Some d

    let pp ppf t = Format.fprintf ppf "%s" (to_string t)
    let () = Pretty_printer.register "Core.Time.Ofday.pp"
  end

  include Ofday
  include Z
end

(* Create an abstract type for Time to prevent us from confusing it with
   other floats.
*)
module Time : sig
  include Constrained_float
  val add : t -> Span.t -> t
  val sub : t -> Span.t -> t
  val diff : t -> t -> Span.t
  val abs_diff : t -> t -> Span.t
  val now : unit -> t
end = struct
  include Float
  let diff t1 t2 = t1 - t2
  
  let abs_diff t1 t2 = Span.abs (diff t1 t2)
  let now () = Unix.gettimeofday ()
end

module Date = struct
  (* Create a local private date type to ensure that all dates are created via
     Date.create.
  *)
  module T : sig
    type t = private { y: int; m: Month.t; d: int; }
    include Binable with type binable = t
    val create : y:int -> m:Month.t -> d:int -> t
    val min_value : t
    val max_value : t
  end = struct
    type t = { y: int; m: Month.t; d: int; } with bin_io
    type binable = t

    
    let min_value = { y =     0; m = Month.jan; d = 1; }
    let max_value = { y = 3_000; m = Month.jan; d = 1; }

    let is_leap_year year =
      (year mod 4 = 0 && not (year mod 100 = 0))
      || year mod 400 = 0
    ;;

    let create ~y:year ~m:month ~d:day =
      let invalid msg =
        invalid_argf "Date.create ~y:%d ~m:%s ~d:%d error: %s"
          year (Month.to_string month) day msg ()
      in
      if day = 0 && Month.equal month Month.jan then begin
        (* A hack to allow us to handle our old min_value and max_value, which
           used invalid dates. *)
        
        if year = 0 then min_value
        else if year = 3_000 then max_value
        else invalid "day = 0 && month = 0 && year <> 0 && year <> 3_000"
      end else begin
        if day <= 0 then invalid "day <= 0";
        begin match Month.get month with
        | `Apr | `Jun | `Sep | `Nov ->
            if day > 30 then invalid "30 day month violation"
        | `Feb ->
            if is_leap_year year then begin
              if day > 29 then invalid "29 day month violation" else ()
            end else if day > 28 then begin
              invalid "28 day month violation"
            end else ()
        | `Jan | `Mar | `May | `Jul | `Aug | `Oct | `Dec ->
            if day > 31 then invalid "31 day month violation"
        end;
        { y = year; m = month; d = day; }
      end
    ;;
  end

  include T

  type stringable = t

  let to_string_iso8601_extended t = (* YYYY-MM-DD *)
    sprintf "%04d-%02d-%02d" t.y (Month.to_int t.m) t.d

  let to_string = to_string_iso8601_extended

  let to_string_iso8601_basic t = (* YYYYMMDD *)
    sprintf "%04d%02d%02d" t.y (Month.to_int t.m) t.d

  let to_string_old t = (* MM/DD/YYYY *)
    sprintf "%02d/%02d/%04d" (Month.to_int t.m) t.d t.y

  let parse_year4 str pos = parse_four_digits str pos

  let parse_month str pos = Month.of_int_exn (parse_two_digits str pos)

  let parse_day str pos = parse_two_digits str pos

  let of_string_iso8601_basic str ~pos = (* YYYYMMDD *)
    if pos + 8 > String.length str then
      invalid_arg "of_string_iso8601_basic: pos + 8 > string length";
    create
      ~y:(parse_year4 str pos)
      ~m:(parse_month str (pos + 4))
      ~d:(parse_day str (pos + 6))
  ;;

  let of_string s =
    let invalid () = failwith "invalid" in
    let ensure b = if not b then invalid () in
    let month_num ~year ~month ~day =
      create
        ~y:(parse_year4 s year)
        ~m:(parse_month s month)
        ~d:(parse_day s day)
    in
    let month_abrv ~year ~month ~day =
      create
        ~y:(parse_year4 s year)
        ~m:(Month.of_string (String.sub s ~pos:month ~len:3))
        ~d:(parse_day s day)
    in
    if String.contains s '/' then begin
      (* m/d/y *)
      match String.split s ~on:'/' with
      | [m; d; y] ->
          let year = Int.of_string y in
          let year =
            if year >= 100 then year
            else if year < 75 then 2000 + year
            else 1900 + year
          in
          let month = Month.of_int_exn (Int.of_string m) in
          let day = Int.of_string d in
          create ~y:year ~m:month ~d:day
      | _ -> invalid ()
    end else if String.contains s '-' then begin
      (* yyyy-mm-dd *)
      ensure (String.length s = 10 && s.[4] = '-' && s.[7] = '-');
      month_num ~year:0 ~month:5 ~day:8;
    end else if String.contains s ' ' then begin
      (* DD MMM YYYY *)
      ensure (String.length s = 11 && s.[2] = ' ' && s.[6] = ' ');
      month_abrv ~day:0 ~month:3 ~year:7;
    end else if String.length s = 9 then begin
      (* DDMMMYYYY *)
      month_abrv ~day:0 ~month:2 ~year:5;
    end else if String.length s = 8 then begin
      (* assume YYYYMMDD *)
      month_num ~year:0 ~month:4 ~day:6
    end else invalid ()
  ;;

  let of_string s =
    try of_string s with
    | exn -> invalid_argf "Time.Date.of_string (%s): %s" s (Exn.to_string exn) ()
  ;;

  
  let sexp_of_t_style = ref `Atom_yyyy_mm_dd

  module Sexpable = struct
    type sexpable = t

    module Old_date = struct
      type t = { y: int; m: int; d: int; } with sexp

      let to_date t = T.create ~y:t.y ~m:(Month.of_int_exn t.m) ~d:t.d
      let of_date t = { y = t.T.y; m = Month.to_int t.T.m; d = t.T.d; }
    end

    let t_of_sexp = function
      | Sexp.Atom s -> of_string s
      | Sexp.List _ as sexp -> Old_date.to_date (Old_date.t_of_sexp sexp)
    ;;

    let t_of_sexp s =
      try
        t_of_sexp s
      with
      | (Sexplib.Conv.Of_sexp_error _) as exn -> raise exn
      | Invalid_argument a -> Sexplib.Conv.of_sexp_error a s

    let sexp_of_t t =
      match !sexp_of_t_style with
      | `Atom_yyyy_mm_dd -> Sexp.Atom (to_string t)
      | `List_ymd -> Old_date.sexp_of_t (Old_date.of_date t)
  end
  include Sexpable

  include Hashable.Make_binable (struct
    include T
    include Sexpable
    include Binable
    let equal (t : t) t' = t = t'
    let hash (t : t) = Hashtbl.hash t
  end)

  include Comparable.Make (struct
    type t = T.t

    include Sexpable

    let compare t1 t2 =
      let n = Int.compare t1.y t2.y in
      if n <> 0 then n
      else
        let n = Month.compare t1.m t2.m in
        if n <> 0 then n
        else Int.compare t1.d t2.d
    ;;
  end)

  let pp ppf date = Format.fprintf ppf "%s" (to_string date)
  let () = Pretty_printer.register "Core.Time.Date.pp"

  let day t = t.d
  let month t = t.m
  let year t = t.y

  let of_tm tm =
    create
      ~y:(tm.Unix.tm_year + 1900)
      ~m:(Month.of_int_exn (tm.Unix.tm_mon + 1))
      ~d:tm.Unix.tm_mday

  let of_time time =
    if Time.(=) time Time.min_value then min_value
    else if Time.(=) time Time.max_value then max_value
    else of_tm (Unix.localtime (floor (Time.to_float time)))

  let today () = of_time (Time.now ())

  (* This, and to_time_internal below, should only be used in add_days and diff in Date.
   * We need to do this here instead of using the normal Time.of_date_ofday because
   * Time.t doesn't exist at this point in time.  We use noon below to smooth out
   * floating point errors in date addition.  It's worth noting that we don't ever need
   * to worry about leap seconds in our math because epoch time assumes all days are a
   * constant length. *)
  let to_tm t =
    { Unix.
        tm_sec = 0;
        tm_min = 0;
        tm_hour = 12;
        tm_mday = t.d;
        tm_mon = Month.to_int t.m - 1;
        tm_year = t.y - 1900;
        tm_wday = 0;
        tm_yday = 0;
        tm_isdst = false;
    }

  let to_time_internal t =
    let tm_date = to_tm t in
    let time =
      try fst (Unix.mktime tm_date)
      with Unix.Unix_error (e,s1,s2) ->
        invalid_argf "Date.to_time(%s): Unix error converting time: (%s,%s,%s)"
          (to_string t) (Unix.error_message e) s1 s2 ()
    in
    Time.of_float time

  

  let add_days t n =
    let time = to_time_internal t in
    of_time (Time.add time (Span.of_day (Float.of_int n)))

  let diff t1 t2 =
    Int.of_float (round (Span.to_day
      (Time.diff (to_time_internal t1) (to_time_internal t2))))

  (* returns a Weekday.t *)
  let day_of_week t =
    let uday = to_tm t in
    let sec, _ = Unix.mktime uday in
    let unix_wday = (Unix.localtime sec).Unix.tm_wday in
    Weekday.of_int_exn unix_wday

  let is_weekday t =
    not (Weekday.is_sun_or_sat (day_of_week t))

  let is_weekend t =
    Weekday.is_sun_or_sat (day_of_week t)    

  let is_business_day t ~is_holiday =
    not (Weekday.is_sun_or_sat (day_of_week t))
    && not (is_holiday t)

  
  let add_days_skipping t ~skip n =
    let step = if Int.(>) n 0 then 1 else -1 in
    let rec loop t k =
      if Int.(=) k 0 then t
      else
        let t_next = add_days t step in
        if skip t_next then loop t_next k
        else loop t_next (k - 1)
    in
    loop t (abs n)

  let add_weekdays t n = add_days_skipping t ~skip:is_weekend n

  let add_business_days t ~is_holiday n =
    add_days_skipping t n ~skip:(fun d -> is_weekend d || is_holiday d)

  let dates_between ~min:t1 ~max:t2 =
    let rec loop t l =
      if t < t1 then l
      else loop (add_days t (-1)) (t::l)
    in
    loop t2 []

  let business_dates_between ~min:t1 ~max:t2 ~is_holiday =
    let rec loop t l =
      if t < t1 then l
      else if is_business_day t ~is_holiday then
        loop (add_days t (-1)) (t::l)
      else loop (add_days t (-1)) l
    in
    loop t2 []

  let weekdays_between ~min ~max =
    let all_dates = dates_between ~min ~max in
    Option.value_map
      (List.hd all_dates)
      ~default:[]
      ~f:(fun first_date ->
        (* to avoid a system call on every date, we just get the weekday for the first
           date and use it to get all the other weekdays *)
        let first_weekday = day_of_week first_date in
        let date_and_weekdays =
          List.mapi all_dates
            ~f:(fun i date -> date,Weekday.shift first_weekday i) in
        List.filter_map date_and_weekdays
          ~f:(fun (date,weekday) ->
            if Weekday.is_sun_or_sat weekday
            then None
            else Some date)
      )

  let rec previous_weekday t =
    let previous_day = add_days t (-1) in
    if is_weekday previous_day then
      previous_day
    else
      previous_weekday previous_day

  let weekdays_between ~min ~max =
    List.filter
      (dates_between ~min ~max)
      ~f:is_weekday
end

open Date.T

module Z = struct
  let of_date_ofday_impl ~suffix mktime date ofday =
    try
      if Date.(<=) date Date.min_value then Time.min_value
      else if Date.(>=) date Date.max_value then Time.max_value
      else begin
        let ofday = Ofday.to_sec ofday in
        
        if Span.(>) ofday Span.day then failwithf "%g seconds > 24 hours" ofday ();
        if Span.(<) ofday 0. then failwithf "%g seconds < 0." ofday ();
        let (float_part, sec) = modf ofday in
        let sec = int_of_float sec in
        let uday = { Unix.
                     tm_sec = sec mod 60;
                     tm_min = (sec / 60) mod 60;
                     tm_hour = sec / 3600;
                     tm_mday = date.d;
                     tm_mon = Month.to_int date.m - 1;
                     tm_year = date.y - 1900;
                     tm_wday = 0;
                     tm_yday = 0;
                     tm_isdst = false;
        } in
        let time =
          try mktime uday
          with Unix.Unix_error (e,s1,s2) ->
            failwithf "Unix error converting date: (%s,%s,%s)"
              (Unix.error_message e) s1 s2 ()
        in
        Time.of_float (time +. float_part)
      end
    with exn ->
      invalid_argf "Time.of_date_ofday%s %s %s: %s"
        suffix (Date.to_string date) (Ofday.to_string ofday) (Exn.to_string exn) ()
  ;;

  let of_date_ofday date ofday =
    of_date_ofday_impl ~suffix:"" (fun uday -> fst (Unix.mktime uday)) date ofday

  let of_date_ofday_utc date ofday =
    of_date_ofday_impl ~suffix:"_utc" Unix.timegm date ofday

  let tm_subs_to_ofday tm subs =
    subs +. float (tm.Unix.tm_sec + 60 * (tm.Unix.tm_min + tm.Unix.tm_hour * 60))

  let to_date_ofday_impl tmfun time =
    if Time.(=) time Time.min_value then Date.min_value, Ofday.start_of_day
    else if Time.(=) time Time.max_value then Date.max_value, Ofday.start_of_day
    else
      let time = Time.to_float time in
      let subs, sec = modf time in
      let tm = tmfun sec in
      let ofday = tm_subs_to_ofday tm subs in
      Date.of_tm tm, Ofday.of_sec ofday

  let to_date_ofday time = to_date_ofday_impl Unix.localtime time
  let to_date_ofday_utc time = to_date_ofday_impl Unix.gmtime time

  let to_date = Date.of_time

  
  let to_ofday time =
    if Time.(=) time Time.min_value || Time.(=) time Time.max_value then
      Ofday.start_of_day
    else
      let time = Time.to_float time in
      let subs, sec = modf time in
      let tm = Unix.localtime sec in
      let ofday = tm_subs_to_ofday tm subs in
      Ofday.of_sec ofday

  let to_string t =
    let date, sec = to_date_ofday t in
    sprintf "%s %s" (Date.to_string date) (Ofday.to_string sec)

  let to_string_trimmed t =
    let date, sec = to_date_ofday t in
    sprintf "%s %s" (Date.to_string date) (Ofday.to_string_trimmed sec)

  let to_string_old t =
    let date, sec = to_date_ofday t in
    sprintf "%s %s" (Date.to_string_old date) (Ofday.to_string sec)

  let to_filename_string t =
    let date, ofday = to_date_ofday t in
    sprintf "%s_%s"
      (Date.to_string date)
      (String.tr ~target:':' ~replacement:'-' (Ofday.to_string ofday))

  let to_string_fix_proto utc t =
    let date, sec =
      match utc with
      | `Utc -> to_date_ofday_utc t
      | `Local -> to_date_ofday t
    in
    sprintf "%s-%s" (Date.to_string_iso8601_basic date) (Ofday.to_string sec)

  let of_string_fix_proto utc str =
    try
      let expect_length = 21 in  (* = 8 + 1 + 12 *)
      let expect_dash = 8 in
      if Char.(<>) str.[expect_dash] '-' then
         failwithf "no dash in position %d" expect_dash ();
      let of_date_ofday =
        match utc with
        | `Utc -> of_date_ofday_utc
        | `Local -> of_date_ofday
      in
      if Int.(>) (String.length str) expect_length then
        failwithf "input too long" ();
      of_date_ofday
        (Date.of_string_iso8601_basic str ~pos:0)
        (Ofday.of_string_iso8601_extended str ~pos:(expect_dash + 1))
    with exn ->
      invalid_argf "Time.of_string_fix_proto %s: %s" str (Exn.to_string exn) ()
  ;;

  let of_filename_string s =
    try
      match String.lsplit2 s ~on:'_' with
      | None -> failwith "no space in filename string"
      | Some (date, ofday) ->
          let date = Date.of_string date in
          let ofday = String.tr ~target:'-' ~replacement:':' ofday in
          let ofday = Ofday.of_string ofday in
          of_date_ofday date ofday
    with
    | exn ->
        invalid_argf "Time.of_filename_string (%s): %s" s (Exn.to_string exn) ()
  ;;

  let of_date_time_strings date_string time_string =
    of_date_ofday (Date.of_string date_string) (Ofday.of_string time_string)

  let of_date_time_strings_utc date_string time_string =
    of_date_ofday_utc (Date.of_string date_string) (Ofday.of_string time_string)

  let of_string s =
    try
      (* use rsplit2 instead of lsplit2 because date can contain a space,
         e.g., "01 JAN 2008 10:37:22.551", while ofday cannot *)
      match String.rsplit2 s ~on:' ' with
      | None -> invalid_arg (sprintf "no space in Time.of_string: %s" s)
      | Some (date,time) -> of_date_time_strings date time
    with
    | e -> invalid_arg (sprintf "Time.of_string: %s" (Exn.to_string e))

  let t_of_sexp sexp = match sexp with
    | Sexp.List [Sexp.Atom date; Sexp.Atom ofday] ->
        begin
          try of_string (date ^ " " ^ ofday)
          with
        e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
        end
    | _ -> of_sexp_error "Time.t_of_sexp" sexp

  let sexp_of_t t =
    match String.lsplit2 (to_string t) ~on:' ' with
    | Some (date,ofday) ->
        Sexp.List [Sexp.Atom date; Sexp.Atom ofday]
    | None ->
        raise (Bug "Time.sexp_of_t: unexpected None")

  let pp ppf t = Format.fprintf ppf "%s" (to_string t)
  let () = Pretty_printer.register "Core.Time.pp"

(* Miscellaneous *)

(** Pause (and don't throw an exception)  *)
  let pause span =
    (* If too large a float is passed in (Span.max_value for instance) then
       select will return immediately, leading to an infinite and expensive
       select loop.  This is handled below by pausing for no longer than 100 days
       at a time. *)
    let span = Span.min span (Span.scale Span.day 100.) in
    let finish = Time.add (Time.now ()) span in
    let rec pause_for span =
      begin
        try
          ignore (Unix.select ~read:[] ~write:[] ~except:[] ~timeout:(Span.to_sec span))
        with
        | Sys.Break -> raise Sys.Break
        | _ -> ()
      end;
      let now = Time.now () in
      if Time.(>) finish now
      then pause_for (Time.diff finish now)
    in
    pause_for span

  let ofday_occurrence ofday before_or_after time =
    let first_guess = of_date_ofday (to_date time) ofday in
    match before_or_after with
    | `right_before ->
        if Time.(<) first_guess time
        then first_guess
        else Time.sub first_guess Span.day
    | `right_after ->
        if Time.(>) first_guess time
        then first_guess
        else Time.add first_guess Span.day


  let ofday_occurrence_utc ofday before_or_after time =
    let to_date time = fst (to_date_ofday_utc time) in
    let first_guess = of_date_ofday_utc (to_date time) ofday in
    match before_or_after with
    | `right_before ->
        if Time.(<) first_guess time
        then first_guess
        else Time.sub first_guess Span.day
    | `right_after ->
        if Time.(>) first_guess time
        then first_guess
        else Time.add first_guess Span.day
end

include Time
include Z
