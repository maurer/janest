open Core.Std

(* in place string reversal *)
let s_rev s =
  let n = String.length s - 1 in
  if n >= 1 then
    for i = 0 to n/2 do
      let c1 = s.[i]
      and c2 = s.[n-i] in
      s.[i] <- c2;
      s.[n-i] <- c1
    done;
  s (* We return the value to enable us to chain applications*)

(* Same as [Int_conversions.prettify_string] but introduces the underscores
   counting from the left*)
let rpretty s =  s_rev (Core.Int_conversions.prettify_string (s_rev s))



let to_string_hum f =
  let s = string_of_float f in
  match String.lsplit2 s ~on:'.' with
  | None -> s (*nan,infinity...*)
  | Some (ip,fpe) ->
      let ip  = Core.Int_conversions.prettify_string ip in
      match String.lsplit2 fpe ~on:'e' with
      | None ->  ip ^ "." ^ rpretty fpe
      | Some (fp,e) -> ip ^ "." ^ rpretty fp ^ "e" ^ e


let pretty f =
  match classify_float f with
  | FP_infinite -> if f < 0.0 then "-inf" else "inf"
  | FP_nan -> "nan"
  | FP_subnormal | FP_normal | FP_zero ->
      let round f = int_of_float (floor (f +. 0.5)) in
      let drop_redundant_suffix s =
        let rec loop i =
          if i = 0 then 1
          else
            match String.get s i with
            | '.' -> i
            | '0' -> loop (i - 1)
            | _ -> i + 1
        in
        String.sub s ~pos:0 ~len:(loop (String.length s - 1))
      in
      let decimal sign f =
        assert (0.9995 <= f && f < 999.5);
        let spot = if f < 9.995 then 1 else if f < 99.95 then 2 else 3 in
        let f = f *. 1000.0 /. (10.0 ** float spot) in
        assert (99.5 <= f && f < 999.5);
        let i = round f in
        assert (100 <= i && i <= 999);
        let d1 = i / 100 in
        let d2 = (i mod 100) / 10 in
        let d3 = i mod 10 in
        let s =
          match spot with
          | 1 -> sprintf "%d%s%d%d" d1 sign d2 d3
          | 2 -> sprintf "%d%d%s%d" d1 d2 sign d3
          | 3 -> sprintf "%d%d%d%s" d1 d2 d3 sign
          | _ -> assert false
        in
        drop_redundant_suffix s
      in
      if f < -0.005 then "<0"
      else if f < 0.004 then "0"
      else if f < 0.995 then
        let i = round (f *. 100.0) in
        drop_redundant_suffix (sprintf "0.%d%d" (i / 10) (i mod 10))
      else if f < 0.9995 then "1"
      else if f < 99.95 then decimal "." f
      else if f < 10_000.0 then sprintf "%d" (round f)
      else if f < 999.5E3 then decimal "k" (f /. 1E3)
      else if f < 999.5E6 then decimal "m" (f /. 1E6)
      else if f < 999.5E9 then decimal "g" (f /. 1E9)
      else if f < 999.5E12 then decimal "t" (f /. 1E12)
      else "HUGE"
