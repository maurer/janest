open Core.Std
open OUnit;;

module Date = Date
module Ofday = Time.Ofday
module Span = Time.Span

let teq t1 t2 = truncate (Time.to_float t1 *. 1000.) = truncate (Time.to_float t2 *. 1000.)
let speq s1 s2 = round (Span.to_ms s1) = round (Span.to_ms s2)
let convtest ?tol f1 f2 =
  let x = float (Random.int 1_000_000) /. 1000. in 
  let tol = match tol with
    | None -> Float.epsilon
    | Some pct -> x *. pct
  in
  abs_float (f1 (f2 x) -. x) <= tol

let mintime_str = "0000-01-01 00:00:00.000"
let maxtime_str = "3000-01-01 00:00:00.000"

let time_gen () = Time.of_float (Quickcheck.fg ())
let reasonable_time time =              (* between about 1970 and 2070 *)
  let time = Time.to_float time in
  time > 0. && time < 100. *. 52. *. 24. *. 60. *. 60.
let similar_time time time' =
  let time = Time.to_float time in
  let time' = Time.to_float time' in
  abs_float (time -. time') < 0.01

let test_list = ref []
let add name test = test_list := (name >:: test) :: !test_list

let () = add "t"
  (fun () ->
      let s1 = "2005-05-25 12:46" in
      let s2 = "2005-05-25 12:46:15" in
      let s3 = "2005-05-25 12:46:15.232" in
      let time1 = Time.of_string s1 in
      let time2 = Time.of_string s2 in
      let time3 = Time.of_string s3 in
      let now1 = Time.now () in
      let now2 = Time.now () in
      "diff1" @? (round (Span.to_sec (Time.diff time2 time1)) = 15);
      "diff1'" @? (round (Span.to_ms (Time.diff time2 time1)) = 15 * 1000);
      "diff2" @? (round (Span.to_ms (Time.diff time3 time2)) = 232);
      "conv1" @? (Time.to_string time1 = s1 ^ ":00.000");
      "conv2" @? (Time.to_string time2 = s2 ^ ".000");
      "conv3" @? (Time.to_string time3 = s3);
      "ord" @? (now2 >= now1);
      "sexp1" @? (Time.t_of_sexp (Time.sexp_of_t time1) = time1);
      "sexp2" @? (Time.t_of_sexp (Time.sexp_of_t time2) = time2);
      "sexp3" @? (Time.t_of_sexp (Time.sexp_of_t time3) = time3);
      let date, ofday = Time.to_date_ofday time3 in
      "date" @? (date = Date.of_string "2005-05-25");
      "ofday" @? (Time.Ofday.to_sec ofday =.
          Time.Ofday.to_sec (Time.Ofday.of_string "12:46:15.232"));
      "ofday1" @? (Time.Ofday.of_string "09:13" = Time.Ofday.of_string "0913");
      "add1" @? teq (Time.add time1 (Span.of_sec 15.)) time2;
      "add2" @? teq (Time.add time2 (Span.of_ms 232.)) time3;
      "min" @? (Time.to_string Time.min_value = mintime_str && Time.of_string mintime_str = Time.min_value);
      "max" @? (Time.to_string Time.max_value = maxtime_str && Time.of_string maxtime_str = Time.max_value);
    )

let () =
  add "Ofday_string_conversion"
    (fun () ->
      for i = 0 to 100_000 do
        let ofday = Ofday.of_span_since_midnight (Span.of_ms (float i)) in
        let ofday_string = Ofday.to_string ofday in
        let ofday' = Ofday.of_string ofday_string in
        if Ofday.(<>) ofday ofday' then
          failwithf "%s <> Ofday.of_string %s"
            ofday_string (Ofday.to_string ofday') ();
        let ofday' = Ofday.of_string_iso8601_extended ofday_string in
        if Ofday.(<>) ofday ofday' then
          failwithf "%s <> Ofday.of_string_iso8601_extended %s"
            ofday_string (Ofday.to_string ofday') ();
      done)

let () =
  add "date"
    (fun () ->
      let start =
        Time.of_date_ofday
          (Date.create ~y:1999 ~m:Month.jan ~d:1)
          Ofday.start_of_day
      in
      let day = Span.of_day 1. in
      for i = 0 to 100_000 do
        let date =
          Time.to_date (Time.add start (Span.scale (float i) day))
        in
        let date_string = Date.to_string date in
        let date' = Date.of_string date_string in
        if Date.(<>) date date' then
          failwithf "%s <> Date.of_string %s"
            date_string (Date.to_string date') ();
      done)
    
let () = 
  add "span_scale"
    (fun () ->
      "ms" @? speq (Span.scale 0.001 (Span.of_sec 10.)) (Span.of_ms 10.);
      "min" @? speq (Span.scale 60. (Span.of_sec 10.)) (Span.of_min 10.);
      "hr" @? speq (Span.scale (60. *. 60.) (Span.of_sec 10.)) (Span.of_hr 10.);
    );
  add "span_conv"
    (fun () ->
      for i = 1 to 100 do
        "sec" @? convtest Span.to_sec Span.of_sec;
        "ms" @? convtest Span.to_ms Span.of_ms;
        "min" @? convtest (fun x -> Span.to_sec x /. 60.) Span.of_min;
        "hr" @? convtest (fun x -> Span.to_sec x /. 60. /. 60.) Span.of_hr;
        "sexp" @?
          convtest ~tol:0.0001
          (fun x -> Span.to_sec (Span.t_of_sexp x))
          (fun x -> Span.sexp_of_t (Span.of_sec x));
      done
    );
  add "date"
    (fun () ->
      let d = Date.create ~y:2004 ~m:Month.apr ~d:15 in
      "conv1" @? (Date.to_string d = "2004-04-15");
      "conv2" @? (d = Date.of_string "2004-04-15");
      "conv3" @? (d = Date.of_string "20040415");
      "conv4" @? (d = Date.of_string "15APR2004");
      "conv5" @? (d = Date.of_string "04/15/2004");
    );
  add "norollover"
    (fun () ->
      let t1 = Time.of_string "2005-05-25 12:46:59.900" in
      let t2 = Time.add t1 (Span.of_ms 99.9) in
      "60secspr" @? ((Time.to_string t2) = "2005-05-25 12:46:59.999");
    );
  add "to_string,of_string"
    (fun () ->
      let check time =
        if reasonable_time time
        then
          let time' = Time.of_string (Time.to_string time) in
          similar_time time time'
        else true
      in
      Quickcheck.laws_exn "string" 100 time_gen check;
    );
  add "to_string,of_string2"
    (fun () ->
      let s = "2005-06-01 10:15:08.047" in
      let t = Time.of_string s in
      "foo" @? (Time.to_string t = s)
    );
  add "to_string,of_string3"
    (fun () ->
      let s = "2006-06-16 04:37:07.082" in
      let t = Time.of_string s in
      "foo" @? (Time.to_string t = s)
    );
  add "to_filename_string,of_filename_string"
    (fun () ->
      let check time =
        if reasonable_time time
        then
          let time' = Time.of_filename_string (Time.to_filename_string time) in
          similar_time time time'
        else true
      in
      Quickcheck.laws_exn "string" 100 time_gen check;
    );
  add "to_filename_string,of_filename_string2"
    (fun () ->
      let s = "2005-06-01_10-15-08.047" in
      let t = Time.of_filename_string s in
      "foo" @? (Time.to_filename_string t = s)
    );
  add "of_sexp,to_sexp"
    (fun () ->
      let check time =
        if reasonable_time time
        then
          let time' = Time.t_of_sexp (Time.sexp_of_t time) in
          similar_time time time'
        else true
      in
      Quickcheck.laws_exn "sexp" 100 time_gen check;
    );
  add "daylight_saving_time"
    (fun () ->
      let s = "2006-04-02 23:00:00.000" in
      let time = Time.of_string s in
      "dst" @? (Time.to_string time = s)
    );
  add "ofday_small_diff"
    (fun () ->
      let same x y = abs_float (x -. y) < sqrt epsilon_float in
      let check (s1,s2,d) =
        let t1 = Time.Ofday.of_string s1 in
        let t2 = Time.Ofday.of_string s2 in
        same (Span.to_sec (Time.Ofday.small_diff t1 t2)) d &&
          same (Span.to_sec (Time.Ofday.small_diff t2 t1)) (-.d) in
      "foo" @? List.for_all ~f:check
        ["10:00:01.298", "14:59:55.000", 6.298;
         "08:59:54.000", "10:00:01.555", -7.555;
         "12:48:55.787", "17:48:55.000", 0.787;
        ]);
  add "ofday_occurrence_right_side"
    (fun () -> 
      let times = [
        "00:00:00";
        "00:00:01";
        "09:00:00";
        "11:59:59";
        "12:00:00";
        "12:00:01";
        "18:30:30";
        "23:59:59";
      ] in
      let now = Time.now () in
      let now_f = Time.to_float now in
      let utimes = Time.to_ofday now :: List.map times ~f:(Time.Ofday.of_string) in
      let after_times = List.map utimes 
        ~f:(fun ut -> Time.ofday_occurrence ut `right_after now) in
      let before_times = List.map utimes
        ~f:(fun ut -> Time.ofday_occurrence ut `right_before now) in
      "right-side-after" @? List.for_all after_times 
        ~f:(fun t -> Time.to_float t > now_f);
      "right-side-before" @? List.for_all before_times 
        ~f:(fun t -> Time.to_float t < now_f);
    );
  add "ofday_occurrence_distance"
    (fun () -> 
      let now = Time.of_string "2007-05-04 13:00:00.000" in
      let after_times = [
        ("13:00:00.000", "2007-05-05 13:00:00.000");
        ("13:00:00.001", "2007-05-04 13:00:00.001");
        ("11:59:59.999", "2007-05-05 11:59:59.999");
        ("00:00:00.000", "2007-05-05 00:00:00.000");
        ("12:59:59.000", "2007-05-05 12:59:59.000");
      ] in
      let before_times = [
        ("13:00:00.000", "2007-05-03 13:00:00.000");
        ("13:00:00.001", "2007-05-03 13:00:00.001");
        ("11:59:59.999", "2007-05-04 11:59:59.999");
        ("00:00:00.000", "2007-05-04 00:00:00.000");
        ("12:59:59.000", "2007-05-04 12:59:59.000");
      ] in
      List.iter after_times ~f:(fun (od_s,prediction_s) -> 
        let od = Time.Ofday.of_string od_s in
        let prediction = Time.of_string prediction_s in
        let real = Time.ofday_occurrence od `right_after now in
        ("right-distance - " ^ od_s ^ "," ^ prediction_s) @? 
          if Span.to_ms (Time.diff prediction real) = 0. then true 
          else false
      );
      List.iter before_times ~f:(fun (od_s,prediction_s) -> 
        let od = Time.Ofday.of_string od_s in
        let prediction = Time.of_string prediction_s in
        let real = Time.ofday_occurrence od `right_before now in
        ("right-distance - " ^ od_s ^ "," ^ prediction_s) @? 
          if Span.to_ms (Time.diff prediction real) = 0. then true 
          else false
      )
    )
;;


let roundtrip s = 
  let t = Span.of_string s in
  ("string roundtrip " ^ s) @? 
    Span.(=) t (Span.of_string (Span.to_string t));
  ("span roundtrip " ^ s) @? 
    String.(=) s (Span.to_string (Span.of_string s))
;;


let () = 
  let extensions = ["ms";"s";"m";"h"] in
  add "roundtrip span<->string" (fun () -> 
    List.iter extensions ~f:(fun ext -> 
      let t x = roundtrip (x ^ ext) in
      t "1";
      t "5";
      t "1.34";
    );
    let t x = roundtrip (x ^ "s") in
    t "59.9999";
    t "59";
  );
  add "Span.of_string" (fun () ->
    let test string secs = 
      ("sec " ^ string) @? (Span.to_sec (Span.of_string string) = secs)
    in
    test "1ms" 0.001;
    test "95ms" 0.095;
    test "1222ms" 1.222;
    test "1.222s" 1.222;
    test "0.5m" 30.;
    test "1m" 60.;
    test "1h" (60. *. 60.);
  )
;;
  


let test = "time" >::: !test_list
