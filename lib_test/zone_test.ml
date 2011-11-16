(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

open OUnit;;
open Core.Std

let init = Memo.unit (fun () ->
  Random.init 9;
  Zone.init ())


let my_tz = Time.Zone.machine_zone ()

(* We don't test Feb 29th because generating proper leap year dates is
  trickier.  Also, there are no time zone changes on leap dates. *)
let month_limits = Map.of_alist_exn [
    1, 31;
    2, 28;
    3, 31;
    4, 30;
    5, 31;
    6, 30;
    7, 31;
    8, 31;
    9, 30;
    10, 31;
    11, 30;
    12, 31
  ]

let random_time () =
  let year  =
    1970 + Random.int 67
    (* dpowers: if we go out much further then floating point errors at the microsecond
       level start to creep in.  We can change this when Time.t = int64 *)
    (*1970 + (Random.int (if Sys.word_size = 64 then 1000 else 67))*)
  in
  let month = 1 + (Random.int 12) in
  let day   = 1 + (Random.int (Map.find_exn month_limits month)) in
  let hour  = Random.int 12 + 8 in
  let min   = Random.int 60 in
  let sec   = Random.int 60 in
  let ms    = Random.int 1_000 in
  let mic   = Random.int 1_000 in
  (year,month,day,hour,min,sec,ms,mic)
;;

(* Round tripping microseconds isn't reliable *)
let random_time_str () =
  let year,month,day,hour,min,sec,ms,_mic = random_time () in
  sprintf "%d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d.%0.3d000" year month day hour min sec ms
;;

let random_tm () =
  let (year,month,day,hour,min,sec,_,_) = random_time () in
  {Unix.
    tm_sec   = sec;
    tm_min   = min;
    tm_hour  = hour;
    tm_mday  = day;
    tm_mon   = month;
    tm_year  = year - 1900;
    tm_wday  = 0;
    tm_yday  = 0;
    tm_isdst = false;
  }

let zone_tests = ref []
let add name test = zone_tests := (name >:: test) :: !zone_tests

let add_random_string_round_trip_tests () =
  for i = 1 to 100 do
    let s1 = random_time_str () in
    let pos_neg = if Random.bool () then "+" else "-" in
    let distance = Int.to_string (Random.int 10 + 1) in
    let s2 = String.concat [s1; pos_neg; distance; ":00"] in
    add ("roundtrip string " ^ s1) (fun () ->
      let t = Time.of_string s1 in
      let c1 = (Time.to_string_deprecated t) in
      if s1 <> c1 then begin
        exit 7;
      end;
      "s1" @? (s1 = (Time.to_string_deprecated (Time.of_string s1)));
      "s2-time" @? (
        let s2_time1 = Time.of_string s2 in
        let s2_time2 = Time.of_string (Time.to_string_abs s2_time1) in
        Time.(=) s2_time1 s2_time2)
    )
  done

let add_random_conversion_test (zone_name,(zone:Zone.t)) =
  add ("roundtrip conversion " ^ zone_name) (fun () ->
    let tm = random_tm () in
    (* round trip to normalize the time *)
    let tm = Unix.gmtime (Unix.timegm tm) in
    let (unix_time,_) = Unix.mktime tm in
    let time = Time.of_float unix_time in
    let (zone_date, zone_ofday) =
      let date,ofday = Time.to_local_date_ofday time in
      Time.convert
        ~from_tz:(Zone.machine_zone ())
        ~to_tz:zone
        date
        ofday
    in
    let round_trip_time =
      let round_date,round_ofday =
        Time.convert
        ~from_tz:zone
        ~to_tz:(Zone.machine_zone ())
        zone_date
        zone_ofday
      in
      Time.of_local_date_ofday round_date round_ofday
    in
    match time = round_trip_time with
    | true -> "time" @? (time = round_trip_time)
    | false -> 
      Printf.printf "\n";
      Printf.printf "%s\n" (Sexp.to_string_hum (Unix.sexp_of_tm tm));
      Printf.printf "%0.20f\n" unix_time;
      Printf.printf "%0.20f\n" (Time.to_float time);
      Printf.printf "%s, %s\n" (Date.to_string zone_date) (Ofday.to_string zone_ofday);
      Printf.printf "%0.20f\n%!" (Time.to_float round_trip_time);
      exit 7)

let add_localtime_tests () =
  List.iter (Zone.initialized_zones ()) ~f:(fun (zone_name, zone) ->
    add ("localtime " ^ zone_name) (fun () ->
      let tm = random_tm () in
      let tm = Unix.gmtime (Unix.timegm tm) in
      Unix.putenv ~key:"TZ" ~data:zone_name;
      ignore (Unix.localtime 1000.);
      let unix_time,_            = Unix.mktime tm in
      let localtime              = Unix.localtime unix_time in
      let our_time               = Time.of_float unix_time in
      let localtime_date_string  = Unix.strftime localtime "%Y-%m-%d" in
      let localtime_ofday_string = Unix.strftime localtime "%H:%M:%S" in
      let date,ofday             = Time.to_date_ofday our_time zone in
      if localtime_date_string <> (Date.to_string date) then ignore (exit 0);
      Unix.unsetenv ("TZ");
      ignore (Unix.localtime 1000.);
      "date" @? (localtime_date_string = Date.to_string date);
      "ofday" @? (localtime_ofday_string = Ofday.to_sec_string ofday)))
;;

let add_random_conversion_tests () =
  List.iter (Zone.initialized_zones ()) ~f:add_random_conversion_test

let add_random_tests () =
  init ();
  add_random_string_round_trip_tests ();
  add_random_conversion_tests ();
  add_localtime_tests ()

let () = add_random_tests ()

let test = "zone" >::: !zone_tests
