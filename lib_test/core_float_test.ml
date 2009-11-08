open Core.Std
open OUnit

let () = Random.self_init ()

(* This may NOT be true, e.g. for x = 7269165347228151. Generally fails about half the
   time for values in range 2.0 ** 52.0 to 2.0 ** 53.0 *)
let round_test x =
  let y = Float.round x in
  -0.5 < y -. x && y -. x <= 0.5

(* Float.iround built so this should always be true *)
let iround_test x =
  let y = Float.iround x in
  match y with
  | None -> true
  | Some y -> -0.5 < (float_of_int y) -. x && (float_of_int y) -. x <= 0.5

let round_test x =
  (*round_test x &&*) iround_test x

let absirand () =
  let rec aux acc cnt =
    if cnt = 0 then
      acc
    else
      let bit = if Random.bool () then 1 else 0 in
      aux (2 * acc + bit) (cnt / 2)
  in
  aux 0 max_int
(* Random with a distribution favouring small ones*)

let frand () =
  let x = (float (absirand ())) +. Random.float 1.0 in
  if Random.bool () then
    -1.0 *. x
  else
    x

let test =
  "core_float" >:::
    [ "to_string_hum" >::
        (fun () ->
          "random" @? (
            List.init ~f:(fun _ -> frand ()) 10_000
            |! List.for_all ~f:round_test
          );
          "max_int" @? round_test (float_of_int max_int);
          "min_int" @? round_test (float_of_int min_int)
        )
    ]
