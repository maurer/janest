open Core.Std
module Int_set = Core_extended.Int_set;;
open OUnit;;
(* open Core_extended.Std *)

module S = Core_extended.Std.Int_set

let adds ~log2_degree n () =
  let t = S.create ~log2_degree () in
  for i = 0 to n do
    assert (not (S.mem t i));
  done;
  for i = 0 to n do
    S.add t i;
    for j = 0 to i do
      assert (S.mem t j);
    done;
    for j = i + 1 to n do
      assert (not (S.mem t j))
    done;
  done;
;;

let add_evens ~log2_degree n () =
  let t = S.create ~log2_degree () in
  for i = 0 to n do
    if i mod 2 = 0 then S.add t i;
  done;
  for i = 0 to n do
    assert (S.mem t i = (i mod 2 = 0));
  done
;;

let add_only ~log2_degree n () =
  let t = S.create ~log2_degree () in
  for i = 0 to n do
    S.add t i;
  done;
;;

let add_ranges ~log2_degree n () =
  let t = S.create ~log2_degree () in
  for i = 0 to n do
    S.add_range t ~lo:(i * 2) ~hi:(i * 2 + 1)
  done;
  for i = 0 to 2 * n - 1 do
    assert (S.mem t i);
  done
;;

let add_down ~log2_degree n () =
  let t = S.create ~log2_degree () in
  for i = n downto 0 do
    S.add t i;
  done;
  for i = 0 to n do
    assert (S.mem t i);
  done;
;;

let add_large ~log2_degree n () =
  let t = S.create ~log2_degree () in
  S.add_range t ~lo:1 ~hi:n;
  assert (not (S.mem t 0));
  for i = 1 to n do
    assert (S.mem t i);
  done;
  assert (not (S.mem t (n + 1)));
  S.add_range t ~lo:1 ~hi:(n + 1);
  for i = 1 to n + 1 do
    assert (S.mem t i);
  done;
  S.add_range t ~lo:n ~hi:(2 * n);
  S.add t 0;
  for i = 0 to 2 * n do
    assert (S.mem t i);
  done;
  assert (not (S.mem t (2 * n + 1)));
;;

let test =
  TestList
    (
      (List.concat
          (List.init 5 ~f:(fun i ->
            let log2_degree = i + 1 in
            let name = sprintf "log2_degree:%d " log2_degree in
            [
             name ^ " adds" >:: adds ~log2_degree 500;
             name ^ " add_evens" >:: add_evens ~log2_degree 1000;
             name ^ " add_ranges" >:: add_ranges ~log2_degree 500;
             name ^ " add_down" >:: add_down ~log2_degree 500;
             name ^ " add_large" >:: add_large ~log2_degree 1000;
            ])))
      @ ["degree32 big" >:: add_only ~log2_degree:5 10_000_000]
    )
;;
