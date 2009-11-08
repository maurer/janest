(* Test Condition.timedwait *)

open Core.Std
open Core_extended.Std
open Unix_ext
open OUnit

let run () =
  let mtx = Mutex.create () in
  let cnd = Condition.create () in
  let condition = ref false in
  let timedout = ref false in
  let condition_setter () =
    Thread.delay 0.2;
    Mutex.lock mtx;
    condition := true;
    Condition.signal cnd;
    Mutex.unlock mtx;
  in
  let _tid = Thread.create condition_setter () in
  Mutex.lock mtx;
  let rec loop () =
    let timeout = Unix.gettimeofday () +. 0.15 in
    if condition_timedwait cnd mtx timeout then (
      assert !timedout;
      assert !condition)
    else (
      assert (not !timedout);
      timedout := true;
      loop ())
  in
  loop ();
  printf "Condition.timedwait succeeded\n\n%!"

let test = "Condition_test" >::: [
  "test" >:: (fun () ->
    "1" @? (try run (); true with _ -> false));
]

