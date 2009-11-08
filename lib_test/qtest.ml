(** Regression test runner. *)

open Core.Std;;


let tests = 
  (* Avoid forking tests, which don't play nicely with async. *)
  Qtest_lib.Std.Test.tests_of_ounit ~exclude:[Parent_death_test.test] Test.all

let () = 
  Qtest_lib.Std.Runner.main tests
