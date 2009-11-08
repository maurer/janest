(** Tests for Filename. *)

open OUnit
open Core.Std

let printer a = a

let test_normalize ~original ~expected () =
  assert_equal ~printer expected (Filename.normalize original)

let test_parent ~original ~expected () =
  assert_equal ~printer expected (Filename.parent original)

let test = 
  "filename_test" >::: [
    "test_normalize1" >:: 
      test_normalize ~original:"/mnt/local" ~expected:"/mnt/local";
    "test_normalize2" >:: 
      test_normalize ~original:"/mnt/local/../global/foo" ~expected:"/mnt/global/foo";
    "test_normalize3" >:: 
      test_normalize ~original:"/mnt/local/../../" ~expected:"/";
    "test_parent1" >:: 
      test_parent ~original:"/mnt/local" ~expected:"/mnt";
    "test_parent2" >:: 
      test_parent ~original:"/mnt/local/../global/foo" ~expected:"/mnt/global";
    "test_parent3" >:: 
      test_parent ~original:"/mnt/local/../../global" ~expected:"/";
  ]
