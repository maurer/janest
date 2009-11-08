open Core.Std
open OUnit

module A = Core_extended.Std.Array

let is_even x = x mod 2 = 0

let test = "Extended_Array" >::: [
  "foldi" >:: (fun () ->
    "1" @? (40 = A.foldi ~init:0 [|1;2;3;4;5|] ~f:(fun i a x -> a + i * x)));
]
