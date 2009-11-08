open Core.Std
open OUnit

module L = Core_extended.Std.List

let is_even x = x mod 2 = 0

let test = "Extended_list" >::: [ 
  "take_while" >:: (fun () ->
    "take evens" @? (
      (L.take_while [2;4;6;7;8;9] is_even) = [2;4;6]));
]
