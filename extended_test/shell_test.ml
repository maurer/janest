open Core.Std
open OUnit
module Sh = Core_extended.Shell
let test =
  "shell" >:::
    [ "run" >::
        (fun () ->
          "length" @? ((Sh.run_lines "./yes.exe" [])
                        =
              List.init 200_000 ~f:(fun _num -> "yes")
          );
          let input = String.create 200_000 in
          "partially read stdin" @? (ignore(Sh.run_lines ~input "./yes.exe" []);true)
        );
    ]
