open OUnit;;

let main () = ignore (run_test_tt_main Test.all)
let () = Core.Exn.handle_uncaught ~exit:true main
