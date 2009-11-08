open Core.Std
open OUnit;;
open Fqueue

let lpush l q = 
  List.fold_right ~init:q ~f:push l

let test =
  "fqueue" >:::
    [ "basic" >::
        (fun () -> 
           let q0 = lpush [1;2;3;4;5] empty in
           let _ ,q1 = pop_exn q0 in
           let _ ,q2 = pop_exn q1 in
           let q3 = push 0 q2 in
           test_invariants q0;
           test_invariants q1;
           test_invariants q2;
           test_invariants q3;
           "len0" @? (length q0 = 5);
           "len1" @? (length q1 = 4);
           "len2" @? (length q2 = 3);
           "len3" @? (length q3 = 4);
           "q0" @? (to_list q0 = [1;2;3;4;5]);
           "q1" @? (to_list q1 = [1;2;3;4]);
           "q2" @? (to_list q2 = [1;2;3]);
           "q3" @? (to_list q3 = [0;1;2;3]);
           "top_exn3" @? (top_exn q3 = 3);
           "bot_exn3" @? (bot_exn q3 = 0);
           "discard_exn" @? (to_list (discard_exn q0) = to_list q1);
           "notempty3" @? not (is_empty q3);
           "empty3" @? is_empty (discard_exn (discard_exn (discard_exn (discard_exn q3))));
           "misc" @? (to_list (lpush [2;3] (discard_exn (discard_exn (discard_exn (discard_exn q3))))) = [2;3]);
        );
      "invariants" >::
        (fun () -> 
           ignore (List.fold_left (List.range 0 1000) ~init:empty
                     ~f:(fun q i -> 
                           let q = 
                             match Random.int 3 with
                             | 0 -> push i q
                             | 1 -> 
                                 (match pop q with
                                  | None -> q | Some (_,q) -> q)
                             | 2 -> (try discard_exn q with Empty -> q)
                             | _ -> q
                           in
                           test_invariants q;
                           q)))
    ]
           
