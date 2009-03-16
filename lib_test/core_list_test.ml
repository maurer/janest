open OUnit;;
open Core.Std
open List
let ignore = Pervasives.ignore

let l1 = [1;2;3;4;5;6;7;8;9;10]
let long1 = range 1 1000000

let raises_exception f =
  try f (); false
  with _ -> true

let test = 
  "jane_list" >:::
    [ "append" >:: 
        (fun () -> 
           "simple" @? (append [1;2;3] [4;5;6] = [1;2;3;4;5;6]);
           "empty1" @? (append [] [4;5;6] = [4;5;6]);
           "empty2" @? (append [1;2;3] [] = [1;2;3]);
           "singl1" @? (append [1] [2;3] = [1;2;3]);
           "singl2" @? (append [1;2] [3] = [1;2;3]);
           "tailre" @? (ignore (append long1 long1); true);
        );
      "map" >::
        (fun () ->
          "idmap" @? (map ~f:(fun x -> x) l1 = l1);
          "empty" @? (map ~f:(fun x -> x) [] = []);
          "float" @? (map ~f:(fun x -> x +. 5.) [1.;2.;3.] = [6.;7.;8.]);
          "tailre" @? (ignore (map ~f:(fun x -> x) long1); true);
        );
      "map2" >::
        (fun () ->
          "combine" @? (map2 ~f:(fun a b -> a, b) [1;2;3] ['a';'b';'c']
                      = [(1,'a'); (2,'b'); (3,'c')]);
          "empty" @? (map2 ~f:(fun _ _ -> ()) [] [] = []);
          "tailre" @? (ignore(map2 ~f:(fun _ _ -> ()) long1 long1); true);
        );
      "fold_right" >::
        (fun () ->
          "id" @? (fold_right ~f:(fun e acc -> e :: acc) l1 ~init:[] = l1);
          "init" @? (fold_right ~f:(fun e acc -> e ^ acc) ["1";"2"] ~init:"3" = "123");
          "empty" @? (fold_right ~f:(fun _ _ -> ()) [] ~init:() = ());
          "tailre" @? (ignore (fold_right ~f:(fun e acc -> e :: acc) long1 ~init:[]); true);
        );
      "fold_right2" >::
        (fun () ->
          "id" @? (fold_right2 ~f:(fun e1 e2 (acc1,acc2) ->(e1::acc1, e2::acc2))
                               l1 l1 ~init:([],[]) = (l1,l1));
          "init" @? (fold_right2 ~f:(fun e1 e2 acc -> e1 ^ e2 ^ acc)
                                 ["1";"3"] ["2";"4"] ~init:"5" = "12345");
          "different sizes" @? raises_exception 
            (fun () -> fold_right2 ~f:(fun _ _ _ -> 0) [] [0] ~init:0);
          "empty" @? (fold_right2 ~f:(fun _ _ _ -> ()) [] [] ~init:() = ());
          "tailre" @? (ignore (fold_right2 ~f:(fun _ _ _ -> ()) long1 long1 ~init:()); 
                       true);
        );
      "split_and_combine" >::
        (fun () ->
          "id" @? ((split (combine l1 (List.rev l1))) = (l1, List.rev l1));
          "tailre" @? (ignore (split (combine long1 long1)); true);
        );
      "mapi" >::
        (fun () -> 
           "simple" @? 
             (mapi ~f:(fun i x -> (i,x))
                ["one";"two";"three";"four"] = [0,"one";1,"two";2,"three";3,"four"]);
           "empty" @?
             (mapi ~f:(fun i x -> (i,x)) [] = []);
        );
      "dedup_and_stable_dedup" >::
        (fun () ->
          "empty" @? (dedup [] = []);
          "stable_empty" @? (stable_dedup [] = []);
          "same" @? (dedup [5;5;5;5] = [5]);
          "stable_same" @? (stable_dedup [5;5;5;5] = [5]);
          "stability" @? (stable_dedup [5;9;3;5;2;2] = [5;9;3;2]);
        );
      "last" >::
        (fun () ->
          "simple" @? (last [1;2;3] = 3);
          "single" @? (last [1] = 1);
          "tailre" @? (last long1 = 999999);
        );
      "split_n" >:: 
        (fun () -> 
          "mid" @? (split_n [1;2;3;4;5;6] 3 = ([1;2;3],[4;5;6]));
          "big" @? (split_n [1;2;3;4;5;6] 100 = ([1;2;3;4;5;6],[]));
          "zero" @? (split_n [1;2;3;4;5;6] 0 = ([],[1;2;3;4;5;6]));
          "neg" @? (split_n [1;2;3;4;5;6] (-5)= ([],[1;2;3;4;5;6]));
        );
      "flatten" >:: 
        (fun () -> 
          "empty" @? (List.flatten [] = []);
          "single empty" @? (List.flatten [[]] = []);
          "single singleton" @? (List.flatten [[3]] = [3]);
          "single multi" @? (List.flatten [[1;2;3;4]] = [1;2;3;4]);
          "multi multi" @? (List.flatten [[1;2;3;4];[5;6;7];[8;9;10];[];[11;12]] = 
              [1;2;3;4;5;6;7;8;9;10;11;12]);
        );
      "fold_left_term" >::
        (fun () -> 
          "simple" @? (List.fold_left_term ~init:0 ~f:(fun x y -> (true,x+y))
                          [1;2;3;4;5] = 15);
          "term" @? (List.fold_left_term ~init:0 ~f:(fun x y -> (y <> 3,x+y))
                          [1;2;3;4;5] = 6);
          "false" @? (List.fold_left_term ~init:0 ~f:(fun x y -> (y <> 1,x+y))
                          [1;2;3;4;5] = 1);
        );
      "rev_map_append" >::
        (fun () -> 
          "id" @?
            (List.rev_map_append [1;2;3;4;5] [6] ~f:(fun x -> x) = [5;4;3;2;1;6]);
          "double" @?
            (List.rev_map_append [1;2;3;4;5] [6] ~f:(fun x -> 2 * x) =
              [10;8;6;4;2;6]);
          "empty" @?
            (List.rev_map_append [] [6] ~f:(fun _ -> failwith "bug!") = [6]);
        );
      "find_a_dup" >::
        (fun () ->
          "empty" @? (List.find_a_dup [] = None);
          "singleton" @? (List.find_a_dup [3] = None);
          "double nodup" @? (List.find_a_dup [3;4] = None);
          "double dup" @? (List.find_a_dup [3;3] = Some 3);
          "multi no dup" @? (List.find_a_dup [3;5;4;6;12] = None);
          "multi single dup" @? (List.find_a_dup [3;5;4;5;12] = Some 5);
          "multi multi dups" @? (List.find_a_dup [3;5;12;5;12] = Some 5);
        )

    ]

(*
let tests = 
  "list" >:::
    [ "split_n" >:: (fun () -> 
                       "mid" @? (split_n 3 [1;2;3;4;5;6] = ([1;2;3],[4;5;6]));
                       "big" @? (split_n 100 [1;2;3;4;5;6] = ([1;2;3;4;5;6],[]));
                       "zero" @? (split_n 0 [1;2;3;4;5;6] = ([],[1;2;3;4;5;6]));
                       "neg" @? (split_n (-5) [1;2;3;4;5;6] = ([],[1;2;3;4;5;6])););
      "first_n" >:: (fun () -> 
                       "mid" @? (first_n 3 [1;2;3;4;5;6] = [1;2;3]);
                       "big" @? (first_n 100 [1;2;3;4;5;6] = [1;2;3;4;5;6]);
                       "zero" @? (first_n 0 [1;2;3;4;5;6] = []);
                       "neg" @? (first_n (-5) [1;2;3;4;5;6] = []););
      "partition_map" >:: 
        (fun () -> 
           "even" @? (partition_map ~f:(fun x -> if x mod 2 = 0 
                                        then `Pass (10 * x/2) else `Fail (float x))
                        [1;2;3;4;5;6;7] = ([10;20;30],[1.;3.;5.;7.]));
        )
    ]
*)
