open Core.Std
open OUnit;;
open OUnit_utils
module S = String

let str1 = "1234567890"

let unescaped_test ~name s = name @? (S.unescaped (S.escaped s) = s)

let test = 
  "core_string" >:::
    [ "slice" >::
        (fun () ->
           "all" @? (S.slice str1 0 0 = str1);
           "ordinary" @? (S.slice str1 1 3= "23");
           "neg1" @? (S.slice str1 0 (-1) = "123456789");
           "neg2" @? (S.slice str1 (-1) 0 = "0");
           "neg3" @? (S.slice str1 (-5) (-4) = "6";)
        );
      "nget" >::
        (fun () ->
           "neg" @? (S.nget str1 (-3) = '8');
           "pos" @? (S.nget str1 3 = str1.[3]);
           "invalid" @?
             (try ignore (S.nget str1 (-100)); false
              with Invalid_argument _ -> true | _ -> false)
        );
      "split2_exn" >::
        (fun () ->
           "none" @? (try ignore (S.split2_exn str1 ' '); false
                      with Not_found -> true | _ -> false);
           "some" @? (S.split2_exn str1 '5' = ("1234","67890"));
        );
      "split2" >::
        (fun () ->
           "none" @? (S.split2 str1 ' ' = None);
           "some" @? (S.split2 str1 '5' = Some ("1234","67890"));
        );
      "strip" >::
        (fun () ->
           "both ends" @? (S.strip "  123  " = "123");
           "all white" @? (S.strip "\n\t \n" = "");
           "no white" @? (S.strip "as \t\ndf" = "as \t\ndf");
           "just left" @? (S.strip " a" = "a");
           "just right" @? (S.strip "a " = "a");
        );
      "lstrip" >::
        (fun () ->
          "left" @? (S.lstrip " \t\r\n123  \t\n" = "123  \t\n");
          "all white" @? (S.lstrip " \t \n\n\r " = "");
          "no white on the left" @? (S.lstrip "foo Bar \n " = "foo Bar \n ");
        );
      "rstrip" >::
        (fun () ->
          "right" @? (S.rstrip " \t\r\n123  \t\n\r" = " \t\r\n123");
          "all white" @? (S.rstrip " \t \n\n\r " = "");
          "no white on the right" @? (S.rstrip " \n foo Bar" = " \n foo Bar");
        );
      "map" >::
        (fun () ->
           "empty" @? (S.map ~f:(fun x -> x) "" = "");
           "1" @? (S.map ~f:(function 'a' -> 'b' | 'b' -> 'a' | x -> x)
                     "faboo" = "fbaoo");
        );
      "split_on_char" >::
        (fun () ->
           "empty" @? (S.split_on_char "" 'c' = [""]);
           "1" @? (S.split_on_char "c" 'c' = ["";""]);
           "end" @? (S.split_on_char "fooc" 'c' = ["foo";""]);
           "begin" @? (S.split_on_char "cfoo" 'c' = ["";"foo"]);
           "beginend" @? (S.split_on_char "cfooc" 'c' = ["";"foo";""]);
        );
      "fold" >::
        (fun () ->
          let to_list s = S.fold ~f:(fun acc c -> c::acc) ~init:[] s in
          "empty" @? (to_list "" = []);
          "singleton" @? (to_list "H" = ['H']);
          "simple" @? (to_list "Hello" = ['o';'l';'l';'e';'H']);
        );
      "check_suffix" >::
        (fun () ->
          "empty" @? (S.check_suffix "" "a" = false);
          "singleton" @? (S.check_suffix "H" "H" = true);
          "simple" @? (S.check_suffix "Hello" "lo" = true);
          "simplefalse" @? (S.check_suffix "HelloFoo" "lo" = false);
        );
      "check_prefix" >::
        (fun () ->
          "empty" @? (S.check_prefix "" "a" = false);
          "singleton" @? (S.check_prefix "H" "H" = true);
          "simple" @? (S.check_prefix "Hello" "He" = true);
          "simplefalse" @? (S.check_prefix "HelloFoo" "lo" = false);
        );
      "concat_array" >::
        (fun () ->
          "empty" @? (S.concat_array ~sep:":" [||] = "");
          "empty singleton" @? (S.concat_array ~sep:":" [|""|] = "");
          "singleton" @? (S.concat_array ~sep:":" [|"Hello"|] = "Hello");
          "Words" @? (S.concat_array ~sep:" " [|"Hello"; "World"; "!"|] = "Hello World !");
        );
      "unescaped" >::
        (fun () ->
           unescaped_test ~name:"empty" "";
           repeat 50 (unescaped_test ~name:"random") sg;
           "hex" @? (S.unescaped "\\xff" = "\xff");
           "strict illegal escape" @?
             (try ignore (S.unescaped "\\a"); false
              with Invalid_argument _ -> true);
           "non strict" @? (S.unescaped ~strict:false "\\a" = "\\a");
           "non-strict illegal escape" @?
             (try ignore (S.unescaped ~strict:false "\\512"); false
              with Invalid_argument _ -> true)
        )
    ]
