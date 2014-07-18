open OUnit
open Core.Std

(* This is to test that the two types unify as expected.  If it compiles, it passes. *)
let (_ : Re2.Std.Re2.t) = Re2.Regex.create_exn "."

module Re2 = Re2.Std.Re2

let default_pat = "foo((?P<baz>baz)|bar)"
let default_re = Re2.create_exn default_pat
let default_input = "foobarAfoobazBfoobarfoobazC"

let with_re ?pat ?err f = fun () ->
  let re =
    match pat with
    | None -> default_re
    | Some pat -> Re2.create_exn pat
  in
  match err with
  | None -> ignore (f re)
  | Some err -> assert_raises err (fun () -> f re)
;;

let assert_str_equal ~expected got =
  let msg = Format.sprintf "expected '%s' got '%s'\n" expected got in
  assert_equal ~msg expected got
;;

let assert_strings_equal ~expected got =
  match List.zip expected got with
  | None -> assert_failure (String.concat ~sep:"|" got)
  | Some l -> List.iter l ~f:(fun (expected, got) ->
    assert_str_equal ~expected got)

(*
let str_of_str_list l =  Sexp.to_string_hum (List.sexp_of_t String.sexp_of_t l)
*)

let _ = run_test_tt_main ("creation" >::: [
  (** creation *)
  "create" >:: (fun () -> ignore (Re2.create "(foo)|(b[a-z]r)b.*zqu+x"));
  "create-invalid" >:: (fun () ->
    assert_bool "create-invalid"
      (try
        let _ = (Re2.create_exn "[" : Re2.t) in
        false
      with
      | _ -> true));
  "dot-nl-default" >:: with_re ~pat:"abc." (fun re ->
    assert_bool "" (Re2.matches re "abcd");
    assert_bool "" (not (Re2.matches re "abc\n"));
  );
  "dot-nl-explicit" >:: (fun () ->
    let re = Re2.create_exn ~options:[`Dot_nl true] "abc." in
    assert_bool "" (Re2.matches re "abcd");
    assert_bool "" (Re2.matches re "abc\n");
  );
  "case-sensitive-default" >:: with_re ~pat:"Foo" (fun re ->
      assert_bool "" (Re2.matches re "Foo");
      assert_bool "" (not (Re2.matches re "foo")));
  "case-sensitive-explicit" >:: (fun () ->
    let re = Re2.create_exn ~options:[`Case_sensitive true] "Foo" in
    assert_bool "" (Re2.matches re "Foo");
    assert_bool "" (not (Re2.matches re "foo")));
  "case-insensitive" >:: (fun () ->
    let re = Re2.create_exn ~options:[`Case_sensitive false] "Foo" in
    assert_bool "" (Re2.matches re "Foo");
    assert_bool "" (Re2.matches re "foo"));

  (** accessors *)
  "pattern" >:: with_re (fun re ->
    assert_str_equal ~expected:default_pat (Re2.pattern re));
  "num_submatches" >:: with_re (fun re ->
    assert_equal 3 (Re2.num_submatches re));
  (* "submatch_index" >:: with_re (fun re ->
   *   assert_equal 2 (Re2.submatch_index_exn re "baz"));
   * "bad-submatch_index" >:: with_re (fun re ->
   *   assert_bool "bad-submatch_index"
   *     (try
   *       let _ = Re2.submatch_index_exn re "boo" in
   *       false
   *     with
   *     | _ -> true)); *)

  (** find *)
  "find" >:: with_re (fun re ->
    assert_str_equal ~expected:"foobar" (Re2.find_first_exn ~sub:(`Index 0) re "foobarbaz"));
  "find-failed" >:: with_re (fun re ->
    assert_bool ""
      (try
        let _ = Re2.find_first_exn re "barfoo" in
        false
      with
      | _ -> true));
  "find-empty-pattern" >:: with_re ~pat:"a*" (fun re ->
    let matches = Re2.find_all_exn re "foobarbaaz" in
    assert_equal (String.length "foobarbaaz" - 1) (List.length matches);
    List.iteri matches ~f:(fun i m ->
      if i = 4 then assert_str_equal ~expected:"a" m
      else if i = 7 then assert_str_equal ~expected:"aa" m
      else assert_str_equal ~expected:"" m));
  "find-substring" >:: with_re (fun re ->
    assert_str_equal ~expected:"bar" (Re2.find_first_exn ~sub:(`Index 1) re "foobarbaz"));
  "find-invalid-substring" >:: with_re (fun re ->
    assert_bool ""
      (try
        let _ = Re2.find_first_exn ~sub:(`Index 3) re "foobarbaz" in
        false
      with
      | _ -> true));
  "find-uncaptured-substring" >:: with_re (fun re ->
    assert_bool ""
      (try
        let _ = Re2.find_first_exn ~sub:(`Index 2) re "foobarbaz" in
        false
       with
      | _ -> true));
  "find-empty-matches" >:: with_re ~pat:"q*" (fun re ->
    assert_str_equal ~expected:"" (Re2.find_first_exn re default_input));

  (** find_all **)
  "find_all-empty-matches" >:: with_re ~pat:"q*" (fun re ->
    let matches = Re2.find_all_exn re default_input in
    assert_equal (String.length default_input) (List.length matches);
    List.iter matches ~f:(fun m -> assert_str_equal ~expected:"" m));
  "find_all-request-submatch-did-not-capture" >:: with_re (fun re ->
    assert_strings_equal ~expected:[ "baz"; "baz" ]
      (Re2.find_all_exn ~sub:(`Name "baz") re default_input));

  (** find_submatches **)
  "find_submatches" >:: with_re (fun re ->
    let expected =  [| Some "foobar"; Some "bar"; None |] in
    let matches = Re2.find_submatches_exn re default_input in
    assert_equal (Array.length expected) (Array.length matches);
    Array.iteri matches ~f:(fun i m -> assert_equal (Array.get expected i) m));
  "find_submatches-failure" >:: with_re ~pat:"q"
    ~err:(Re2.Exceptions.Regex_match_failed "q")
    (fun re -> Re2.find_submatches_exn re default_input);

  (** matches *)
  "has-matches" >:: with_re (fun re ->
    assert_bool "" (Re2.matches re "foobaz");
    assert_bool "" (not (Re2.matches re "bazfoo")));
  "get-matches" >:: with_re (fun re ->
    match Re2.get_matches_exn re "foobar foobaz foobazbar" with
    | [m1; m2; m3] ->
      List.iter ~f:(fun (expected, n, m) -> assert_str_equal ~expected
        (Re2.Match.get_exn ~sub:(`Index n) m))
        [("foobar", 0, m1); ("bar", 1, m1); (* check m1 sub 2 separately *)
         ("foobaz", 0, m2); ("baz", 1, m2); ("baz", 2, m2);
         ("foobaz", 0, m3); ("baz", 1, m3); ("baz", 2, m3) ];
      assert_bool ""
        (try
          let _ = Re2.Match.get_exn ~sub:(`Index 2) m1 in
          false
        with
        | _ -> true)
    | _ -> assert_failure "incorrect number of matches");
  "get-two-matches" >:: with_re (fun re ->
    match Re2.get_matches_exn ~max:2 re "foobar foobaz foobazbar" with
    | [_; _] -> ()
    | _ -> assert_failure "incorrect number of matches");
  "match-index-past-sub-limit" >:: with_re (fun re ->
    assert_bool ""
      (try
        let _ =
          List.map (Re2.get_matches_exn ~sub:(`Index 1) ~max:1 re "foobarfoobarfoobar")
            ~f:(fun m -> Re2.Match.get_exn ~sub:(`Index 2) m)
        in
        false
      with
      | _ -> true));
  "get-potentially-empty-matches" >:: with_re ~pat:"a*" (fun re ->
    match Re2.get_matches_exn re "ba" with
    | [ m1; m2; m3 ] ->
      assert_str_equal ~expected:"" (Re2.Match.get_exn ~sub:(`Index 0) m1);
      assert_str_equal ~expected:"a" (Re2.Match.get_exn ~sub:(`Index 0) m2);
      assert_str_equal ~expected:"" (Re2.Match.get_exn ~sub:(`Index 0) m3)
    | _ -> assert_failure "wrong number of matches");
  "named-submatch" >:: with_re (fun re ->
    let f m = Re2.Match.get_exn ~sub:(`Name "baz") m in
    match Re2.get_matches_exn re "foobarfoobazfoobar" with
    | [ m1; m2; m3] ->
      assert_bool "" (try let _ = f m1 in false with _ -> true);
      assert_equal "baz" (f m2);
      assert_bool "" (try let _ = f m3 in false with _ -> true);
    | _ -> assert_failure "wrong number of matches");
  "invalid-named-submatch" >:: with_re (fun re ->
    match Re2.get_matches_exn re default_input with
    | [] -> assert_failure "wrong number of matches"
    | h::_ ->
      assert_bool ""
        (try
          let _ = Re2.Match.get_exn ~sub:(`Name "foo") h in
          false
        with
        | _ -> true));
  "bug-in-matches" >:: with_re ~pat:".*NY (open|close).*start*" (fun re ->
    assert_bool "" (not (Re2.matches re "huge state2011-08-26 15:38:15.970718-04:00")));
  "shortest-match-default" >:: with_re ~pat:"a(b|bb)" (fun re ->
    assert_str_equal ~expected:"ab" (Re2.find_first_exn re "abb"));
  "shortest-match-1" >:: with_re ~pat:"a(bb|b)" (fun re ->
    assert_str_equal ~expected:"abb" (Re2.find_first_exn re "abb"));
  "shortest-match-2" >:: with_re ~pat:"a(b|bb)" (fun re ->
    assert_str_equal ~expected:"ab" (Re2.find_first_exn re "ababb"));
  "longest-match" >:: (fun () ->
    let re = Re2.create_exn ~options:[`Longest_match true] "a(b|bb)" in
    assert_str_equal ~expected:"abb" (Re2.find_first_exn re "abb"));
  "empty-match" >:: with_re ~pat:"q*" (fun re ->
    let matches = Re2.get_matches_exn re default_input in
    let n_expected, n_got = (String.length default_input + 1, List.length matches) in
    assert_equal n_expected n_got
      ~msg:(Printf.sprintf !"expected %d got %d matches: %{sexp:Re2.Match.t list}"
              n_expected n_got matches) ;
    List.iteri matches ~f:(fun i m ->
      let pos, len = Re2.Match.get_pos_exn ~sub:(`Index 0) m in
      assert_equal i pos;
      assert_equal 0 len));

  (** splitting *)
  "split" >:: with_re (fun re ->
    match Re2.split re default_input with
    | [ ""; "A"; "B"; ""; "C" ] -> ()
    (*
    | l -> assert_bool (str_of_str_list l) false);
    *)
    | l -> assert_failure (String.concat ~sep:"\t" l));
  "split-with-empty-last-field" >:: with_re (fun re ->
    match Re2.split re "Afoobaz" with
    | [ "A"; "" ] -> ()
    | l -> assert_failure (String.concat ~sep:"\t" l));
  "split-with-matches" >:: with_re (fun re ->
    match Re2.split re default_input ~include_matches:true with
    | [ ""; "foobar"; "A"; "foobaz"; "B"; "foobar"; ""; "foobaz"; "C" ] -> ()
    | l -> assert_failure (Sexp.to_string_hum (<:sexp_of< string list >> l)));

  (** replacements *)
  "replace" >:: with_re (fun re ->
    assert_str_equal ~expected:"barAbazBbarbazC"
    (Re2.replace_exn re default_input ~f:(fun m ->
      (Re2.Match.get_exn ~sub:(`Index 1) m))));
  "replace-only" >:: with_re (fun re ->
    assert_str_equal ~expected:"foobarA__BfoobarfoobazC"
    (Re2.replace_exn ~only:1 re default_input ~f:(fun _ -> "__")));
  "replace-returns-None" >:: with_re (fun re ->
    assert_str_equal ~expected:default_input
    (Re2.replace_exn re default_input ~f:(fun m ->
      Re2.Match.get_exn ~sub:(`Index 0) m)));

  (** rewrites *)
  "rewrite" >:: with_re (fun re ->
    match Re2.rewrite re ~template:"\\1" default_input with
    | Ok retval -> assert_str_equal ~expected:"barAbazBbarbazC" retval
    | Error err -> Error.raise err);
  "rewrite-invalid-template" >:: with_re (fun re ->
    match Re2.rewrite re ~template:"\\3" default_input with
    | Ok _ -> assert_failure "expected error"
    | Error _ -> ());
  "valid-rewrite-string" >:: with_re (fun re ->
    assert_equal true (Re2.valid_rewrite_template re ~template:"\\1"));
  "invalid-rewrite-string" >:: with_re (fun re ->
    assert_equal false (Re2.valid_rewrite_template re ~template:"\\6"));

  (** escaping *)
  "escape" >:: (fun () ->
    assert_bool "unescaped" (Re2.matches (Re2.create_exn "1.5-2.0?") "105-2x");
    let re = Re2.create_exn (Re2.escape "1.5-2.0?") in
    assert_bool "escaped as pattern" (not (Re2.matches re "105-2x"));
    assert_bool "escaped matches unescaped" (Re2.matches re "1.5-2.0?"));

  (** custom operations *)
  "polymorphic-equality" >:: with_re (assert_equal (Re2.create_exn default_pat));
  "marshalling" >:: with_re (fun re ->
    assert_equal re (Marshal.from_string (Marshal.to_string re []) 0));
  "marshalling-nonstandard-options" >:: (fun () ->
    let re = Re2.create_exn ~options:[`Posix_syntax true] "foo" in
    assert_equal (Marshal.from_string (Marshal.to_string re []) 0) re);

  (** submatch numbering *)
  "sub-find_all" >:: with_re (fun re ->
    assert_strings_equal (Re2.find_all_exn re default_input)
      ~expected:[ "foobar"; "foobaz"; "foobar"; "foobaz" ]);
  "sub-find_all-1" >:: with_re (fun re ->
    assert_strings_equal (Re2.find_all_exn re default_input ~sub:(`Index 1))
      ~expected:[ "bar"; "baz"; "bar"; "baz" ]);
  "sub-find_all-baz" >:: with_re (fun re ->
    assert_strings_equal (Re2.find_all_exn re default_input ~sub:(`Name "baz"))
      ~expected:[ "baz"; "baz" ]);
  (* "sub-Iterator.next" >:: with_re (fun re ->
   *   let it = Re2.Iterator.create re ~input:"foobar" in
   *   let m = Re2.Iterator.next_exn ~sub:(`Index 1) it in
   *   assert_str_equal ~expected:"foobar" (Re2.Match.get_exn ~sub:(`Index 0) m);
   *   assert_str_equal ~expected:"bar" (Re2.Match.get_exn ~sub:(`Index 1) m)); *)
  "sub-get_matches-1" >:: with_re (fun re ->
    match Re2.get_matches_exn ~sub:(`Index 1) re "foobar foobaz" with
    | [ m1; m2 ] ->
      assert_str_equal ~expected:"foobar" (Re2.Match.get_exn ~sub:(`Index 0) m1);
      assert_str_equal ~expected:"bar" (Re2.Match.get_exn ~sub:(`Index 1) m1);
      assert_str_equal ~expected:"foobaz" (Re2.Match.get_exn ~sub:(`Index 0) m2);
      assert_str_equal ~expected:"baz" (Re2.Match.get_exn ~sub:(`Index 1) m2)
    | _ -> assert_failure "");
  "sub-get_matches-0-failing" >:: with_re (fun re ->
    let matches = Re2.get_matches_exn ~sub:(`Index 0) re default_input in
    assert_equal 4 (List.length matches);
    List.iter matches ~f:(fun m ->
      try assert_failure (Re2.Match.get_exn ~sub:(`Index 1) m) with
      | Re2.Exceptions.Regex_no_such_subpattern (_, 1) -> ()));
  (* not testing replace because it gets its Match.t values from get_matches *)
]) ;;
