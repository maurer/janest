open OUnit;;
open Core.Std



let test_data = [("a",1);("b",2);("c",5);("c",4);("c",3);]
let visible_bindings = [("a",1);("b",2);("c",3);]

let string_of_alist a =
  let sexp_of_tuple t = Tuple2.sexp_of_t String.sexp_of_t Int.sexp_of_t t in
  let sexp = List.sexp_of_t sexp_of_tuple a in Sexp.to_string_hum sexp

let (empty_hash:(string,int) Hashtbl.t) = Hashtbl.create 10
let test_hash = begin
    let h = Hashtbl.create 10 in
    List.iter test_data ~f:(fun (k,v) -> 
        Hashtbl.shadow_add h ~key:k ~data:v
      );
    h
  end

(* This is a very strong notion of equality on hash tables - it is "unsafe" because
 * it observes differences between shadowed bindings in the two tables. *)
let unsafe_equal t t' equal_data =
  (* true iff all the bindings in [t] appear in [t']. Shadowed bindings for a key must
   * be in the same order. *)
  let subtable t t' = 
    try
      List.for_all (Hashtbl.keys t) ~f:(fun key ->
        List.for_all2 
          (Hashtbl.shadow_find t key) 
          (Hashtbl.shadow_find t' key) 
          ~f:equal_data)
    with
    | Invalid_argument _ -> false
  in
  subtable t t' && subtable t' t

let test = 
  "Hashtbl" >:::
    [ "find" >::
        (fun () -> 
          "no_exception" @? 
            let found = Hashtbl.find test_hash "a" in
            let not_found = Hashtbl.find test_hash "A" in
            match found,not_found with
            | Some _, None -> true
            | _ -> false
        );
      "iter_vals" >::
          (fun () -> 
            let predicted = List.sort ~cmp:Int.descending (
              List.map test_data ~f:(fun (_,v) -> v)) 
            in
            let found = ref [] in
            Hashtbl.iter_vals test_hash ~f:(fun v -> found := v :: !found);
            (sprintf "all_vals: Expected: %s\nFound: %s" 
              (List.to_string Int.to_string predicted)
              (List.to_string Int.to_string !found))
            @? ( !found = predicted )
          );
      "of_alist" >::
          (fun () -> 
            let test_data = visible_bindings in
            "size" @? 
              (let predicted = List.length test_data in
              let found = Hashtbl.length (Hashtbl.of_alist_exn test_data) in
              predicted = found);
            "right keys" @? 
              (let predicted = List.map test_data ~f:(fun (k,_) -> k) in
              let found = Hashtbl.keys (Hashtbl.of_alist_exn test_data) in
              let sp = List.sort ~cmp:ascending predicted in
              let sf = List.sort ~cmp:ascending found in
              sp = sf)
          );
      "of_alist_multi" >::
          (fun () -> 
            let test_data' = visible_bindings in
            let test_data = test_data' @ test_data' in
            "size" @? 
              (let predicted = List.length test_data / 2 in
              let found = Hashtbl.length (Hashtbl.of_alist_shadow test_data) in
              predicted = found);
            "right keys" @? 
              (let predicted = List.dedup (List.map test_data ~f:(fun (k,_) -> k)) in
              let found = Hashtbl.keys (Hashtbl.of_alist_shadow test_data) in
              let sp = List.sort ~cmp:ascending predicted in
              let sf = List.sort ~cmp:ascending found in
              sp = sf);
            "right data" @? 
              (let predicted = (List.map test_data' ~f:(fun (_,d) -> [d;d])) in
              let found = Hashtbl.data (Hashtbl.of_alist_shadow test_data) in
              let sp = List.sort ~cmp:ascending predicted in
              let sf = List.sort ~cmp:ascending found in
              sp = sf);
          );
      "keys" >::
          (fun () -> 
            "size and right keys" @? 
              (let predicted = List.map visible_bindings ~f:(fun (k,_) -> k) in
              let found = Hashtbl.keys test_hash in
              let sp = List.sort ~cmp:ascending predicted in
              let sf = List.sort ~cmp:ascending found in
              sp = sf)
          );
      "data" >::
          (fun () -> 
            "size and right data" @? 
              (let predicted = List.map test_data ~f:(fun (_,v) -> v) in
              let found = Hashtbl.data test_hash in
              let sp = List.sort ~cmp:ascending predicted in
              let sf = List.sort ~cmp:ascending found in
              sp = sf)            
          );

      "map" >:: (fun () ->
        let add1 x = x + 1 in
        let predicted_data = 
          List.sort ~cmp:ascending (List.map test_data ~f:(fun (k,v) -> (k,add1 v))) 
        in
        let found = Hashtbl.map test_hash ~f:add1 in
        let found_alist = List.sort ~cmp:ascending (Hashtbl.to_alist found) in
        "size" @? ( List.length test_data = Hashtbl.length found );
        let title = 
          sprintf "right_data:\nExpected: %s\nFound: %s" 
            (string_of_alist predicted_data)
            (string_of_alist found_alist)
        in 
        title @? (predicted_data = found_alist));

      "filter_map" >:: (fun () ->
        begin
          let to_string h = Sexp.to_string_hum (
            Hashtbl.sexp_of_t String.sexp_of_t Int.sexp_of_t h)
          in
          let f x = Some x in
          let result = Hashtbl.filter_map test_hash ~f in
          (sprintf "Result is identical: Expected: %s\nFound: %s" 
            (to_string test_hash)
            (to_string result))
          @? ( unsafe_equal test_hash result Int.(=) )
        end;
        let is_even x = x mod 2 = 0 in
        let add1_to_even x = if is_even x then Some (x + 1) else None in
        let predicted_data = List.filter_map test_data ~f:(fun (k,v) -> 
          if is_even v then Some (k, v+1) else None) 
        in
        let found = Hashtbl.filter_map test_hash ~f:add1_to_even in
        let found_alist = List.sort ~cmp:ascending (Hashtbl.to_alist found) in
        "size and right data" @? (
          List.length predicted_data = Hashtbl.length found
          && predicted_data = found_alist));
    ]
    
