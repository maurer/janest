open OUnit;;
open Core.Std

let test_data = [("a",1);("b",2);("c",3)]
let (empty_hash:(string,int) Hashtbl.t) = Hashtbl.create 10
let test_hash = begin
    let h = Hashtbl.create 10 in
    List.iter test_data ~f:(fun (k,v) -> 
        Hashtbl.add h ~key:k ~data:v
      );
    h
  end

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
            "all_vals" @? 
              (let predicted = List.map test_data ~f:(fun (_,v) -> v) in
              let found = ref [] in
              Hashtbl.iter_vals test_hash ~f:(fun v -> found := v :: !found);
              (List.rev !found) = predicted)
          );
      "of_alist" >::
          (fun () -> 
            "size" @? 
              (let predicted = List.length test_data in
              let found = Hashtbl.length (Hashtbl.of_alist test_data) in
              predicted = found);
            "right keys" @? 
              (let predicted = List.map test_data ~f:(fun (k,_) -> k) in
              let found = Hashtbl.keys (Hashtbl.of_alist test_data) in
              let sp = List.sort ~cmp:ascending predicted in
              let sf = List.sort ~cmp:ascending found in
              sp = sf)
          );
      "keys" >::
          (fun () -> 
            "size and right keys" @? 
              (let predicted = List.map test_data ~f:(fun (k,_) -> k) in
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
    ]
    
