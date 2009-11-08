open Core.Std
open Core_extended.Std
open OUnit

let test = "Rmap" >::: [
  "insert-find-remove" >:: (fun () ->
    let t = Rmap.create () in
    let inserted = Hash_set.create 5_000 in
    Random.self_init ();
    let verify_inserted () =
      if
        Hash_set.fold ~init:true inserted ~f:(fun acc key ->
          acc && (Option.is_some (Rmap.find t key)))
      then ()
      else failwith "some inserts are missing"
    in
    for i = 1 to 2_000 do
      let k = Random.int 10_000 in
      Hash_set.add inserted k;
      Rmap.add t ~key:k ~data:();
      Rmap.invariant t;
      verify_inserted ();
    done;
    List.iter (Hash_set.to_list inserted) ~f:(fun x ->
      Rmap.remove t x;
      Rmap.invariant t;
      begin match Rmap.find t x with
      | None -> ()
      | Some _ -> failwith (sprintf "present after removal: %d" x)
      end;
      Hash_set.remove inserted x;
      verify_inserted ()))
]
