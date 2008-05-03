open OUnit;;
open Core.Std

let test =
  "hash_queue" >:::
    [
      "basic" >:: (fun () ->
        let module Hq_arg = struct
          include String
          let hash (x : t) = Hashtbl.hash x
          let equal (x : t) (y : t) = x = y
          let sexp_of_t = Sexplib.Conv.sexp_of_string
          let t_of_sexp = Sexplib.Conv.string_of_sexp
        end in
        let module Hq = Hash_queue.Make (Hq_arg) in
        let hq = Hq.create () in
        let inv () = Hq.invariant hq in
        inv ();
        assert (Hq.is_empty hq);
        assert
          ([] = Hq.fold_keys hq ~init:[] ~f:(fun ac ~key:_ ~data:_ -> () :: ac));
        assert ([] = Hq.fold hq ~init:[] ~f:(fun ac _ -> () :: ac));
        let n = 10 in
        for i = 1 to n do 
          assert (Hq.enqueue hq (string_of_int i) i = `Ok); 
          inv ();
        done;
        assert (Hq.length hq = n);
        assert
          (List.rev
              (Hq.fold_keys hq ~init:[] ~f:(fun ac ~key ~data -> (key, data) :: ac))
              = List.init n ~f:(fun i -> let i = i + 1 in (string_of_int i, i)));
        assert
          (List.rev (Hq.fold hq ~init:[] ~f:(fun ac data -> data :: ac))
              = List.init n ~f:(fun i -> i + 1));
        Hq.iter_keys hq ~f:(fun ~key ~data -> assert (key = string_of_int data));
        let sum = ref 0 in
        Hq.iter hq ~f:(fun x -> sum := !sum + x);
        assert (!sum = (n * (n + 1) / 2));
        assert (Hq.mem hq "1");
        ignore (Hq.dequeue hq);
        inv ();
        assert (not (Hq.mem hq "1"));
        assert (Hq.length hq = n - 1);
        assert (Hq.remove hq (string_of_int n) = `Ok);
        inv ();
        assert (Hq.length hq = n - 2);
        let num = ref 0 in
        Hq.dequeue_all hq ~f:(fun _ -> num := !num + 1);
        inv ();
        assert (!num = n - 2);
        assert (Hq.is_empty hq);
        inv ();
        Hq.clear hq;
        assert (Hq.is_empty hq);
        for i = 1 to 100 do 
          assert (Hq.enqueue hq (string_of_int i) i = `Ok); 
        done;
        Hq.clear hq;
        assert (Hq.is_empty hq);
      )
    ]
