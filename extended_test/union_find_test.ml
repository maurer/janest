open OUnit;;
open Core.Std
open Core_extended.Std

open Union_find

let test =
  "union_find" >:::
    [
      "all" >::
        (fun () ->
          let t1 = create 13 in
          assert (get t1 = 13);
          set t1 15;
          assert (get t1 = 15);
          assert (same_class t1 t1);
          let t2 = create 17 in
          assert (not (same_class t1 t2));
          union t1 t2;
          assert (same_class t1 t1);
          assert (same_class t1 t2);
          assert (same_class t2 t2);
          assert (get t1 = get t2);
          assert (get t1 = 15 || get t1 = 17);
          let t3 = create 19 in
          union t1 t3;
          let t4 = create 21 in
          assert (not (same_class t3 t4));
          union t1 t4;
          assert (same_class t3 t4);
        );
    ]
;;
