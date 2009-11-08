(*pp cpp *)

open OUnit;;


let unused_test = Timer_test.test

let all =
  TestList
    [
      Bag_test.test;
      (* Bigbuffer_test.test; *)
      (* Bigstring_test.test; *)
      Binary_packing_test.test;
      Common_test.test;
      Comparable_test.test;
      Crc_test.test;
      Doubly_linked_test.test;
      Filename_test.test;
      Fqueue_test.test;
      Hashtbl_test.test;
      Hash_queue_test.test;
      Heap_test.test;
      Interval_test.test;
      Int_conversions_test.test;
      Hash_heap_test.test;
      Core_int_test.test;
      Core_array_test.test;
      Core_map_test.test;
      Core_set_test.test;
      Core_list_test.test;
      Core_queue_test.test;
      Core_string_test.test;
      Core_mutex_test.test;
      PMap_test.test;
      PSet_test.test;
      Time_test.test;
      TZ_test.test;
      Int_set_test.test;
      Core_float_test.test;
    ]
