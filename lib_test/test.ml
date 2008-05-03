(*pp cpp *)

open OUnit;;

let all =
  TestList
    [
      Bag_test.test;
      Bigbuffer_test.test;
      Bigstring_test.test;
      Binary_packing_test.test;
      Common_test.test;
      Comparable_test.test;
      Crc_test.test;
      Doubly_linked_test.test;
      Fqueue_test.test;
      Hashtbl_test.test;
      Hash_queue_test.test;
      Heap_test.test;
      Interval_test.test;
      Int_conversions_test.test;
      Core_array_test.test;
      Core_list_test.test;
      Core_queue_test.test;
      Core_string_test.test;
#ifdef __linux__
      Linux_ext_test.test;
#endif
      PMap_test.test;
      PSet_test.test;
      Time_test.test;
    ]
