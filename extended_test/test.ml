(*pp cpp $ARCH_FLAGS *)
open OUnit;;

let all = TestList [
  Cbuffer_test.test;
  Condition_test.test;
  Extended_array.test;
  Extended_float.test;
  Extended_list.test;
  Extended_string.test;
  Escaping_test.test;
#ifdef __linux__
  Extended_linux_test.test;
#endif
  Int_set_test.test;
  Iter_test.test;
  Lru_test.test;
  Shell_test.test;
  Union_find_test.test;
  Rmap_test.test;
]
