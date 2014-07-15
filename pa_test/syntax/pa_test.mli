(* pa_test defines three quotation expanders, which can be used to reduce boilerplate
   in testing code:

   - <:test_eq< type >> that expands to a function of type
     [ ?here:Lexing.position list -> ?(equal = (fun x y -> <:compare< type >> x y = 0)) -> type -> type -> unit]
     that throws a nice exception if the values are not equal according to [equal].

   - <:test_pred< type >> that expands to a function of type
     [?here:Lexing.position list -> (type -> bool) -> type -> unit]
     that throws a nice exception if the predicate fails on the value.

   - <:test_result< type >> that expands to a function of type
     [ ?here:Lexing.position list -> ?(equal = (fun x y -> <:compare< type >> x y = 0)) -> type -> expected:type -> unit]
     that throws a nice exception if the values are not equal according to [equal].
     The difference with <:test_eq<>> is that the exception thrown notes which value
     was the "expected" value and which one was the one being tested against that value.

   See the docs repo for more info or ../test for examples.

   For users outside of jane street, please note that the generated code calls Core_kernel
   and sexplib.
*)
