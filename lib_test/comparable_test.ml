open OUnit;;
open Core.Std

module F
  (M : sig
    type t
    include Comparable.S with type comparable = t
    val one : t
    val two : t
    val three : t
  end) : sig
  val test : OUnit.test
end = struct
  open M
  let equal = M.(=)
  let (=) = Bool.equal

  let test =
    let foreach f = f one; f two; f three in
    "all" >::
      (fun () ->
        foreach (fun a ->
          foreach (fun b ->
            assert (equal (min a b) (min b a));
            assert (equal (max a b) (max b a));
            assert ((a < b) = (Int.(<) (compare a b) 0));
            assert ((equal a b) = (Int.equal (compare a b) 0));
            assert ((a > b) = (Int.(>) (compare a b) 0));
            assert ((a < b) = (Int.(<) (ascending a b) 0));
            assert ((equal a b) = (Int.equal (ascending a b) 0));
            assert ((a > b) = (Int.(>) (ascending a b) 0));
            assert ((a > b) = (Int.(<) (descending a b) 0));
            assert ((equal a b) = (Int.(=) (descending a b) 0));
            assert ((a < b) = (Int.(>) (descending a b) 0));
            assert ((a > b) = (b < a));
            assert ((a >= b) = (b <= a));
            assert ((a < b) = not (a >= b));
            assert ((a > b) = not (a <= b));
            assert ((equal a b) = not (a <> b));
            assert ((a >= b) = (a > b || equal a b));
            assert ((a <= b) = (a < b || equal a b))));
        assert (equal one one);
        assert (equal two two);
        assert (equal three three);
        assert (not (one < one));
        assert (one < two);
        assert (one < three);
        assert (not (two < one));
        assert (not (two < two));
        assert (two < three);
        assert (not (three < one));
        assert (not (three < two));
        assert (not (three < three));
        assert (equal (max one one) one);
        assert (equal (max one two) two);
        assert (equal (max one three) three);
        assert (equal (max two two) two);
        assert (equal (max two three) three);
        assert (equal (max three three) three);
        assert (equal (min one one) one);
        assert (equal (min one two) one);
        assert (equal (min one three) one);
        assert (equal (min two two) two);
        assert (equal (min two three) two);
        assert (equal (min three three) three);
      )
  ;;
end

module Float =
  F (struct
    include Float
    let one = 1.0
    let two = 2.0
    let three = 3.0
  end)

module String =
  F (struct
     include String
     let one = "a"
     let two = "b"
     let three = "c"
  end)

module Span =
  F (struct
    include Time.Span
    let one = min_value
    let two = zero
    let three = max_value
  end)

module Ofday =
  F (struct
    include Time.Ofday
    let one = of_sec 1.
    let two = of_sec 2.
    let three = of_sec 3.
  end)

module Date =
  F (struct
    include Time.Date
    let one = Time.now ()
    let two = Time.add one (Time.Span.of_hr 30.0)
    let three = Time.add two (Time.Span.of_hr 30.0)
    let one = Time.to_date one
    let two = Time.to_date two
    let three = Time.to_date three
  end)

module Time =
  F (struct
    include Time
    let one = Time.now ()
    let two = Time.add one (Time.Span.of_sec 1.)
    let three = Time.add two (Time.Span.of_sec 1.)
  end)

module Int32 =
  F (struct
    include Int32
    let one = 1l
    let two = 2l
    let three = 3l
  end)

module Int64 =
  F (struct
    include Int64
    let one = 1L
    let two = 2L
    let three = 3L
  end)

module Nativeint =
  F (struct
    include Nativeint
    let one = of_int 1
    let two = of_int 2
    let three = of_int 3
  end)

module Int' =
  F (struct
    type t = int
    include Comparable.From_compare (Int)
    let one = 1
    let two = 2
    let three = 3
  end)

module Int'' =
  F (struct
    type t = int
    include
      Comparable.Inherit (Int) (struct type t = int let component x = x end)
    let one = 1
    let two = 2
    let three = 3
  end)

module Int =
  F (struct
    include Int
    let one = 1
    let two = 2
    let three = 3
  end)

let test =
  TestList
  [Float.test;
   String.test;
   Float.test;
   Span.test;
   Ofday.test;
   Date.test;
   Time.test;
   Int.test;
   Int32.test;
   Int64.test;
   Nativeint.test;
   Int'.test;
   Int''.test;
  ]
;;
