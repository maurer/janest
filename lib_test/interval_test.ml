open OUnit;;
open Core.Std

let test = 
  "interval" >:::
    [ "is_empty_or_singleton" >:: 
        (fun () -> 
          let t x = Interval.is_empty_or_singleton x in
          let i = Interval.make in
          "singleton1" @? t (i 0 0);
          "singleton2" @? t (i 10 10);
          "singleton3" @? t (i "foo" "foo");
          "empty1" @? t (i 1 0);
          "nonempty" @? not (t (i 0 1));
        );
      "are_disjoint_as_open_intervals" >:: 
        (fun () -> 
          let t x = Interval.are_disjoint_as_open_intervals x in
          let i = Interval.make in
          "touching" @? t [i 3 4; i 4 5];
          "not touching" @? t [i 3 4; i 5 6];
          "overlapping" @? not (t [i 3 5; i 4 6]);
        );
    ]

