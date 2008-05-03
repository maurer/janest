open Core.Std
open OUnit;;

module B = Binary_packing

let inverses inc inc_inc pack unpack min max to_string () =
  let try_it i =
    let buf = String.create 100 in
    pack ~buf ~pos:0 i;
    let i' = unpack ~buf ~pos:0 in
    if i' <> i then
      failwith (Printf.sprintf "failed on input %s; returned %s"
                   (to_string i) (to_string i')) in

  let rec f i inc_by =
    if i < max then begin
      try_it i;
      let i' = inc inc_by i in
      if i' > i then
        f (inc inc_by i) (inc_by + inc_inc)
    end in
  f min 1;
  try_it max

let test = 
  "binary_packing" >:::
    [ "Binary_packing.test" >::
        (fun () ->
          let test () =
            try
              Binary_packing.test ();
              true
            with _ -> false in
          Quickcheck.laws_exn "\"Runs without raising\""
            1 (Quickcheck.always ()) test
        );

      "[pack|unpack]_signed_8" >::
        inverses (+) 0 B.pack_signed_8 B.unpack_signed_8
        (-0x80) 0x7F string_of_int;
      "[pack|unpack]_signed_16" >::
        inverses (+) 0 B.pack_signed_16 B.unpack_signed_16
        (-0x8000) 0x7FFF string_of_int;
      "[pack|unpack]_signed_32" >::
        inverses (fun n -> Int32.add (Int32.of_int_exn n)) 1 B.pack_signed_32 B.unpack_signed_32
        (Int32.of_string "-0x80000000") (Int32.of_string "0x7FFFFFFF") Int32.to_string;
      "[pack|unpack]_signed_32_int" >::
        if Sys.word_size = 8 then
          inverses (+) 1 B.pack_signed_32_int B.unpack_signed_32_int
            (int_of_string "-0x80000000") (int_of_string "0x7FFFFFFF") string_of_int
        else
          (fun () -> ());
      
      "[pack|unpack]_float" >::
        (fun () ->
          let test_float i =
            let buf = String.create 100 in
            B.pack_float ~buf ~pos:0 i;
            let i' = B.unpack_float ~buf ~pos:0 in
            i' = i in
          Quickcheck.laws_exn "unpack_float (pack_float x) = x"
            100 Quickcheck.fg test_float
        );
    ]
