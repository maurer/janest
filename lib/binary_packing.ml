open Std_internal

module Char = Caml.Char
module Int32 = Caml.Int32
module Int64 = Caml.Int64

let pack_unsigned_8 ~buf ~pos n =
  if n > 0xFF || n < 0 then
    invalid_argf "pack_unsigned_8: argument '%d' out of range" n ()
  else buf.[pos] <- Char.unsafe_chr n;
;;

let unpack_unsigned_8 ~buf ~pos = Char.code buf.[pos]

let pack_signed_8 ~buf ~pos n =
  if n > 0x7F || n < -0x80 then
    invalid_argf "pack_signed_8: argument '%d' out of range" n ()
  else buf.[pos] <- Char.unsafe_chr (0xFF land n)

let unpack_signed_8 ~buf ~pos =
  let n = unpack_unsigned_8 ~buf ~pos in
  if n >= 0x80 then
    -(0x100 - n)
  else
    n

let pack_signed_16 ~buf ~pos n =
  if n > 0x7FFF || n < -0x8000 then
    invalid_argf "pack_signed_16: argument '%d' out of range" n ()
  else begin
    buf.[pos] <- Char.unsafe_chr (0xFF land (n asr 8));
    buf.[pos+1] <- Char.unsafe_chr (0xFF land n)
  end

let unpack_signed_16 ~buf ~pos =
  let n = (Char.code buf.[pos] lsl 8) lor (Char.code buf.[pos+1]) in
  if n >= 0x8000 then -(0x10000 - n)
  else n

let pack_signed_32_int ~buf ~pos n =
  assert (Sys.word_size = 64);
  if n > 0x7FFFFFFF || n < -(0x7FFFFFFF + 1) then
    invalid_argf "pack_signed_32_int: argument '%d' out of range" n ()
  else begin
    buf.[pos] <- Char.unsafe_chr (0xFF land (n asr 24));
    buf.[pos+1] <- Char.unsafe_chr (0xFF land (n asr 16));
    buf.[pos+2] <- Char.unsafe_chr (0xFF land (n asr 8));
    buf.[pos+3] <- Char.unsafe_chr (0xFF land n)
  end

let pack_signed_32 ~buf ~pos n =
  buf.[pos] <-
    Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 24));
  buf.[pos+1] <-
    Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 16));
  buf.[pos+2] <-
    Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 8));
  buf.[pos+3] <- Char.unsafe_chr (0xFF land Int32.to_int n)

let unpack_signed_32 ~buf ~pos =
  Int32.logor
    (Int32.shift_left (Int32.of_int (Char.code buf.[pos])) 24)
    (Int32.of_int
      ((Char.code buf.[pos+1] lsl 16)
        lor (Char.code buf.[pos+2] lsl 8)
        lor (Char.code buf.[pos+3])))

let unpack_signed_32_int ~buf ~pos =
  assert (Sys.word_size = 64);
  let n =
    (Char.code buf.[pos] lsl 24)
    lor (Char.code buf.[pos+1] lsl 16)
    lor (Char.code buf.[pos+2] lsl 8)
    lor (Char.code buf.[pos+3])
  in
  if n > 0x7FFFFFFF then -(((0x7FFFFFFF + 1) lsl 1) - n)
  else n

let pack_signed_64 ~buf ~pos v =
  let top3 = Int64.to_int (Int64.shift_right v 40) in
  let mid3 = Int64.to_int (Int64.shift_right v 16) in
  let bot2 = Int64.to_int v in
  buf.[pos] <- Char.unsafe_chr (0xFF land (top3 lsr 16));
  buf.[pos+1] <- Char.unsafe_chr (0xFF land (top3 lsr 8));
  buf.[pos+2] <- Char.unsafe_chr (0xFF land top3);
  buf.[pos+3] <- Char.unsafe_chr (0xFF land (mid3 lsr 16));
  buf.[pos+4] <- Char.unsafe_chr (0xFF land (mid3 lsr 8));
  buf.[pos+5] <- Char.unsafe_chr (0xFF land mid3);
  buf.[pos+6] <- Char.unsafe_chr (0xFF land (bot2 lsr 8));
  buf.[pos+7] <- Char.unsafe_chr (0xFF land bot2)

let unpack_signed_64 ~buf ~pos =
  Int64.logor
    (Int64.logor
      (Int64.shift_left
        (Int64.of_int (Char.code buf.[pos] lsl 16
                        lor Char.code buf.[pos+1] lsl 8
                        lor Char.code buf.[pos+2]))
        40)
      (Int64.shift_left
        (Int64.of_int (Char.code buf.[pos+3] lsl 16
                        lor Char.code buf.[pos+4] lsl 8
                        lor Char.code buf.[pos+5]))
        16))
    (Int64.of_int (Char.code buf.[pos+6] lsl 8
                    lor Char.code buf.[pos+7]))

let pack_signed_64_int ~buf ~pos n =
  assert (Sys.word_size = 64);
  buf.[pos] <- Char.unsafe_chr (0xFF land (n asr 56));
  buf.[pos+1] <- Char.unsafe_chr (0xFF land (n asr 48));
  buf.[pos+2] <- Char.unsafe_chr (0xFF land (n asr 40));
  buf.[pos+3] <- Char.unsafe_chr (0xFF land (n asr 32));
  buf.[pos+4] <- Char.unsafe_chr (0xFF land (n asr 24));
  buf.[pos+5] <- Char.unsafe_chr (0xFF land (n asr 16));
  buf.[pos+6] <- Char.unsafe_chr (0xFF land (n asr 8));
  buf.[pos+7] <- Char.unsafe_chr (0xFF land n)

let unpack_signed_64_int ~buf ~pos =
  assert (Sys.word_size = 64);
  let n = (Char.code buf.[pos] lsl 56)
    lor (Char.code buf.[pos+1] lsl 48)
    lor (Char.code buf.[pos+2] lsl 40)
    lor (Char.code buf.[pos+3] lsl 32)
    lor (Char.code buf.[pos+4] lsl 24)
    lor (Char.code buf.[pos+5] lsl 16)
    lor (Char.code buf.[pos+6] lsl 8)
    lor (Char.code buf.[pos+7])
  in
  n

let pack_float ~buf ~pos f = pack_signed_64 ~buf ~pos (Int64.bits_of_float f)

let unpack_float ~buf ~pos = Int64.float_of_bits (unpack_signed_64 ~buf ~pos)

let test () =
  let buf = String.make 8 'a' in
  let test name to_string p u ns =
    List.iter ns ~f:(fun n ->
      p ~buf ~pos:0 n;
      let n' = u ~buf ~pos:0 in
      if n <> n' then
        failwith (sprintf "%s = unpack_%s (pack_%s %s)"
                     (to_string n') name name (to_string n)))
  in
  test "signed_8" string_of_int
    pack_signed_8 unpack_signed_8 [-0x80; -0x7F; -0xF; -1; 0; 1; 0xF; 0x7F];
  test "signed_16" string_of_int
    pack_signed_16 unpack_signed_16
    [-0x8000; -0x7ABC; -0xFF; -1; 0; 1; 0xFF; 0x7ABC; 0x7FFF];
  test "signed_32" Int32.to_string
    pack_signed_32 unpack_signed_32
    [-0x80000000l; -0x76543210l; -0xFFl; Int32.minus_one; Int32.zero; Int32.one; 0x76543210l; 0x7FFFFFFFl];
  test "signed_64" Int64.to_string
    pack_signed_64 unpack_signed_64
    [-0x8000_0000_0000_0000L;
     -0x789A_BCDE_F012_3456L;
     -0xFFL;
     Int64.minus_one;
     Int64.zero;
     Int64.one;
     0x789A_BCDE_F012_3456L;
     0x7FFF_FFFF_FFFF_FFFFL]
