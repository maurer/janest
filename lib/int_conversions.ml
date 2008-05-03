type 'a int_spec = {
  name : string;
  num_bits : int;
  max : 'a;
  min : 'a;
  to_string : 'a -> string;
  
}

let convert a b a_to_b b_to_a =
  if a.num_bits <= b.num_bits then
    ((fun i -> Some (a_to_b i)), a_to_b)
  else
    let min = b_to_a b.min in
    let max = b_to_a b.max in
    let is_in_range i = min <= i && i <= max in
    let convert i = if is_in_range i then Some (a_to_b i) else None in
    let convert_exn i =
      if is_in_range i then
        a_to_b i
      else
        failwith (Printf.sprintf
                     "conversion from %s to %s failed: %s is out of range"
                     a.name b.name (a.to_string i))
    in
    (convert, convert_exn)
;;

let int = {
  name = "int";
  num_bits = Word_size.num_bits Word_size.word_size - 1;
  max = max_int;
  min = min_int;
  to_string = string_of_int;
}

let int32 = {
  name = "int32";
  num_bits = 32;
  max = Int32.max_int;
  min = Int32.min_int;
  to_string = Int32.to_string;
}

let int64 = {
  name = "int64";
  num_bits = 64;
  max = Int64.max_int;
  min = Int64.min_int;
  to_string = Int64.to_string;
}

let nativeint = {
  name = "nativeint";
  num_bits = Word_size.num_bits Word_size.word_size;
  max = Nativeint.max_int;
  min = Nativeint.min_int;
  to_string = Nativeint.to_string;
}

let (int_to_int32, int_to_int32_exn) =
  convert int int32 Int32.of_int Int32.to_int
let (int32_to_int, int32_to_int_exn) =
  convert int32 int Int32.to_int Int32.of_int

let int_to_int64 = Int64.of_int
let (int64_to_int, int64_to_int_exn) =
  convert int64 int Int64.to_int Int64.of_int

let int_to_nativeint = Nativeint.of_int
let (nativeint_to_int, nativeint_to_int_exn) =
  convert nativeint int Nativeint.to_int Nativeint.of_int

let int32_to_int64 = Int64.of_int32
let (int64_to_int32, int64_to_int32_exn) =
  convert int64 int32 Int64.to_int32 Int64.of_int32

let int32_to_nativeint = Nativeint.of_int32
let (nativeint_to_int32, nativeint_to_int32_exn) =
  convert nativeint int32 Nativeint.to_int32 Nativeint.of_int32
;;

let (int64_to_nativeint, int64_to_nativeint_exn) =
  convert int64 nativeint Int64.to_nativeint Int64.of_nativeint
;;
let nativeint_to_int64 = Int64.of_nativeint
