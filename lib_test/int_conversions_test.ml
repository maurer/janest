open OUnit;;
open Core.Std

module Examples (I : Int_intf.S) = struct
  open I
  let two = one + one

  let examples =
    [min_int; min_int + one; min_int + two; div min_int two; neg two; neg one;
     zero;
     one; two; div max_int two; max_int - two; max_int - one; max_int;]
  end

module Inverses (X : Int_intf.S) (Y : Int_intf.S)
  (Conv : sig
      val x_to_y : X.t -> Y.t
      val y_to_x : Y.t -> X.t option
  end) = struct
    open Conv
    let xs = let module E = Examples (X) in E.examples
    let ys = let module E = Examples (Y) in E.examples
    let out_of_range y = assert (y >= x_to_y X.max_int || y <= x_to_y X.min_int)
    let test =
      "all" >:: (fun () ->
        List.iter xs ~f:(fun x ->
          let y = x_to_y x in
          match y_to_x y with
          | Some x' -> assert (x = x')
          | None -> out_of_range y);
        List.iter ys ~f:(fun y ->
          match y_to_x y with
          | Some x -> assert (x_to_y x = y)
          | None -> out_of_range y))
  end

module Inverses' (X : Int_intf.S) (Y : Int_intf.S)
  (Conv : sig
      val x_to_y : X.t -> Y.t option
      val y_to_x : Y.t -> X.t option
  end) = struct
    open Conv
    let xs = let module E = Examples (X) in E.examples
    let ys = let module E = Examples (Y) in E.examples
    let get = function
      | Some z -> z
      | None -> failwith "Out_of_range"
    let y_to_x_exn y = get (y_to_x y)
    let x_to_y_exn x = get (x_to_y x)
    let x_out_of_range x =
      assert (x >= y_to_x_exn Y.max_int || x <= y_to_x_exn Y.min_int)
    let y_out_of_range y =
      assert (y >= x_to_y_exn X.max_int || y <= x_to_y_exn X.min_int)
    let test =
      "all" >:: (fun () ->
        List.iter xs ~f:(fun x ->
          match x_to_y x with
          | None -> x_out_of_range x
          | Some y ->
              match y_to_x y with
              | Some x' -> assert (x = x')
              | None -> y_out_of_range y);
        List.iter ys ~f:(fun y ->
          match y_to_x y with
          | Some x -> assert (x_to_y_exn x = y)
          | None -> y_out_of_range y))
  end

module Ii6 = Inverses (Int) (Int64) (struct
  let x_to_y = Int.to_int64
  let y_to_x = Int.of_int64
end)

module Ii6' = Inverses (Int) (Int64) (struct
  let x_to_y = Int64.of_int
  let y_to_x = Int64.to_int
end)

module Iin = Inverses (Int) (Nativeint) (struct
  let x_to_y = Int.to_nativeint
  let y_to_x = Int.of_nativeint
end)

module Iin' = Inverses (Int) (Nativeint) (struct
  let x_to_y = Nativeint.of_int
  let y_to_x = Nativeint.to_int
end)

module I36 = Inverses (Int32) (Int64) (struct
  let x_to_y = Int32.to_int64
  let y_to_x = Int32.of_int64
end)

module I36' = Inverses (Int32) (Int64) (struct
  let x_to_y = Int64.of_int32
  let y_to_x = Int64.to_int32
end)

module I3n = Inverses (Int32) (Nativeint) (struct
  let x_to_y = Int32.to_nativeint
  let y_to_x = Int32.of_nativeint
end)

module I3n' = Inverses (Int32) (Nativeint) (struct
  let x_to_y = Nativeint.of_int32
  let y_to_x = Nativeint.to_int32
end)

module In6 = Inverses (Nativeint) (Int64) (struct
  let x_to_y = Int64.of_nativeint
  let y_to_x = Int64.to_nativeint
end)

module In6' = Inverses (Nativeint) (Int64) (struct
  let x_to_y = Nativeint.to_int64
  let y_to_x = Nativeint.of_int64
end)

module Ii3 = Inverses' (Int) (Int32) (struct
  let x_to_y = Int.to_int32
  let y_to_x = Int.of_int32
end)

module Ii3' = Inverses' (Int) (Int32) (struct
  let x_to_y = Int32.of_int
  let y_to_x = Int32.to_int
end)
  
let test =
  TestList
    [
      Ii6.test;
      Ii6'.test;
      Iin.test;
      Iin'.test;
      I36.test;
      I36'.test;
      I3n.test;
      I3n'.test;
      In6.test;
      In6'.test;
      Ii3.test;
      Ii3'.test;
    ]
;;
