open Std_internal

type 'a t = 'a ref = { mutable contents : 'a }

let create x = ref x

let (!) = Pervasives.(!)
let (:=) = Pervasives.(:=)
  
let equal (t1 : 'a t) t2 = phys_equal t1 t2

let swap t1 t2 =
  let tmp = !t1 in
  t1 := !t2;
  t2 := tmp;
;;

let replace t f = t := f !t

