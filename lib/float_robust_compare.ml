(* This is factored out of float.ml in order to break a module dependency cycle.  *)



let epsilon = 0.000001
type robustly_comparable = float
let ( >=. ) x y = x >= y -. epsilon
let ( <=. ) x y = y >=. x
let ( =. ) x y = x >=. y && y >=. x
let ( >. ) x y = x > y +. epsilon
let ( <. ) x y = y >. x
let ( <>. ) x y = not (x =. y)
