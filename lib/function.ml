let const c = (); fun _ -> c

let non f = (); fun x -> not (f x)

let forever f =
  let rec forever () =
    f ();
    forever ()
  in
  try forever ()
  with e -> e

external ident : 'a -> 'a = "%identity"

let ( |! ) x y = y x

