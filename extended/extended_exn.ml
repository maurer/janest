open Core.Std

let to_string = function
  | Failure s -> "Failure: " ^ s
  | e -> Exn.to_string e

let to_string_hum = function
  | Failure s ->  s
  | e -> Exn.to_string e
