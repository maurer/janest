open Core.Std

(* interactive readline test *)

let names = [
  "Till";
  "Bene";
  "Mark";
  "David";
  "Markus"
]

let () =
  let tab_completion ~left ~right:_ =
    let last = List.last_exn (String.split left ~on:' ') in
    List.filter names ~f:(String.is_prefix ~prefix:last)
  in
  let rec loop () =
    let line = Core_extended.Readline.input_line ~tab_completion () in
    Printf.printf "%S\n%!" line;
    loop ()
  in
  try
    loop ()
  with End_of_file -> ()
