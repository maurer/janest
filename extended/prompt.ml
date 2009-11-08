open Core.Std
module U = Unix

let print_flush s =
  print_string s;
  flush stdout

let prompt display =
  print_flush display;
  read_line ()

let password display =
  let module T = U.Terminal_io in
  print_flush display;  
  let term_init = T.tcgetattr U.stdin in
  let term_no_echo = {term_init with T.c_echo = false; } in
  T.tcsetattr U.stdin ~mode:T.TCSANOW term_no_echo;
  let pass =
    try
      Some (read_line ())
    with (* we need to guarantee we turn echo back on *)
    | _ -> None
  in
  T.tcsetattr U.stdin ~mode:T.TCSAFLUSH term_init;
  print_flush "\n";
  pass

let confirm display true_answer =
  print_flush display;
  let ans = String.strip (read_line ()) in
  String.lowercase ans = String.lowercase true_answer
