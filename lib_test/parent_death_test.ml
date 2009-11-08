(* Test parent death notification *)

open Core.Std
open Sys
open Linux_ext
open OUnit

let handle_signal s =
  if Signal.equal s Signal.hup then
    printf "Success: got SIGHUP -> parent died!\n%!"
  else (
    printf "Got unknown signal: %s\n%!" (Signal.to_string s);
    assert false
  )

let run () =
  Signal.handle Signal.hup handle_signal;
  let pid = Unix.fork () in
  if pid = 0 then (
    pr_set_pdeathsig Signal.hup;
    if Unix.getppid () = 1 then Signal.send Signal.kill ~pid:(Unix.getpid ());
    Unix.sleep 3)
  else Unix.sleep 1

let test = "Parent_death_test" >::: [
  "test" >:: (fun () ->
    "1" @? (try run (); true with _ -> false));
]

