open Core.Std
open Unix_ext

(* Handling RAM limits *)

let physical_ram () = Int64.( * ) (sysconf PAGESIZE) (sysconf PHYS_PAGES)

type ram_usage_limit = Unlimited | Absolute of int64 | Relative of float

let set_ram_limit l =
  RLimit.set RLimit.AS
    {
      RLimit.cur = RLimit.Limit l;
      RLimit.max = RLimit.Infinity;
    }

let apply_ram_usage_limit = function
  | Unlimited -> ()
  | Absolute i -> set_ram_limit i
  | Relative f ->
      set_ram_limit (Int64.of_float (f *. Int64.to_float (physical_ram ())))

let ram_msg =
  "RAM limit should be either an integer or a float between 0 and 1, not "

let string_to_ram_usage_limit s =
  try
    let i = Int64.of_string s in
    if i > Int64.zero then Absolute i
    else Unlimited
  with Failure _ ->
    let f = float_of_string s in
    if f < 0. || f > 1. then raise (Arg.Bad (ram_msg ^ s));
    if f > 0. then Relative f
    else Unlimited

let ram_limit_spec =
  (
    "-ram_limit",
    Arg.String (fun s -> apply_ram_usage_limit (string_to_ram_usage_limit s)),
    "num Limit RAM consumption either as an absolute number of bytes or as \
     a fraction of the total RAM, 0 - no limit"
  )


(* Signal handling *)

let all_sigs =
  [
    Signal.abrt;
    Signal.alrm;
    Signal.fpe;
    Signal.hup;
    Signal.ill;
    Signal.int;
    Signal.kill;
    Signal.pipe;
    Signal.quit;
    Signal.segv;
    Signal.term;
    Signal.usr1;
    Signal.usr2;
    Signal.chld;
    Signal.cont;
    Signal.stop;
    Signal.tstp;
    Signal.ttin;
    Signal.ttou;
    Signal.vtalrm;
    Signal.prof;
  ]


let wrap_block_signals f =
  let blocked_sigs = Signal.sigprocmask `Set all_sigs in
  protect ~f ~finally:(fun () ->
    ignore (Signal.sigprocmask `Set blocked_sigs))


