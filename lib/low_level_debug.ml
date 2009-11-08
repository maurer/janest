(* Useful debugging functions working at a low level. *)


external stop_upon_sigbus : unit -> unit = "low_level_debug_stop_upon_sigbus"
external stop_upon_sigsegv : unit -> unit = "low_level_debug_stop_upon_sigsegv"
external stop_upon_sigpipe : unit -> unit = "low_level_debug_stop_upon_sigpipe"
external stop_upon_exit : unit -> unit = "low_level_debug_stop_upon_exit"


external stop_me_now : unit -> unit = "low_level_debug_stop_me_now"

(* It seems that veneers like these are needed so that the functions are
   correctly exported in the object files. *)
let stop_upon_sigbus () = stop_upon_sigbus ()
let stop_upon_sigsegv () = stop_upon_sigsegv ()
let stop_upon_sigpipe () = stop_upon_sigpipe ()
let stop_upon_exit () = stop_upon_exit ()
let stop_me_now () = stop_me_now ()
