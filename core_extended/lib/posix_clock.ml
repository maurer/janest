open Core.Std

INCLUDE "config.mlh"

type t =
  | Realtime
  | Monotonic
  | Process_cpu
  | Process_thread

let all = [
  Realtime;
  Monotonic;
  Process_cpu;
  Process_thread;
]

let to_string t =
  match t with
  | Realtime       -> "Realtime"
  | Monotonic      -> "Monotonic"
  | Process_cpu    -> "Process_cpu"
  | Process_thread -> "Process_thread"

IFDEF POSIX_TIMERS THEN

external getres : t -> Int63.t = "caml_clock_getres" "noalloc"
external gettime : t -> Int63.t = "caml_clock_gettime" "noalloc"

module Int63_arithmetic : sig
  type t = Int63.t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
end = Int63

let min_interval t =
  let canary_val = Int63.of_int 1_000_000 in
  let current_min = ref canary_val in
  for _i = 1 to 10_000 do
    let t1 = gettime t in
    let t2 = gettime t in
    let open Int63.Replace_polymorphic_compare in
    let open Int63_arithmetic in
    if t1 <> t2 && t2 > t1 then current_min := min (t2 - t1) !current_min
  done;
  if !current_min <> canary_val then !current_min
  else failwith (Printf.sprintf !"unable to calculate min_interval for %{}" t)
;;

let mean_gettime_cost ~measure ~using =
  assert (getres Process_cpu = Int63.one);
  let count = 10_000_000 in
  let start = gettime using in
  for _i = 1 to count do
    ignore (gettime measure);
  done;
  let stop = gettime using in
  Int63_arithmetic.((stop - start) / Int63.of_int count)
;;

let getres            = Ok getres
let gettime           = Ok gettime
(* let nanosleep      = Ok nanosleep *)
let min_interval      = Ok min_interval
let mean_gettime_cost = Ok mean_gettime_cost

ELSE

let getres            = unimplemented "Posix_clock.getres"
let gettime           = unimplemented "Posix_clock.gettime"
(* let nanosleep      = unimplemented "Posix_clock.nanosleep" *)
let min_interval      = unimplemented "Posix_clock.min_interval"
let mean_gettime_cost = unimplemented "Posix_clock.mean_gettime_cost"

ENDIF



module Time_stamp_counter = struct
  type t = int

  let diff t1 t2 = t1 - t2

  IFDEF ARCH_x86_64 THEN
    external rdtsc : unit -> int = "caml_rdtsc" "noalloc"
  ELSE IFDEF ARCH_i386 THEN
    external rdtsc : unit -> int = "caml_rdtsc" "noalloc"
  ELSE
    let rdtsc () =
      failwith "Posix_clock.Time_stamp_counter.rdtsc \
                is not implemented for this architecture."
  ENDIF ENDIF

  IFDEF ARCH_x86_64 THEN
    external rdtscp : unit -> int = "caml_rdtscp" "noalloc"
  ELSE IFDEF ARCH_i386 THEN
    external rdtscp : unit -> int = "caml_rdtscp" "noalloc"
  ELSE
    let rdtscp () =
      failwith "Posix_clock.Time_stamp_counter.rdtscp \
                is not implemented for this architecture."
  ENDIF ENDIF

end
