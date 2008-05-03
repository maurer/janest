open Std_internal

(** Safely lock/unlock a mutex around a function. *)
let wrap_mutex mtx f =
  Mutex.lock mtx;
  protect ~f ~finally:(fun () -> Mutex.unlock mtx)

(** Type of intervals for repeated events *)
type interval =
  | INone  (** No repetition of events *)

  (** Regular repetition of events.  [span] must be greater than zero. *)
  | INormal of Time.Span.t

  (** Randomized repetition of events.  [span] must be greater than zero.
      The float specifies the maximum ratio with which [span] will be
      multiplied and added to itself, and must be in the range \[0.0, 1.0\]. *)
  | IRandom of Time.Span.t * float

type t =
  {
    mutable activated : bool;  (** whether the timer is currently activated *)
    tick_unit : Time.Span.t;
    events : event Heap.t;
    mtx : Mutex.t;  (** mutex protecting access to record fields *)
  }

and event =
  {
    mutable time : Time.t; (** Absolute time at which event should first be run *)
    mutable interval : interval;
    handler : event -> Time.t -> unit;
    timer : t;
    mutable t_event_opt : event Heap.heap_el option;
  }

let cmp ev1 ev2 = Time.compare ev1.time ev2.time

let ready_to_run ev = Time.(<) ev.time (Time.now ())

let rec run_timer timer =
  Mutex.lock timer.mtx;
  if not timer.activated then ()
  else
    let events = timer.events in
    match Heap.cond_pop_heap_el events ready_to_run with
    | None ->
        Mutex.unlock timer.mtx;
        Time.pause timer.tick_unit;
        run_timer timer
    | Some event ->
        let ev = Heap.heap_el_get_el event in
        let sched_time = ev.time in
        (* put events back on the heap as necessary *)
        begin match ev.interval with
        | INone -> ()
        | INormal span ->
            ev.time <- Time.add sched_time span;
            Heap.push_heap_el events event
        | IRandom (span, max_ratio) ->
            let p2 = Random.float 2.0 in
            let p = p2 -. 1. in
            let confusion = Time.Span.scale (max_ratio *. p) span in
            ev.time <- Time.add (Time.add sched_time span) confusion;
            Heap.push_heap_el events event
        end;
        Mutex.unlock timer.mtx;
        (try ev.handler ev sched_time
         with e ->
           eprintf
             "Timer.run: Exception in event handler: %s\n%!" (exn_to_string e));
        run_timer timer

let create ?(min_size = 1000) ?(tick_unit = Time.Span.of_sec 0.01) () =
  let events = Heap.create ~min_size cmp in
  if Time.Span.(<=) tick_unit Time.Span.zero then
    invalid_arg "Timer.create: tick_unit <= 0";
  let timer =
    {
      activated = true;
      tick_unit = tick_unit;
      events = events;
      mtx = Mutex.create ();
    }
  in
  ignore (Thread.create run_timer timer);
  timer

let deactivate timer =
  wrap_mutex timer.mtx (fun () -> timer.activated <- false)

let check_span loc span =
  if Time.Span.(<) span Time.Span.zero then invalid_arg (sprintf "Timer.%s: span < 0" loc)

let get_interval_param loc randomize = function
  | None -> INone
  | Some span ->
      check_span loc span;
      match randomize with
      | None -> INormal span
      | Some max_ratio ->
          if max_ratio < 0. || 1. < max_ratio then
            invalid_arg (
              sprintf "Timer.%s: max_ratio not in range [0.0, 1.0]" loc);
          IRandom (span, max_ratio)

let add_abs timer handler ?randomize ?interval time =
  let interval = get_interval_param "add_abs" randomize interval in
  wrap_mutex timer.mtx
    (fun () ->
       if not timer.activated then failwith "Timer.add_abs: timer deactivated";
       let event =
         {
           time = time;
           interval = interval;
           handler = handler;
           timer = timer;
           t_event_opt = None;
         }
       in
       let t_event = Heap.push timer.events event in
       event.t_event_opt <- Some t_event;
       event)

let add t handler ?randomize ?interval span =
  let time = Time.add (Time.now ()) span in
  check_span "add" span;
  add_abs t handler ?randomize ?interval time

let remove { timer = timer; t_event_opt = t_event_opt } =
  match t_event_opt with
  | Some t_event ->
      wrap_mutex timer.mtx (fun () ->
        if Heap.heap_el_is_valid t_event then Heap.remove t_event)
  | None -> assert false

let reschedule ({ timer = timer } as ev) ?randomize ?interval span =
  match ev.t_event_opt with
  | Some t_event ->
      let loc = "reschedule" in
      check_span loc span;
      let interval = get_interval_param loc randomize interval in
      wrap_mutex timer.mtx (fun () ->
        if not timer.activated then failwith "Timer.reschedule: timer deactivated";
        if Heap.heap_el_is_valid t_event then (
          Heap.remove t_event;
          ev.time <- Time.add ev.time span;
          ev.interval <- interval;
          Heap.push_heap_el timer.events t_event)
        else failwith "Timer.reschedule: event not scheduled")
  | None -> assert false

let get_timer ev = ev.timer
let get_event_time ev = ev.time
let get_event_interval ev = ev.interval

let is_activated timer = timer.activated
