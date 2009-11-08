open Std_internal

module Mutex = Core_mutex

(** Synchronized queue type *)
type 'a t = {
  ev_q: 'a Queue.t;
  maxsize: int;
  mutex: Mutex.t;
  not_empty: Condition.t;
  not_full: Condition.t;
}

let create maxsize =
  {
    ev_q = Queue.create ();
    mutex = Mutex.create ();
    not_empty = Condition.create ();
    not_full = Condition.create ();
    maxsize = maxsize;
  }

let id () = Thread.id (Thread.self ())

let wrap q run =
  Mutex.lock q.mutex;
  protect ~f:run
    ~finally:(fun () ->
      let len = Queue.length q.ev_q in
      if len <> 0 then Condition.signal q.not_empty;
      if len < q.maxsize then Condition.signal q.not_full;
      Mutex.unlock q.mutex)
;;

let clear q =
  let run () =
    Queue.clear q.ev_q
  in
  wrap q run
;;


let wait_not_full q =
  while Queue.length q.ev_q >= q.maxsize do
    Condition.wait q.not_full q.mutex
  done

let wait_not_empty q =
  while Queue.is_empty q.ev_q do
    Condition.wait q.not_empty q.mutex
  done

(** Pushes an event on the queue if there's room *)
let push q x =
  let run () =
    wait_not_full q;
    Queue.enqueue q.ev_q x;
  in
  wrap q run

(** Pushes an event on the queue, unconditionally, may grow the queue past maxsize *)
let push_uncond q x =
  let run () = Queue.enqueue q.ev_q x in
  wrap q run

(** Pushes an event on the queue if the queue is less than maxsize, otherwise drops it.
  Returns true if the push was successful *)
let push_or_drop q x =
  let run () =
    if Queue.length q.ev_q < q.maxsize then
      begin
        Queue.enqueue q.ev_q x;
        true
      end
    else false
  in
  wrap q run

(** computes the length of the queue *)
let length q =
  let run () = Queue.length q.ev_q in
  wrap q run

(** Pops an event off of the queue, blocking until
    something is available *)
let pop q =
  let run () =
    wait_not_empty q;
    Queue.dequeue_exn q.ev_q
  in
  wrap q run

(** Pops an event off of the queue, blocking until something is available.
    Returns pair of the element found and the length of remaining queue *)
let lpop q =
  let run () =
    wait_not_empty q;
    let el = Queue.dequeue_exn q.ev_q in
    let len = Queue.length q.ev_q in
    el, len
  in
  wrap q run

let transfer_queue_in_uncond q in_q =
  if not (Queue.is_empty in_q) then begin
    let run () =
      Queue.transfer ~src:in_q ~dst:q.ev_q
    in
    wrap q run
  end

let transfer_queue_in q in_q =
  if not (Queue.is_empty in_q) then begin
    let run () =
      wait_not_full q;
      Queue.transfer ~src:in_q ~dst:q.ev_q
    in
    wrap q run
  end


let transfer_queue_nowait_nolock sq q = Queue.transfer ~src:sq.ev_q ~dst:q


let transfer_queue_nowait sq q =
  if not (Queue.is_empty sq.ev_q) then begin
    let run () = transfer_queue_nowait_nolock sq q in
    wrap sq run
  end


let transfer_queue sq q =
  let run () =
    wait_not_empty sq;
    transfer_queue_nowait_nolock sq q
  in
  wrap sq run

(* The external version of wait_not_empty *)
let wait_not_empty sq =
  let run () = wait_not_empty sq in
  wrap sq run
