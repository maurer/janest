include Caml.Gc

let read_finaliser_queue, write_finaliser_queue = 
  Thread_safe_queue.one_reader_one_writer ()
;;

let start_finaliser_thread_promise = lazy (
  ignore (
    Thread.create (fun () -> Common.forever (fun () ->
      match read_finaliser_queue () with
      | None -> Thread.delay 1.0
      | Some f -> f () ))
    () ))
;;

let finalise f x =
  Lazy.force start_finaliser_thread_promise ;
  Caml.Gc.finalise (fun x -> write_finaliser_queue (fun () -> f x)) x
;;
