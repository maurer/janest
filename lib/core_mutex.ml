include Mutex

let try_lock = try_lock

let lock m = if try_lock m then () else lock m

let critical_section l ~f = 
  lock l;
  Exn.protect ~f ~finally:(fun () -> unlock l);
;;
