include Mutex

let create = Unix_ext.create_error_checking_mutex

let phys_equal = Caml.(==)

let equal (t : t) t' = phys_equal t t'

let critical_section l ~f =
  lock l;
  Exn.protect ~f ~finally:(fun () -> unlock l)

let update_signal mtx cnd ~f =
  critical_section mtx ~f:(fun () ->
    let res = f () in
    Condition.signal cnd;
    res)

let update_broadcast mtx cnd ~f =
  critical_section mtx ~f:(fun () ->
    let res = f () in
    Condition.broadcast cnd;
    res)

let am_holding_mutex mtx =
  match
    try if try_lock mtx then `Free else `Held_by_other
    with _ -> `Held_by_me
  with
  | `Free -> unlock mtx; false
  | `Held_by_me -> true
  | `Held_by_other -> false
