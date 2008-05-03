module Mutex = Core_mutex
  
type 'a create = unit -> (unit -> 'a option) * ('a -> unit)

type 'a t = 'a z option ref
and 'a z = { value : 'a; next : 'a t; }

let one_reader_one_writer () =
  let q = ref None in
  let front = ref q in
  let back = ref q in
  let write a =
    let next = ref None in
    !back := Some { value = a; next = next; };
    back := next;
  in
  let read () =
    match !(!front) with
    | None -> None
    | Some x -> 
        front := x.next;
        Some x.value
  in
  (read, write)

let wrap f =
  let mutex = Mutex.create () in
  fun a -> Mutex.critical_section mutex ~f:(fun () -> f a)
    
let one_reader_many_writers () =
  let (read, write) = one_reader_one_writer () in
  (read, wrap write)

let many_readers_one_writer () =
  let (read, write) = one_reader_one_writer () in
  (wrap read, write)

let many_readers_many_writers () =
  let (read, write) = one_reader_one_writer () in
  (wrap read, wrap write)
