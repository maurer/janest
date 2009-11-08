(* This module exploits the fact that OCaml does not perform
   context-switches under certain conditions.  It can therefore avoid
   using mutexes in these cases. *)



module Mutex = Core_mutex

type 'a create = unit -> (unit -> 'a option) * ('a -> unit)

type 'a queue_end = 'a z option ref
and 'a z = {
  value : 'a;
  next : 'a queue_end;
}

type 'a t = {
  mutable front : 'a queue_end;
  mutable back : 'a queue_end;
}

(* Given the semantics of the current OCaml runtime (and for the
   foreseeable future), code sections documented as atomic below will
   never contain a context-switch.  The deciding criterion is whether
   they contain allocations or calls to external/builtin functions.
   If there is none, a context-switch cannot happen.  Assignments without
   allocations, field access, pattern-matching, etc., do not trigger
   context-switches.

   Code reviewers should therefore make sure that the sections documented
   as atomic below do not violate the above assumptions.  It is prudent to
   disassemble the .o file (using objdump -dr) and examine it.
*)
let one_reader_one_writer () =
  let queue_end = ref None in
  let q = { front = queue_end; back = queue_end } in
  let write a =
    let next = ref None in
    let el = Some { value = a; next = next } in
    (* BEGIN ATOMIC SECTION *)
    q.back := el;
    q.back <- next;
    (* END ATOMIC SECTION *)
  in
  let read () =
    (* BEGIN ATOMIC SECTION *)
    match !(q.front) with
    | None -> None
    | Some el ->
        q.front <- el.next;
    (* END ATOMIC SECTION *)
        Some el.value
  in
  read, write

let wrap f =
  let mutex = Mutex.create () in
  fun a -> Mutex.critical_section mutex ~f:(fun () -> f a)

let one_reader_many_writers () =
  let read, write = one_reader_one_writer () in
  read, wrap write

let many_readers_one_writer () =
  let read, write = one_reader_one_writer () in
  wrap read, write

let many_readers_many_writers () =
  let read, write = one_reader_one_writer () in
  wrap read, wrap write
