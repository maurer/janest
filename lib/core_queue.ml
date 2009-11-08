open Sexplib.Conv

module List = Core_list
module Queue = Caml.Queue

exception Empty = Queue.Empty

type 'a t = 'a Queue.t

let create = Queue.create

let enqueue t x = Queue.push x t

let is_empty = Queue.is_empty

let dequeue t = if is_empty t then None else Some (Queue.pop t)

let dequeue_exn = Queue.pop

let peek t = if is_empty t then None else Some (Queue.peek t)

let peek_exn = Queue.peek

let clear = Queue.clear

let copy = Queue.copy

let length = Queue.length

let iter t ~f = Queue.iter f t

let fold t ~init ~f = Queue.fold f init t

let transfer ~src ~dst = Queue.transfer src dst

let filter_inplace q ~f =
  let q' = create () in
  transfer ~src:q ~dst:q';
  iter q' ~f:(fun x -> if f x then enqueue q x)

let to_list t = List.rev (fold t ~init:[] ~f:(fun acc elem -> elem::acc))

let of_list list =
  let t = create () in
  ListLabels.iter list ~f:(enqueue t);
  t


let to_array t = Array.of_list (to_list t)



module Z = struct
  exception Local_exit

  let find t ~f =
    let r = ref None in
    try
      iter t ~f:(fun x -> if f x then (r := Some x; raise Local_exit));
      None
    with Local_exit -> !r
end
let find = Z.find

let exists t ~f = Option.is_some (find t ~f)
let for_all t ~f = not (exists t ~f:(fun x -> not (f x)))

module ZZ = struct
  exception Local_exit

  let partial_iter t ~f =
    try
      iter t ~f:(fun x ->
        match f x with
        | `Continue -> ()
        | `Stop -> raise Local_exit)
    with Local_exit -> ()
end
let partial_iter = ZZ.partial_iter

type 'a sexpable = 'a t


let t_of_sexp a_of_sexp sexp = of_list (list_of_sexp a_of_sexp sexp)
let sexp_of_t sexp_of_a t = sexp_of_list sexp_of_a (to_list t)

type 'a container = 'a t

let container = {
  Container.
  length = length;
  is_empty = is_empty;
  iter = iter;
  fold = fold;
  exists = exists;
  for_all = for_all;
  find = find;
  to_list = to_list;
  to_array = to_array;
}
