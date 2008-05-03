exception Empty

include Doubly_linked

let enqueue t x = ignore (insert_last t x : _ Elt.t)

let dequeue = remove_first

let dequeue_exn t =
  match dequeue t with
  | None -> raise Empty
  | Some x -> x
;;

let peek = first

let peek_exn t =
  match peek t with
  | None -> raise Empty
  | Some x -> x
;;


