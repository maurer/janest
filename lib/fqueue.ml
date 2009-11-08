(** Simple implementation of a polymorphic functional queue *)
(** push and top and bottom are O(1).  
   pop and take are O(1) amortized.
   to_list is O(n).
   length is O(1).
*)

(* Invariants:  
   - if queue is not empty, outlist is not empty  
   - if queue has more than 1 element, then inlist is not empty
   - queue.length = List.length(queue.outlist) + List.length(queue.inlist)
*)

exception Bug of string
exception Empty

type 'a t = { inlist: 'a list;
              outlist: 'a list;
              length: int;
            }

let test_invariants queue = 
  assert (queue.length = (List.length queue.outlist) + (List.length queue.inlist));
  assert (if queue.length <> 0 then List.length queue.outlist <> 0 else true);
  assert (if queue.length > 1 then List.length queue.inlist <> 0 else true)

let empty = { inlist = [];
              outlist = [];
              length = 0;
            }

let push el queue =
  let length = queue.length + 1 in
  if queue.outlist = [] then
    if queue.inlist = [] then
      { inlist = [];
        outlist = [el];
        length = length;
      } 
    else
      { inlist = [el]; 
        outlist = List.rev queue.inlist;
        length = length;
      }
  else
    { inlist = el::queue.inlist;
      outlist = queue.outlist;
      length = length;
    }

(** pushes el on the top of the queue, effectively making it
    the least recently enqueued element *)
let push_top el queue =
  let length = queue.length + 1 in
  if queue.inlist = [] then
    if queue.outlist = [] then
      { inlist = [];
        outlist = [el];
        length = length;
      }
    else
      { inlist = List.rev queue.outlist;
        outlist = [el];
        length = length;
      }
  else
    { inlist = queue.inlist;
      outlist = el::queue.outlist;
      length = length;
    }

(** same as push *)
let enq = push

(** returns bottom (most-recently enqueued) item  *)
let bot_exn queue = 
  match (queue.inlist,queue.outlist) with
      ([],[]) -> raise Empty
    | ([],[x]) -> x
    | (x::_,_) -> x
    | ([], _ :: _ :: _) -> raise (Bug "FQueue.bot_exn: empty inlist and outlist with len >1")

let bot queue = try Some (bot_exn queue) with Empty -> None

(** returns top (least-recently enqueued) item  *)
let top_exn queue = 
  match (queue.outlist,queue.inlist) with
    | ([],[]) -> raise Empty
    | (x::_,_) -> x
    | ([],_::_) -> raise (Bug "FQueue.top_exn: empty outlist and non-empty inlist")

let top queue = 
  try Some (top_exn queue) 
  with Empty -> None

(** returns top of queue and queue with top removed  *)
let pop_exn queue = match (queue.inlist,queue.outlist) with
  | ([],[]) -> raise Empty
  | ([y],[x]) ->
      (x, { inlist = []; outlist = [y]; length = queue.length - 1 })
  | (y::ytl,[x]) ->
      (x, { inlist = [y]; outlist = List.rev ytl; length = queue.length - 1 })
  | (inlist,x::tl) ->
      (x, { inlist = inlist; outlist = tl; length = queue.length - 1})
  | (_ :: _, []) ->
      raise (Bug "FQueue.pop_exn: outlist empty on non-empty list")

let pop queue = 
  try Some (pop_exn queue) with Empty -> None

(** same as pop *)
let deq = pop

let deq_exn = pop_exn

(** returns queue with top removed *)
let discard_exn queue = 
  let (_,new_q) = pop_exn queue in
    new_q


(** converts queue to list, from most to least recently enqueued item *)
let to_list queue = 
  queue.inlist @ (List.rev (queue.outlist))

let sexp_of_t sexp_of_a q = Sexplib.Conv.sexp_of_list sexp_of_a (to_list q)

let length queue = queue.length

let is_empty queue = queue.length = 0
