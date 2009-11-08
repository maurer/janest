(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(* A hash-queue is a combination of a queue and a hashtable that
 * supports constant-time lookup and removal of queue elements in addition to
 * the usual queue operations (enqueue, dequeue).  The queue elements are key-value
 * pairs.  The hashtable has one entry for each element of the queue. *)

(* for tail-recursive versions of List functions
   Can't open Std_internal due to cyclic dependencies
*)
module List = Core_list
module Array = Core_array

(* The key is used for the hashtable of queue elements. *)
module type Key = Core_hashtbl.HashedType

module type S = sig
  module Key : Key

    (* a hash-queue, where the values are of type 'a *)
  type 'a t

  include Container.S1 with type 'a container = 'a t

    (* [invariant t] checks the invariants of the queue. *)
  val invariant : 'a t -> unit

    (* [create ()] returns an empty queue. *)
  val create : unit -> 'a t

    (*  clear the queue *)
  val clear : 'a t -> unit

  (* Finding elements. *)
    (* [mem q k] returns true iff there is some (k, v) in the queue. *)
  val mem : 'a t -> Key.t -> bool

  (* [lookup t k] returns the value of the key-value pair in the queue with
     key k, if there is one. *)
  val lookup : 'a t -> Key.t -> 'a option

  val lookup_exn : 'a t -> Key.t -> 'a

  (* Adding, removing, and replacing elements. *)
    (* [enqueue t k v] adds the key-value pair (k, v) to the end of the queue,
       returning `Ok if the pair was added, or `Key_already_present
       if there is already a (k, v') in the queue.
    *)
  val enqueue : 'a t -> Key.t -> 'a -> [ `Ok | `Key_already_present ]

  val enqueue_exn : 'a t -> Key.t -> 'a -> unit

    (* [dequeue t] returns the front element of the queue. *)
  val dequeue : 'a t -> 'a option

  val dequeue_exn : 'a t -> 'a

  (* [dequeue_with_key t] returns the front element of the queue and its key *)
  val dequeue_with_key : 'a t -> (Key.t * 'a) option

  val dequeue_with_key_exn : 'a t -> (Key.t * 'a)

    (* [dequeue_all t ~f] dequeues every element of the queue and applies f to each
       one. *)
  val dequeue_all : 'a t -> f:('a -> unit) -> unit
    (* [remove q k] removes the key-value pair with key k from the queue. *)
  val remove : 'a t -> Key.t -> [ `Ok | `No_such_key ]

  val remove_exn : 'a t -> Key.t -> unit


    (* [replace q k v] changes the value of key k in the queue to v. *)
  val replace : 'a t -> Key.t -> 'a -> [ `Ok | `No_such_key ]

  val replace_exn : 'a t -> Key.t -> 'a -> unit

  (* Iterating over elements *)
    (* [iter t ~f] applies f to each key and element of the queue.  It is an
       error to modify the queue while iterating over it. *)
    
  val iteri : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit
  val foldi : 'a t -> init:'b -> f:('b -> key:Key.t -> data:'a -> 'b) -> 'b

end

module Make (Key : Key) : S with module Key = Key = struct
  module Key = Key
  module Table = Core_hashtbl.Make (Key)

  module Key_value = struct
    module T = struct
      type 'a t = {
        key : Key.t;
        mutable value : 'a;
      }
    end
    include T

    let equal (t : 'a t) t' = t == t'
    let key t = t.key
    let value t = t.value
  end

  open Key_value.T

  module Elt = Doubly_linked.Elt

  type 'a t = {
    queue : 'a Key_value.t Doubly_linked.t;
    table : 'a Key_value.t Elt.t Table.t;
  }

  let invariant t =
    assert (Doubly_linked.length t.queue = Table.length t.table);
    (* Look at each element in the queue, checking:
     *   - every element in the queue is in the hash table
     *   - there are no duplicate keys
     *)
    let keys = Table.create (Table.length t.table) in
    Doubly_linked.iter t.queue ~f:(fun kv ->
      let key = kv.key in
      match Table.find t.table key with
      | None -> assert false
      | Some _ ->
          assert (not (Table.mem keys key));
          Table.replace keys ~key ~data:());
  ;;

  let create () = {
    queue = Doubly_linked.create ();
    table = Table.create 16;
  }

  let clear t =
    Doubly_linked.clear t.queue;
    Table.clear t.table;
  ;;

  let length t = Table.length t.table

  let is_empty t = length t = 0

  let lookup t k =
    match Table.find t.table k with
    | None -> None
    | Some elt -> Some (Elt.value elt).value
  ;;

  let lookup_exn t k = (Elt.value (Table.find_exn t.table k)).value

  let mem t k = Table.mem t.table k

  type 'a container = 'a t

  (* Note that this is the tail-recursive Core_list.map *)
  let to_list t = List.map (Doubly_linked.to_list t.queue) ~f:Key_value.value

  let to_array t = Array.map (Doubly_linked.to_array t.queue) ~f:Key_value.value

  let for_all t ~f = Doubly_linked.for_all t.queue ~f:(fun kv -> f kv.value)

  let exists t ~f = Doubly_linked.exists t.queue ~f:(fun kv -> f kv.value)

  let find t ~f =
    Option.map (Doubly_linked.find t.queue ~f:(fun kv -> f kv.value))
      ~f:Key_value.value
  ;;

  let enqueue t key value =
    if Table.mem t.table key then
      `Key_already_present
    else begin
      let elt =
        Doubly_linked.insert_last t.queue
          { Key_value.key = key; value = value; }
      in
      Table.replace t.table ~key ~data:elt;
      `Ok
    end
  ;;

  let enqueue_exn t key value =
    match enqueue t key value with
    | `Key_already_present -> failwith "Hash_queue.enqueue_exn: key already present"
    | `Ok -> ()
  ;;

  let dequeue_with_key t =
    match Doubly_linked.remove_first t.queue with
    | None -> None
    | Some kv -> Table.remove t.table kv.key; Some (kv.key, kv.value)
  ;;

  let dequeue_with_key_exn t =
    match dequeue_with_key t with
    | None -> raise Not_found
    | Some (k, v) -> (k, v)
  ;;

  let dequeue t =
    match dequeue_with_key t with
    | None -> None
    | Some (_, v) -> Some v
  ;;

  let dequeue_exn t =
    match dequeue t with
    | None -> raise Not_found
    | Some v -> v
  ;;

  let iteri t ~f =
    Doubly_linked.iter t.queue ~f:(fun kv -> f ~key:kv.key ~data:kv.value)
  ;;

  let iter t ~f = iteri t ~f:(fun ~key:_ ~data -> f data)

  let foldi t ~init ~f =
    Doubly_linked.fold t.queue ~init ~f:(fun ac kv ->
      (f ac ~key:kv.key ~data:kv.value))
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun ac ~key:_ ~data -> f ac data)

  let dequeue_all t ~f =
    let rec loop () =
      match dequeue t with
      | None -> ()
      | Some v -> f v; loop ()
    in
    loop ()

  let remove t k =
    match Table.find t.table k with
    | None -> `No_such_key
    | Some elt ->
        Doubly_linked.remove t.queue elt;
        Table.remove t.table (Elt.value elt).key;
        `Ok
  ;;

  let remove_exn t k =
    match remove t k with
    | `No_such_key -> raise Not_found
    | `Ok -> ()
  ;;

  let replace t k v =
    match Table.find t.table k with
    | None -> `No_such_key
    | Some elt -> (Elt.value elt).value <- v; `Ok
  ;;

  let replace_exn t k v =
    match replace t k v with
    | `No_such_key -> raise Not_found
    | `Ok -> ()

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
end
