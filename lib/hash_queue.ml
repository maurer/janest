(* A hash-queue is a combination of a queue and a hashtable that
 * supports constant-time lookup and removal of queue elements in addition to
 * the usual queue operations (enqueue, dequeue).  The queue elements are key-value
 * pairs.  The hashtable has one entry for each element of the queue. *)

module Array = Caml.ArrayLabels
module List = Caml.ListLabels
  
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
  
  (* CRv2 rdouglass: rename to find, add find_exn *)
  
    (* [find_opt t k] returns the value of the key-value pair in the queue with
       key k, if there is one. *)
  val find_opt : 'a t -> Key.t -> 'a option

  (* Adding, removing, and replacing elements. *)
    (* [enqueue t k v] adds the key-value pair (k, v) to the end of the queue,
       returning `Enqued if the pair was added, or `Key_already_present
       if there is already a (k, v') in the queue.
    *)
  val enqueue : 'a t -> Key.t -> 'a -> [ `Ok | `Key_already_present ]
    (* [dequeue t] returns the front element of the queue. *)
  val dequeue : 'a t -> 'a option

  (* [dequeue_with_key t] returns the front element of the queue and its key *)
  val dequeue_with_key : 'a t -> (Key.t * 'a) option

    (* [dequeue_all t ~f] dequeues every element of the queue and applies f to each 
       one. *)
  val dequeue_all : 'a t -> f:('a -> unit) -> unit
    (* [remove q k] removes the key-value pair with key k from the queue. *)
  val remove : 'a t -> Key.t -> [ `Ok | `No_such_key ]
    (* [replace q k v] changes the value of key k in the queue to v. *)
  val replace : 'a t -> Key.t -> 'a -> [ `Ok | `No_such_key ]

  (* Iterating over elements *)
    (* [iter t ~f] applies f to each key and element of the queue.  It is an
       error to modify the queue while iterating over it. *)
    (* CRv2 sweeks: Add a dynamic check to catch this error *)
  val iter_keys : 'a t -> f:(key:Key.t -> data:'a -> unit) -> unit
  val fold_keys : 'a t -> init:'b -> f:('b -> key:Key.t -> data:'a -> 'b) -> 'b

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
          Table.add keys ~key ~data:());
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

  let is_empty t = 0 = length t

  let find_opt t k = 
    match Table.find t.table k with
    | None -> None
    | Some elt -> Some (Elt.value elt).value
  ;;

  let mem t k = Table.mem t.table k

  type 'a container = 'a t

  let to_list t = List.map (Doubly_linked.to_list t.queue) ~f:Key_value.value

  let to_array t = Array.map (Doubly_linked.to_array t.queue) ~f:Key_value.value

  let for_all t ~f = Doubly_linked.for_all t.queue ~f:(fun kv -> f kv.value)

  let exists t ~f = Doubly_linked.for_all t.queue ~f:(fun kv -> f kv.value)

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
      Table.add t.table ~key ~data:elt;
      `Ok
    end

  let dequeue_with_key t =
    Option.map (Doubly_linked.remove_first t.queue) ~f:(fun kv ->
      Table.remove t.table kv.key;
      (kv.key, kv.value))
  ;;
                                                  
  let dequeue t = 
    match dequeue_with_key t with
    | None -> None
    | Some (_, v) -> Some v

  let iter_keys t ~f =
    Doubly_linked.iter t.queue ~f:(fun kv -> f ~key:kv.key ~data:kv.value)
  ;;

  let iter t ~f = iter_keys t ~f:(fun ~key:_ ~data -> f data)

  let fold_keys t ~init ~f =
    Doubly_linked.fold t.queue ~init ~f:(fun ac kv ->
      (f ac ~key:kv.key ~data:kv.value))
  ;;

  let fold t ~init ~f = fold_keys t ~init ~f:(fun ac ~key:_ ~data -> f ac data)

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

  let replace t k v = 
    match Table.find t.table k with
    | None -> `No_such_key
    | Some elt -> (Elt.value elt).value <- v; `Ok
  ;;

end
