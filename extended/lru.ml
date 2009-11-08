open Core.Std


type 'a lru = {
  (* sorted in order of descending recency *)
  list: 'a Doubly_linked.t;
  (* allows fast lookup in the list above *)
  table: ('a, 'a Doubly_linked.Elt.t) Hashtbl.t;
  mutable maxsize: int;
  destructor: 'a -> unit;
  mutable size: int;
}

let kill_extra lru =
  while lru.size > lru.maxsize do
    let data = Option.value_exn (Doubly_linked.remove_last lru.list) in
    Hashtbl.remove lru.table data; (* remove from table *)
    lru.size <- lru.size - 1; (* reduce size by 1 *)
    lru.destructor data
  done

let touch lru x =
  
  try
    let el = Hashtbl.find_exn lru.table x in
    Doubly_linked.remove lru.list el;
    let new_el = Doubly_linked.insert_first lru.list x in
    Hashtbl.replace lru.table ~key:x ~data:new_el
  with Not_found ->
    let el = Doubly_linked.insert_first lru.list x in
    Hashtbl.replace lru.table ~key:x ~data:el;
    lru.size <- lru.size + 1;
    kill_extra lru

let create maxsize destructor = {
  list = Doubly_linked.create ();
  table = Hashtbl.create 100;
  maxsize = maxsize;
  destructor = destructor;
  size = 0;
}

let clear lru =
  lru.size <- 0;
  Hashtbl.iter lru.table ~f:(fun ~key:_ ~data -> lru.destructor (Doubly_linked.Elt.value data));
  Hashtbl.clear lru.table;
  Doubly_linked.clear lru.list

let size lru = lru.size

let change_size lru newsize =
  lru.maxsize <- newsize;
  kill_extra lru

let in_cache key lru =
  Hashtbl.mem lru.table key

type 'a memo_store  = Rval of 'a | Expt of exn

let memoize ?destruct size f =
  let store = Hashtbl.create 5 in
  let lru = create size (fun arg ->
    begin match destruct with
    | None -> ()
    | Some f -> match Hashtbl.find_exn store arg with
      | Rval r -> f r
      | Expt _ -> ()
    end;
    Hashtbl.remove store arg
  ) in
  let return rval = match rval with
    | Rval x -> x
    | Expt e -> raise e
  in
  let lookup arg =
    touch lru arg;
    match (try Some (Hashtbl.find_exn store arg) with Not_found -> None) with
    | Some rval -> return rval
    | None ->
        let rval = try Rval (f arg) with
          | Sys.Break as e -> raise e
          | e -> Expt e
        in
        Hashtbl.replace store ~key:arg ~data:rval;
        return rval
  in
  (lru,lookup)
