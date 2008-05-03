open Sexplib
open Sexplib.Conv
module H = MoreLabels.Hashtbl

module type HashedType = sig 
  include H.HashedType
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module type S = sig
  type key
  and 'a t
  val create : int -> 'a t
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key:key -> data:'a -> unit
  val remove : 'a t -> key -> unit
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key:key -> data:'a -> unit
  val mem : 'a t -> key -> bool
  val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
  val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
  val length : 'a t -> int
  val find_default : 'a t -> key -> default:(unit -> 'a) -> 'a
  val find : 'a t -> key -> 'a option
  val find_exn : 'a t -> key -> 'a
  val iter_vals : f:('a -> unit) -> 'a t -> unit
  val of_alist : (key * 'a) list -> 'a t
  val to_alist : 'a t -> (key * 'a) list
  val keys : 'a t -> key list
  val data : 'a t -> 'a list
  val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
  val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
  module Infix :
    sig
      val ( |> ) : 'a t -> key -> 'a
      val ( |?> ) : 'a t -> key -> 'a option
      val ( <| ) : 'a t -> key * 'a -> unit
    end
end

type ('a, 'b) t = ('a, 'b) H.t

let add = H.add
let clear = H.clear
let copy = H.copy
let create = H.create
let find_all = H.find_all
let fold = H.fold
let iter = H.iter
let length = H.length
let mem = H.mem
let remove = H.remove
let replace = H.replace

let hash = H.hash
let hash_param = H.hash_param

(* We define a functor, Extend, that takes a hashtable and adds our
   additional operations.  We need this functor because we need to extend both
   the standard hashtable (MoreLabels.Hashtbl) and specialized hashtables
   created by Hashtbl.Make.
*)
module Extend (T : 
  sig
    type ('a, 'b) t 
    type 'a key

    val add : ('a, 'b) t -> key:('a key) -> data:'b -> unit
    val create : int -> ('a, 'b) t
    val find : ('a, 'b) t -> 'a key -> 'b
    val fold :
      f:(key:('a key) -> data:'b -> 'c -> 'c) ->
      ('a, 'b) t -> init:'c -> 'c
    val iter : f:(key:('a key) -> data:'b -> unit) -> ('a, 'b) t -> unit
  end): sig
  val find_default : ('a, 'b) T.t -> 'a T.key -> default:(unit -> 'b) -> 'b 
  val find : ('a, 'b) T.t -> 'a T.key -> 'b option
  val find_exn : ('a, 'b) T.t -> 'a T.key -> 'b
  val iter_vals : f:('b -> unit) -> ('a, 'b) T.t -> unit
  val of_alist : ('a T.key * 'b) list -> ('a, 'b) T.t
  val to_alist : ('a, 'b) T.t -> ('a T.key * 'b) list
  val sexp_of_t : ('a T.key -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) T.t -> Sexp.t
  val t_of_sexp : (Sexp.t -> 'a T.key) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) T.t
  val keys : ('a, 'b) T.t -> 'a T.key list
  val data : ('a, 'b) T.t -> 'b list
  module Infix : sig
    val ( |> ) : ('a, 'b) T.t -> 'a T.key -> 'b
    val ( |?> ) : ('a, 'b) T.t -> 'a T.key -> 'b option
    val ( <| ) : ('a, 'b) T.t -> 'a T.key * 'b -> unit
  end
end = struct
  
  let find_exn t id = T.find t id
  
  let find t id =
    try 
      let res = T.find t id in
      Some res
    with Not_found -> None
      
  let find_default t id ~default =
    match find t id with
    | Some x -> x
    | None ->
        let default = default () in
        T.add t ~key:id ~data:default;
        default

  let iter_vals ~f t = T.iter t ~f:(fun ~key:_ ~data -> f data)          

  let of_alist lst =
    let len = List.length lst in
    let t = T.create len in
    ListLabels.iter lst ~f:(fun (k, v) -> T.add t ~key:k ~data:v);
    t

  let to_alist htbl =
    T.fold ~f:(fun ~key ~data list -> (key,data)::list) ~init:[] htbl
      
  let keys htbl =
    T.fold ~f:(fun ~key ~data:_ list -> key::list) ~init:[] htbl

  let data htbl =
    T.fold ~f:(fun ~key:_ ~data list -> data::list) ~init:[] htbl

  let sexp_of_t sexp_of_k sexp_of_d t = 
    sexp_of_list (sexp_of_pair sexp_of_k sexp_of_d) (to_alist t)

  let t_of_sexp k_of_sexp d_of_sexp sexp = 
    let t = T.create 0 in
    let sexps =  match sexp with
      | Sexp.Atom _ -> Conv.of_sexp_error
          "Hashtbl.t_of_sexp: found Atom where list was expected" sexp
      | Sexp.List l -> l
    in
    ListLabels.iter sexps ~f:(fun kd_sexp -> 
      let (k,d) = pair_of_sexp k_of_sexp d_of_sexp kd_sexp in
      if find t k = None then 
        T.add t ~key:k ~data:d
      else 
        Conv.of_sexp_error "Hashtbl: duplicate key" kd_sexp
    );
    t

  module Infix = struct
    (** {!Hashtbl.find_exn} *)
    let ( |> ) htbl key = T.find htbl key
    (** {!Hashtbl.find} *)
    let ( |?> ) htbl key = find htbl key
    (** {!Hashtbl.add} *)
    let ( <| ) htbl (key,data) = T.add htbl ~key ~data
  end

end

include Extend (struct
  include H
  type 'a key = 'a
end)

module Make (Ht : HashedType) = struct
  module M = H.Make (Ht)
  include M

  include Extend (struct
    type ('a, 'b) t = 'b M.t
    type 'a key = Ht.t
    let create = M.create
    let find = M.find
    let add = M.add
    let fold = M.fold
    let iter = M.iter
  end)
  let sexp_of_t f x = sexp_of_t Ht.sexp_of_t f x
  let t_of_sexp f x = t_of_sexp Ht.t_of_sexp f x
end
