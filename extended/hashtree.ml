(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Hashtree"

(** Hashtables with buckets implemented as trees.*)
open Core.Std

type ('k, 'v) t = {
  mutable table : ('k, 'v) Rmap.t array;
  mutable length : int;
} with sexp, bin_io

type ('k, 'v) sexpable = ('k, 'v) t
type ('k, 'v) binable = ('k, 'v) t

let create size =
  {table = Array.init size ~f:(fun _ -> Rmap.create ());length = 0}
let slot t key = Hashtbl.hash key mod (Array.length t.table)

let really_add t ~key ~data =
  t.length <- t.length + 1;
  Rmap.add t.table.(slot t key) ~key ~data

let maybe_resize_table t =
  if t.length >= Array.length t.table * 10 then begin
    let new_table =
      Array.init (Array.length t.table * 10) ~f:(fun _ -> Rmap.create ())
    in
    let old_table = t.table in
    t.table <- new_table;
    t.length <- 0;
    Array.iter ~f:(fun l ->
      Rmap.iter l ~f:(fun ~key ~data ->
        really_add t ~key ~data))
      old_table
  end

let add t ~key ~data =
  maybe_resize_table t;
  really_add t ~key ~data

let find t key = Rmap.find t.table.(slot t key) key
let remove t key = Rmap.remove t.table.(slot t key) key
let length t = t.length
let fold t ~init ~f =
  Array.fold_left t.table ~init ~f:(fun init r -> Rmap.fold r ~init ~f)


module type Key = sig
  type t
  include Sexpable with type sexpable = t

  val hash : t -> int
  val compare : t -> t -> int
end

module type S = sig
  module Key : Key

  type 'a t

  include Sexpable.S1 with type 'a sexpable = 'a t

  val create : int -> 'a t
  val add : 'a t -> key:Key.t -> data:'a -> unit
  val remove : 'a t -> Key.t -> unit
  val find : 'a t -> Key.t -> 'a option
  val length : 'a t -> int
  val fold : 'a t -> init:'b -> f:(key:Key.t -> data:'a -> 'b -> 'b) -> 'b
end

module Make (Key : Key) : S with module Key = Key = struct
  module Key = Key
  module Rmap = Rmap.Make (Key)

  type 'a t = {
    mutable table : 'a Rmap.t array;
    mutable length : int;
  } with sexp

  type 'a sexpable = 'a t

  let create size =
    {table = Array.init size ~f:(fun _ -> Rmap.create ());length = 0}
  let slot t key = Key.hash key mod (Array.length t.table)

  let really_add t ~key ~data =
    t.length <- t.length + 1;
    Rmap.add t.table.(slot t key) ~key ~data

  let maybe_resize_table t =
    if t.length >= Array.length t.table * 10 then begin
      let new_table =
        Array.init (Array.length t.table * 10) ~f:(fun _ -> Rmap.create ())
      in
      let old_table = t.table in
      t.table <- new_table;
      t.length <- 0;
      Array.iter ~f:(fun l ->
        Rmap.iter l ~f:(fun ~key ~data ->
          really_add t ~key ~data))
        old_table
    end

  let add t ~key ~data =
    maybe_resize_table t;
    really_add t ~key ~data

  let find t key = Rmap.find t.table.(slot t key) key
  let remove t key = Rmap.remove t.table.(slot t key) key
  let length t = t.length
  let fold t ~init ~f =
    Array.fold_left t.table ~init ~f:(fun init r -> Rmap.fold r ~init ~f)    
end
