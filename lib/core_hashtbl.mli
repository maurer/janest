open Sexplib
(* Core_hashtbl is an extension of the standard MoreLabels.Hashtbl.  It replaces
   "find" with "find_exn", adds "find" that returns an option, and new funcitons
  "find_default", and "of_alist".  It also extends Hashtbl.Make so
   that the resulting structure includes those functions.  *)

type ('a, 'b) t = ('a, 'b) MoreLabels.Hashtbl.t

(* These are standard Hashtbl functions. *)
val add : ('a, 'b) t -> key:'a -> data:'b -> unit
val clear : ('a, 'b) t -> unit
val copy : ('a, 'b) t -> ('a, 'b) t
val create : int -> ('a, 'b) t
val find_all : ('a, 'b) t -> 'a -> 'b list
val fold : f:(key:'a -> data:'b -> 'c -> 'c) -> ('a, 'b) t -> init:'c -> 'c
val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
val length : ('a, 'b) t -> int
val mem : ('a, 'b) t -> 'a -> bool
val remove : ('a, 'b) t -> 'a -> unit
val replace : ('a, 'b) t -> key:'a -> data:'b -> unit

(* [find_default t k ~default] returns the data associated with key k if it
   is in the table t, otherwise it lets d = default() and adds it to the table.
*)
val find_default : ('a, 'b) t -> 'a -> default:(unit -> 'b) -> 'b 

(* [find t k] returns Some (the current binding) of k in t, or None if no such binding exists *)
val find : ('a, 'b) t -> 'a -> 'b option

(* [find_exn t k] returns the current binding of k in t, or raises Not_found if no such binding exists.*)
val find_exn : ('a, 'b) t -> 'a -> 'b

(* [iter_vals t ~f] is like iter, except it only supplies the value to f, not
   the key. *)
val iter_vals : f:('b -> unit) -> ('a, 'b) t -> unit

(* [of_alist l] returns a new hashtable populated with the supplied data *)
val of_alist : ('a * 'b) list -> ('a, 'b) t

(* Returns the list of all (key,data) pairs for given hashtable. *)
val to_alist : ('a, 'b) t -> ('a * 'b) list

(* Returns the list of all keys for given hashtable. *)
val keys : ('a, 'b) t -> 'a list

(* Returns the list of all data for given hashtable. *)
val data : ('a, 'b) t -> 'b list

val sexp_of_t : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a,'b) t -> Sexp.t
val t_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a,'b) t

module Infix : sig
  val ( |> ) : ('a, 'b) t -> 'a -> 'b
  val ( |?> ) : ('a, 'b) t -> 'a -> 'b option
  val ( <| ) : ('a, 'b) t -> 'a * 'b -> unit
end

module type HashedType = sig 
  include MoreLabels.Hashtbl.HashedType
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module type S = sig
  type key 
  type 'a t 
  val create : int -> 'a t
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key:key -> data:'a -> unit
  val remove : 'a t -> key -> unit
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key:key -> data:'a -> unit
  val mem : 'a t -> key -> bool
  val iter : f:(key:key -> data:'a -> unit) ->
         'a t -> unit
  val fold : f:(key:key -> data:'a -> 'b -> 'b) ->
         'a t -> init:'b -> 'b
  val length : 'a t -> int
  
  val find_default : 'a t -> key -> default:(unit -> 'a) -> 'a
  val find : 'a t -> key -> 'a option
  val find_exn : 'a t -> key -> 'a
  val iter_vals : f:('a -> unit) -> 'a t -> unit
  val of_alist : (key * 'a) list -> 'a t
  val to_alist : 'a t -> (key * 'a) list
  val keys : 'a t -> key list
  val data : 'a t -> 'a list
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  module Infix : sig
    val ( |> ) : 'a t -> key -> 'a
    val ( |?> ) : 'a t -> key -> 'a option
    val ( <| ) : 'a t -> key * 'a -> unit
  end
end

module Make: functor (H : HashedType) -> S with type key = H.t

val hash : 'a -> int
val hash_param : int -> int -> 'a -> int
