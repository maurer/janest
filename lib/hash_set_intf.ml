module List = StdLabels.List
open Sexplib
open Sexplib.Conv



module type Types = sig
  type 'a elt
  type 'a t
end

(* Gen is a signature functor that generalizes the monomorpic hash set and
   polymorphic hash set. *)
module Gen (T : Types) = struct
  open T
  module type S = sig
    val create : int -> 'a t
    val add : 'a t -> 'a elt -> unit
    (** [strict_add] fails if element is already there *)
    val strict_add : 'a t -> 'a elt -> unit 
    val remove : 'a t -> 'a elt -> unit
    (** [strict_remove] fails if element wasn't there *)
    val strict_remove : 'a t -> 'a elt -> unit
    val clear : 'a t -> unit
    val fold : f:('a -> 'b elt -> 'a) -> init:'a -> 'b t -> 'a
    val iter : f:('a elt -> unit) -> 'a t -> unit
    val length : 'a t -> int
    val mem : 'a t -> 'a elt -> bool
    val is_empty : 'a t -> bool
    val of_list : 'a elt list -> 'a t
    val to_list : 'a t -> 'a elt list
    val equal : 'a t -> 'a t -> bool
  end
end

module type S = sig
  type elt
  type t
  module T : Types with type 'a elt = elt with type 'a t = t
  
  val create : int -> t
  val add : t -> elt -> unit
  val strict_add : t -> elt -> unit
  val remove : t -> elt -> unit
  val strict_remove : t -> elt -> unit
  val clear : t -> unit
  val fold : f:('a -> elt -> 'a) -> init:'a -> t -> 'a
  val iter : f:(elt -> unit) -> t -> unit
  val length : t -> int
  val mem : t -> elt -> bool
  val is_empty : t -> bool
  val of_list : elt list -> t
  val to_list : t -> elt list
  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module type S_binable = sig
  include S
  include Binable.S with type binable = t
end

module type S1 = sig
  type 'a t
  module T : Types with type 'a elt = 'a with type 'a t = 'a t

  val create : int -> 'a t
  val add : 'a t -> 'a -> unit
  val strict_add : 'a t -> 'a -> unit
  val remove : 'a t -> 'a -> unit
  val strict_remove : 'a t -> 'a -> unit
  val clear : 'a t -> unit
  val fold : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a
  val iter : f:('a -> unit) -> 'a t -> unit
  val length : 'a t -> int
  val mem : 'a t -> 'a -> bool
  val is_empty : 'a t -> bool
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val equal : 'a t -> 'a t -> bool
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
end

(* Check that S and S1 are instances of Gen *)
module Check_S (M : S) = (M : Gen(M.T).S)
module Check_S1 (M : S1) = (M : Gen(M.T).S)
