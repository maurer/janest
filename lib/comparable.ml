(*CRv2 TV:
  This module should probably be split in an Equality and a Comparable module
  (*With the Comparable module including the Equality module*).
*)
(* sweeks: That would be nice.  We now have an Equatable module.  Unfortunately,
   if we include Equatable.Infix here, then everyone who wants a module to
   be comparable would also have to define "type equatable = t", which is a
   little annoying.  Sigh, if we only had used signature functors.
*)
module type Infix = sig
  type comparable
  val ( >= ) : comparable -> comparable -> bool
  val ( <= ) : comparable -> comparable -> bool
  (* CRv2 sweeks: I wonder if rebinding (=) is a bad idea, since it is so commonly
     used in its overloaded form.  Perhaps we should just put [equal] in [S]?

     Ron and I talked about this, and decided to keep it as =, since we don't
     open things nearly as much any more, so override the toplevel polymorphic
     = isn't a problem.

     As to whether we should always use "=" or "equal" in modules, we are
     undecided.
  *)
  val ( = ) : comparable -> comparable -> bool
  val ( > ) : comparable -> comparable -> bool
  val ( < ) : comparable -> comparable -> bool
  val ( <> ) : comparable -> comparable -> bool
end

module type S = sig
  include Infix
  (* CRv2 sweeks: replace "int" return type with [ Less | Greater | Equal ] *)
  val compare : comparable -> comparable -> int
  val ascending : comparable -> comparable -> int
  val descending : comparable -> comparable -> int  
  val min : comparable -> comparable -> comparable
  val max : comparable -> comparable -> comparable
  (* CRv2 CF: get a Map for free, through a functor *)
  (* CRv2 TV: we should also be able to get a free Set module*)
end

module Poly (T : sig type t end) : S with type comparable = T.t = struct
  type comparable = T.t
  include Pervasives
  let ascending = compare
  let descending x y = compare y x  
end

module From_compare (T : sig
  type t
  val compare : t -> t -> int
end) : S with type comparable = T.t = struct
  type comparable = T.t

  let compare = T.compare
  let ascending = compare
  let descending t t' = compare t' t
    
  module Infix = struct
    let (>) a b = compare a b > 0
    let (<) a b = compare a b < 0
    let (>=) a b = compare a b >= 0
    let (<=) a b = compare a b <= 0
    let (=) a b = compare a b = 0
    let (<>) a b = compare a b <> 0
  end
  include Infix
      
  let min t t' = if t <= t' then t else t'
  let max t t' = if t >= t' then t else t'
end

(** Inherit comparability from a component. *)
module Inherit (C : S) (T : sig type t val component : t -> C.comparable end)
  : S with type comparable = T.t = struct

    type comparable = T.t
    let binary f t t' = f (T.component t) (T.component t')
    let compare = binary C.compare
    let (>=) = binary C.(>=)
    let (<=) = binary C.(<=)
    let (=) = binary C.(=)
    let (>) = binary C.(>)
    let (<) = binary C.(<)
    let (<>) = binary C.(<>)
    let ascending = binary C.ascending
    let descending = binary C.descending
    let min t t' = if t <= t' then t else t'
    let max t t' = if t >= t' then t else t'
end
