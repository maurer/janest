open Std_internal

module type Unit_ref = sig
  type t

  val create : unit -> t
  val equal : t -> t -> bool
end

module Unit_ref (M : sig end) : Unit_ref = struct
  type t = unit ref

  let create () = ref ()

  let equal (t : t) t' = t == t'
end

(** an abstract unique identifier based on 64 bit integers. *)

module type Int64 = sig
  type t

  include Binable with type binable = t
  include Comparable with type comparable = t
  include Floatable with type floatable = t
  include Hashable with type hashable = t
  include Setable with type setable = t
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t

  val create : unit -> t
end 

(* CRv2 sweeks: It might be nice to pass a string to [Make] that is used
   as a prefix for pretty printing, to make it easier to read unique names
   and distinguish among different ones.
*)
module Int64 (Z : sig end) : Int64 = struct
  include Core_int64

  let current = ref 0L

  let create () = 
    let x = !current in
    current := succ x;
    x
end

