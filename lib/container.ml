(* This file has generic signatures for container data structures, with standard
   functions (iter, fold, exists, for_all, ...) that one would expect to find in
   any container.  The idea is to include [Container.S0] or [Container.S1] in
   the signature for every container-like data structure (Array, List, String,
   ...) to ensure a consistent interface.
*)
 
(* CRv2 SW: Make the following modules meet S0.
   Bigstring
*)
module type S0 = sig
  type container
  type elt
  val length : container -> int
  val is_empty : container -> bool
  val iter : container -> f:(elt -> unit) -> unit
  val fold : container -> init:'b -> f:('b -> elt -> 'b) -> 'b
  val exists : container -> f:(elt -> bool) -> bool
  val for_all : container -> f:(elt -> bool) -> bool
  val find : container -> f:(elt -> bool) -> elt option
  val to_list : container -> elt list
  val to_array : container -> elt array
end

module type S0_phantom = sig
  type elt
  type 'a container
  val length : 'a container -> int
  val is_empty : 'a container -> bool
  val iter : 'a container -> f:(elt -> unit) -> unit
  val fold : 'a container -> init:'b -> f:('b -> elt -> 'b) -> 'b
  val exists : 'a container -> f:(elt -> bool) -> bool
  val for_all : 'a container -> f:(elt -> bool) -> bool
  val find : 'a container -> f:(elt -> bool) -> elt option
  val to_list : 'a container -> elt list
  val to_array : 'a container -> elt array
end

(* CRv2 sweeks: Make the following modules meet S1.
   Core_queue
   Fqueue
   Heap
   PSet
*)
module type S1 = sig
  type 'a container
  val length : 'a container -> int
  val is_empty : 'a container -> bool
  val iter : 'a container -> f:('a -> unit) -> unit
  val fold : 'a container -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val exists : 'a container -> f:('a -> bool) -> bool
  val for_all : 'a container -> f:('a -> bool) -> bool
  val find : 'a container -> f:('a -> bool) -> 'a option
  val to_list : 'a container -> 'a list
  val to_array : 'a container -> 'a array
end

(* The following functors exist as a consistency check among all the various
   [S?] interfaces.  They ensure that each particular [S?] is an instance of
   a more generic signature. *)
module Check (T : sig
  type 'a elt
  type 'a container
end) (M : sig
  open T
  val length : 'a container -> int
  val is_empty : 'a container -> bool
  val iter : 'a container -> f:('a elt -> unit) -> unit
  val fold : 'a container -> init:'b -> f:('b -> 'a elt -> 'b) -> 'b
  val exists : 'a container -> f:('a elt -> bool) -> bool
  val for_all : 'a container -> f:('a elt -> bool) -> bool
  val find : 'a container -> f:('a elt -> bool) -> 'a elt option
  val to_list : 'a container -> 'a elt list
  val to_array : 'a container -> 'a elt array
end) = struct end

module Check_S0 (M : S0) =
  Check (struct
    type 'a elt = M.elt
    type 'a container = M.container
  end) (M)

module Check_S0_phantom (M : S0_phantom) =
  Check (struct
    type 'a elt = M.elt
    type 'a container = 'a M.container
  end) (M)

module Check_S1 (M : S1) =
  Check (struct
    type 'a elt = 'a
    type 'a container = 'a M.container
  end)
