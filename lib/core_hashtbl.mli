(* Core_hashtbl is an extension of the standard MoreLabels.Hashtbl.  It
   replaces "find" with "find_exn", adds "find" that returns an option, and
   new functions "find_default", and "of_alist".  It also extends Hashtbl.Make
   so that the resulting structure includes those functions.  *)

open Core_hashtbl_intf

include S2

module type HashedType = HashedType

module type S = S

module Make (H : HashedType) : S with type key = H.t

val hash : 'a -> int
val hash_param : int -> int -> 'a -> int

module Make_binable (H : sig
  include HashedType
  include Binable.S with type binable = t
end) : sig
  include S with type key = H.t
  include Binable.S1 with type 'a binable = 'a t
end
