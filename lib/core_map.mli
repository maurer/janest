(** Polymorphic map module.  Just like the standard Map module, but with the
    order of the two type parameters inverted (we think [key,value] makes a lot
    more sense than [value,key]).  There are also a few additional Map
    operations in here.  If you open Core.Std, this is your Map module. *)

(** {6 Polymorphic versions of standard Map operations} *)

open Core_map_intf

(* S2 is a polymorphic map *)
include S2

module type Key = Key
module type S = S

module Make (Key : Key) : S with type key = Key.t

module Make_binable (Key : sig
  include Key
  include Binable.S with type binable = t
end) : sig
  
  type 'a t = 'a Make(Key).t
  include Binable.S1 with type 'a binable = 'a t
end
