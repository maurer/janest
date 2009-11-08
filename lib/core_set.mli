(** Polymorphic set module.  Just like the standard Set module, but
    with some extra Set operations.  If you open Core.Std, this is your
    Set module. *)

(** {6 Polymorphic versions of standard Set operations} *)

open Core_set_intf

(* S1 is a polymorphic set *)
include S1

module type Elt = Elt
module type S = S

module Make (Elt : Elt) : S with type elt = Elt.t

module Make_binable (Elt : sig
  include Elt
  include Binable.S with type binable = t
end) : sig
  include S with type elt = Elt.t
  include Binable.S with type binable = t
end
