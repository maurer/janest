open Std_internal

(** piece-wise linear interpolation from float-like types to float *)

module type Key = sig
  type t
  include Floatable with type floatable = t
  include Sexpable with type sexpable = t
end

module type S = sig
  type key

  type t
  include Sexpable with type sexpable = t

  val create : (key * float) list -> (t, string) Result.t
  val get : t -> key -> float
end

module Make (Key : Key) : S with type key = Key.t

module Time : S with type key = Time.t
module Float : S with type key = float
module Int : S with type key = int
