type t = float

include Sexpable.S with type sexpable = t
include Binable.S with type binable = t
include Floatable.S with type floatable = t
include Stringable.S with type stringable = t
include Hashable.S with type hashable = t
(* [max] and [min] will return nan if either argument is nan *)
include Comparable.S with type comparable = t
(* The results of robust comparisons on [nan] should be considered undefined. *)
include Robustly_comparable.S with type robustly_comparable = t

val max_value : t                   (* infinity *)
val min_value : t                   (* neg_infinity *)
val zero : t
val epsilon : t



val of_int : int -> t
val to_int : t -> int
val of_int64 : int64 -> t
val to_int64 : t -> int64

(* overrides of Pervasives functions *)
val truncate : t -> int                 (* closer to 0 *)
val round : t -> t                    (* nearest *)
val iround : t -> int option
val iround_exn : t -> int

(** Ordinary t-only nan test. *)
val is_nan : t -> bool

(** Ordinary t-only infinity test. *)
val is_inf : t -> bool

(** min that returns the other value if one of the values is a [nan]. *)
val min_inan : t -> t -> t
(** max that returns the other value if one of the values is a [nan]. *)
val max_inan : t -> t -> t

val (+) : t -> t -> t
val (-) : t -> t -> t
val ( * ) : t -> t -> t
val (/) : t -> t -> t

val modf : float -> float * float
val floor : float -> float
val ceil : float -> float

val mod_float : float -> float -> float

(* mostly modules that inherit from t, since the infix operators are more convenient *)
val add : t -> t -> t
val sub : t -> t -> t
val scale : t -> t -> t
val abs : t -> t

module Class : sig
  type t =
  | Infinite
  | Nan
  | Normal
  | Subnormal
  | Zero

  include Binable.S with type binable = t
  include Sexpable.S with type sexpable = t
  include Stringable.S with type stringable = t
end

val classify : t -> Class.t

module Sign : sig
  type t = Neg | Zero | Pos
end

val sign : t -> Sign.t
