(* float stuff *)

type t = float

include Sexpable.S with type sexpable = t
include Binable.S with type binable = t
include Floatable.S with type floatable = t
include Stringable.S with type stringable = t
include Hashable.S with type hashable = t
include Comparable.S with type comparable = t
include Robustly_comparable.S with type robustly_comparable = t

val max_value : t                   (* infinity *)
val min_value : t                   (* neg_infinity *)
val min_normal_pos : t Lazy.t       (* smallest positive normal *)
val zero : t
val epsilon : t

val of_int : int -> t
val to_int : t -> int
  
(* overrides of Pervasives functions *)
val truncate : t -> int
val round : t -> int

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

(* mostly modules that inherit from t, since the infix operators are more convenient *)
val add : t -> t -> t
val sub : t -> t -> t
val scale : t -> t -> t
val abs : t -> t

val to_string_hum : t -> string

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

