open Interfaces

type t = char

include Comparable with type comparable = t
include Sexpable with type sexpable = t

val to_int : t -> int

val of_int : int -> t option

val of_int_exn : int -> t

val unsafe_of_int : int -> t

val escaped : char -> string

val lowercase : char -> char

val uppercase : char -> char

val to_string : t -> string

(** Predicates *)
val is_digit : t -> bool  (* '0' '1' '2' ... '9' *)
val is_lowercase : t -> bool (* 'a' 'b' 'c' ... 'z' *)
val is_print : t -> bool (* *)
val is_uppercase : t -> bool (* 'A' 'B' 'C' ... 'Z' *)
val is_whitespace : t -> bool (* ' ' '\n' '\t' '\r' *)

(** [get_digit 'i'] = [Some i] if [is_digit i] and [None] otherwise. *)
val get_digit : t -> int option

val get_digit_exn : t -> int
