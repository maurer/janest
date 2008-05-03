(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(** Basic types and definitions required throughout the system. *)

exception Bug of string
exception Finally of exn * exn
(** Raised when finalization after an exception failed, too.
    The first exception argument is the one raised by the initial
    function, the second exception the one raised by the finalizer. *)
exception Validation_error of string list
exception Unimplemented of string

type decimal = float with bin_io, sexp
type 'a set = 'a PSet.t
type ('a,'b) map = ('a,'b) PMap.t

type ('a,'b) result = ('a,'b) Result.t = Ok of 'a | Error of 'b
type 'a bound = Incl of 'a | Excl of 'a | Unbounded
type passfail = Pass | Fail of string

(** handy types for marking things read-only and read-write *)

type immutable with sexp, bin_io
type read_only with sexp, bin_io
type read_write with sexp, bin_io
type write_only with sexp, bin_io

(** {6 Integer mod and div} *)

(** mod and div operators that have the right behavior on negative numbers,
    that is, [x % y] always returns a positive int between 0 and y-1.
    Invariant: [if r = a % b && q = a /% b then q * b + r = a] *)
val ( % ) : int -> int -> int
val ( /% ) : int -> int -> int

(** {6 Error handling} *)
val protectx : f:('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b
(** A common idiom used in functional languages, that executes [f] and
    afterwards executes [finally], whether [f] throws an exception or
    not.*)

val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a

val critical_section : Mutex.t -> f:(unit -> 'a) -> 'a

(** {6 Input Output}*)

val read_wrap : ?binary:bool -> f:(in_channel -> 'a) -> string -> 'a
(** [read_wrap ~f fname] executes [~f] on the open input channel from
    [fname], and closes it afterwards.  Opens channel in binary mode iff
    [binary] is true. *)

val write_wrap : ?binary:bool -> f:(out_channel -> 'a) -> string -> 'a
(** [write_wrap ~f fname] executes [~f] on the open output channel from
    [fname], and closes it afterwards.  Opens channel in binary mode iff
    [binary] is true. *)

val input_lines : ?fix_win_eol:bool -> in_channel -> string list
(** Completely reads an input channel and returns the results as a list of
    strings. Each line in one string. *)


(**{6 triple handling }*)
(*
  CRv2 tvaroquaux these are now redundant with the functions in tupple3
*)
val fst3 : ('a*_*_) -> 'a
(** Returns the first element of a triple. *)

val snd3 : (_*'a*_) -> 'a
(** Returns the second element of a triple. *)

val trd3 : (_*_*'a) -> 'a
(** Returns the third element of a triple. *)

(**
   {6 Option handling}
*)
(*
  CRv2 tvaroquaux: These functions are rebinds from the option module and they
  should be considered as deprecated
*)
val opt_map : ('a -> 'b) -> 'a option -> 'b option
val may : ('a -> unit) -> 'a option -> unit
val uw : 'a option -> 'a
val uw_default : 'a -> 'a option -> 'a

val forever : (unit -> unit) -> exn
(** [forever f] runs [f ()] until it throws an exception and returns the exception.
    This function is useful for read_line loops, etc. *)


(** The identity function*)
external ident : 'a -> 'a = "%identity"

(*CRv2 tvaroquaux
  external ascending : 'a -> 'a -> int = "%compare "
*)
val ascending : 'a -> 'a -> int
(** A comparator that returns results in ascending order. *)
val descending : 'a -> 'a -> int
(** A comparator that returns results in descending order. *)

val ( |! ) : 'a -> ( 'a -> 'b) -> 'b
(** A 'pipe' operator. *)

val ( ||> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Reverse function composition. *)

val (<||) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Function composition *)

val (^/) : string -> string -> string
(** same as [Filename.concat]*)

val failwithf :  ('a, unit, string, unit -> 'b) format4 -> 'a
val invalid_argf :  ('a, unit, string, unit -> 'b) format4 -> 'a

val register_pretty_printer : string -> unit

(* DEPRECATED: Use [Exn] module. *)
val register_exn_converter : (exn -> string option) -> unit
val exn_to_string : exn -> string
val sexp_of_exn : exn -> Sexplib.Sexp.t
