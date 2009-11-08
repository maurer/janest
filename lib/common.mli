(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(** Basic types and definitions required throughout the system. *)

exception Bug of string

(** Raised when finalization after an exception failed, too.
    The first exception argument is the one raised by the initial
    function, the second exception the one raised by the finalizer. *)
exception Finally of exn * exn

exception Validation_error of string list

exception Unimplemented of string

(* The sexps of this type only have 12 digits after the decimal. Therefore they're less
   annoying to look at than the sexps of floats. *)
type decimal = float with bin_io, sexp

type ('a,'b) result = ('a,'b) Result.t = Ok of 'a | Error of 'b
type 'a bound = Incl of 'a | Excl of 'a | Unbounded



type passfail = Pass | Fail of string


(** handy types for marking things read-only and read-write *)
type immutable with sexp, bin_io
type read_only with sexp, bin_io
type read_write with sexp, bin_io
type write_only with sexp, bin_io

(** [never_returns] should be used as the return type of functions that don't return and
 * might block forever, rather than ['a] or [_].  This forces callers of such functions to
 * have a call to [never_returns] at the call site, which makes it clear to readers what's
 * going on. We do not intend to use this type for functions such as [failwithf] that
 * always raise an exception. *)
type never_returns
val never_returns : never_returns -> _

(** {6 Error handling} *)
(** See exn.mli *)
val protectx : f:('a -> 'b) -> 'a -> finally:('a -> unit) -> 'b
val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a


val critical_section : Mutex.t -> f:(unit -> 'a) -> 'a

(** {6 Input Output}*)




(** [read_wrap ~f fname] executes [~f] on the open input channel from
    [fname], and closes it afterwards.  Opens channel in binary mode iff
    [binary] is true. *)
val read_wrap : ?binary:bool -> f:(in_channel -> 'a) -> string -> 'a


(** [write_wrap ~f fname] executes [~f] on the open output channel from
    [fname], and closes it afterwards.  Opens channel in binary mode iff
    [binary] is true. *)
val write_wrap : ?binary:bool -> f:(out_channel -> 'a) -> string -> 'a


(** [write_lines fname lines] writes each string in [lines] (plus a newlnie) to file
    [fname]. *)
val write_lines : string -> string list -> unit


(** Completely reads an input channel and returns the results as a list of
    strings. Each line in one string. *)
val input_lines : ?fix_win_eol:bool -> in_channel -> string list


(** [read_lines filename] Opens filename, reads all lines, and closes the file. *)
val read_lines : string -> string list

(**{6 triple handling }*)
val fst3 : ('a * _ * _) -> 'a
val snd3 : (_ * 'a * _) -> 'a
val trd3 : (_ * _ * 'a) -> 'a

(**{6 space safe double and triple handling }*)


val ss_fst : ('a, _) Space_safe_tuple.T2.t -> 'a
val ss_snd : ( _,'a) Space_safe_tuple.T2.t -> 'a
val ss_fst3 : ('a, _, _) Space_safe_tuple.T3.t -> 'a
val ss_snd3 : ( _,'a, _) Space_safe_tuple.T3.t -> 'a
val ss_trd3 : ( _, _,'a) Space_safe_tuple.T3.t -> 'a

(**
   {6 Option handling}
*)

val may : ('a -> unit) -> 'a option -> unit
val uw : 'a option -> 'a

(** {6 Functions from function.ml} *)
val (|!) : 'a -> ('a -> 'b) -> 'b
val ident : 'a -> 'a
val const : 'a -> _ -> 'a

(** A comparator that returns results in ascending order. *)
external ascending : 'a -> 'a -> int = "%compare"
(** A comparator that returns results in descending order. *)
val descending : 'a -> 'a -> int

val (^/) : string -> string -> string
(** same as [Filename.concat]*)

val failwithf :  ('a, unit, string, unit -> _) format4 -> 'a
val invalid_argf :  ('a, unit, string, unit -> _) format4 -> 'a
val exitf : ('a, unit, string, unit -> _) format4 -> 'a

(** toplevel binding for polymorphic equality (=).  Named for easy use in
    labelled arguments (one can do [f x y ~equal]).
*)
val equal : 'a -> 'a -> bool

(* We disable [==] and [!=] and replace them with the longer and more mnemonic
   [phys_equal] because they too easily lead to mistakes (for example
   they don't even work right on Int64 or Float).  One can usually use the
   [equal] function for a specific type, or use (=) or (<>) for built in types
   like char, int, float, ...
*)
val phys_equal : 'a -> 'a -> bool
val (==) : 'a -> 'a -> [ `Consider_using_phys_equal ]
val (!=) : 'a -> 'a -> [ `Consider_using_phys_equal ]


val kprintf : _ -> [ `Please_use_ksprintf ]



(* override Pervasives methods that need LargeFile support *)
val seek_out : out_channel -> int64 -> unit
val pos_out : out_channel -> int64
val out_channel_length : out_channel -> int64
val seek_in : in_channel -> int64 -> unit
val pos_in : in_channel -> int64
val in_channel_length : in_channel -> int64
