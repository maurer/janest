(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(** Basic types and definitions required throughout the system. *)

TYPE_CONV_PATH "Core.Common"

open StdLabels
open MoreLabels
open Printf
open Sexplib.Conv
open Bin_prot

(** handy types for marking things read-only and read-write *)
type immutable with bin_io, sexp
type read_only with bin_io, sexp
type read_write with bin_io, sexp
type write_only with bin_io, sexp

let sexp_of_immutable _ = failwith "attempt to convert abstract type immutable"
let sexp_of_read_only _ = failwith "attempt to convert abstract type read_only"
let sexp_of_read_write _ = failwith "attempt to convert abstract type read_write"
let sexp_of_write_only _ = failwith "attempt to convert abstract type write_only"

type 'a set = 'a PSet.t
type ('a,'b) map = ('a,'b) PMap.t

include (Exn : sig
  exception Finally of exn * exn
end)

let exn_to_string = Exn.to_string
let register_exn_converter = Exn.register_converter
let sexp_of_exn = Exn.sexp_of_t
let protectx = Exn.protectx
let protect = Exn.protect

let critical_section = Core_mutex.critical_section

(** Optional function application. The function is only applied if the option has
    contents (i.e. not [None]). *)
let opt_map f o = Option.map ~f o

(** [may f (Some x)] applies function [f] to [x]. [may f None] does nothing. *)
let may f o = Option.iter ~f o

(** [read_wrap ~f fname] executes [~f] on the open input channel from
    [fname], and closes it afterwards.  Opens channel in binary mode iff
    [binary] is true. *)
let read_wrap ?(binary = false) ~f fname =
  let ic =
    if binary then
      open_in_bin fname
    else
      open_in fname
  in
  protectx ic ~f ~finally:close_in

(** [write_wrap ~f fname] executes [~f] on the open output channel from
    [fname], and closes it afterwards.  Opens channel in binary mode iff
    [binary] is true. *)
let write_wrap ?binary ~f fname =
  let oc =
    match binary with
    | None | Some false -> open_out fname
    | Some true -> open_out_bin fname
  in
  protectx oc ~f ~finally:close_out

let input_lines = In_channel.input_lines

(** unwraps an option, throwing [Not_found] if it is [None] *)
let uw = function Some x -> x | None -> raise Not_found

(** unwraps an option, returning the default value if it is [None] *)
let uw_default default_value o = Option.value o ~default:default_value

(** [forever f] runs [f ()] until it throws an exception and returns the exception.
    This function is useful for read_line loops, etc. *)
let forever f =
  let rec forever () =
    f ();
    forever ()
  in
  try forever ()
  with e -> e

(** identity function *)
external ident : 'a -> 'a = "%identity"

(** Operators for picking apart tuples *)

(** Returns the first element of a triple. *)
let fst3 (x,_,_) = x

(** Returns the second element of a triple. *)
let snd3 (_,y,_) = y

(** Returns the third element of a triple. *)
let trd3 (_,_,z) = z

(** A comparator that returns results in ascending order. *)
let ascending = compare

(** A comparator that returns results in descending order. *)
let descending x y = compare y x

(** Extensible exception printer *)

open Sexplib.Sexp

let register_pretty_printer = Pretty_printer.register

let failwithf = Core_printf.failwithf
let invalid_argf = Core_printf.invalid_argf

(** Equivalent to Filename.concat *)
let ( ^/ ) = Filename.concat

(* CRv2 sweeks: these should be moved into [Core_int]. *)
(** mod and div operators that have the right behavior on negative numbers,
  that is, [x % y] always returns a positive int between 0 and y-1.
    Invariant: [if r = a % b && q = a /% b then q * b + r = a] *)
let ( % ) x y =
  if y <= 0 then invalid_arg "% in common.ml: modulus should be positive";
  let rval = x mod y in
  if rval < 0
  then rval + y
  else rval

let ( /% ) x y =
  if y <= 0 then invalid_arg "/% in common.ml: modulus should be positive";
  if x < 0
  then (x - y + 1) / y
  else x / y

(** A 'pipe' operator. *)
let ( |! ) x y = y x
(* CRv2 / SL/RG: maybe |$ instead? *)
let ( ^$ ) f x = f x

(** Function composition

    F# uses (>>) and (<<) but (>>) conflicts with anonymous bind...
 *)
let (||>) f g = fun x -> g (f x)
let (<||) f g = fun x -> f (g x)

(** float division of integers *)
let (//) x y = float x /. float y

type decimal = float with sexp, bin_io
let sexp_of_decimal x = Sexplib.Sexp.Atom (sprintf "%.12G" x)
let decimal_of_sexp = function
    Sexplib.Sexp.Atom s -> float_of_string s
  | s -> Sexplib.Conv.of_sexp_error
      "decimal_of_sexp: Expected Atom, found List" s

type ('a,'b) result = ('a,'b) Result.t = Ok of 'a | Error of 'b
type 'a bound = Incl of 'a | Excl of 'a | Unbounded
type passfail = Pass | Fail of string

exception Validation_error of string list
exception Unimplemented of string
exception Bug of string
exception Uninitialized_value of string

let () = register_exn_converter (function
  | Validation_error errors -> Some ("Validation_error " ^ String.concat ~sep:", " errors)
  | Unimplemented s -> Some ("Unimplemented " ^ s)
  | Bug s -> Some ("Bug " ^ s)
  | Uninitialized_value s -> Some ("Uninitialized_value " ^ s)
  | _ -> None )

(* CR sweeks: override kprintf with `Please_use_ksprintf *)
