(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(** Basic types and definitions required throughout the system. *)

TYPE_CONV_PATH "Core.Common"

open StdLabels
open MoreLabels
open Printf
open Sexplib.Conv
open Bin_prot

(* we want 64bit file operations as the default *)
include Pervasives.LargeFile

(** handy types for marking things read-only and read-write *)
type immutable with bin_io, sexp
type read_only with bin_io, sexp
type read_write with bin_io, sexp
type write_only with bin_io, sexp

let sexp_of_immutable _ = failwith "attempt to convert abstract type immutable"
let immutable_of_sexp _ = failwith "attempt to convert abstract type immutable"
let sexp_of_read_only _ = failwith "attempt to convert abstract type read_only"
let read_only_of_sexp _ = failwith "attempt to convert abstract type read_only"
let sexp_of_read_write _ = failwith "attempt to convert abstract type read_write"
let read_write_of_sexp _ = failwith "attempt to convert abstract type read_write"
let sexp_of_write_only _ = failwith "attempt to convert abstract type write_only"
let write_only_of_sexp _ = failwith "attempt to convert abstract type write_only"

type never_returns
let never_returns (_ : never_returns) = assert false

include (Exn : sig
  exception Finally of exn * exn
end)

let protectx = Exn.protectx
let protect = Exn.protect

let critical_section = Mutex0.critical_section

let (|!) = Function.(|!)

let ident = Function.ident
let const = Function.const


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
let write_wrap ?(binary = false) ~f fname =
  let oc =
    if binary then
      open_out_bin fname
    else
      open_out fname
  in
  protectx oc ~f ~finally:close_out

let write_lines fname lines =
  write_wrap fname ~f:(fun oc -> Out_channel.output_lines oc lines)

let input_lines = In_channel.input_lines

let read_lines fname = read_wrap fname ~f:input_lines

(** unwraps an option, throwing [Not_found] if it is [None] *)
let uw = function Some x -> x | None -> raise Not_found

(** Operators for picking apart tuples *)

let ss_fst = Space_safe_tuple.T2.get1
let ss_snd = Space_safe_tuple.T2.get2

let ss_fst3 = Space_safe_tuple.T3.get1
let ss_snd3 = Space_safe_tuple.T3.get2
let ss_trd3 = Space_safe_tuple.T3.get3

(** Returns the first element of a triple. *)
let fst3 (x,_,_) = x

(** Returns the second element of a triple. *)
let snd3 (_,y,_) = y

(** Returns the third element of a triple. *)
let trd3 (_,_,z) = z

(** A comparator that returns results in ascending order. *)
external ascending : 'a -> 'a -> int = "%compare"

(** A comparator that returns results in descending order. *)
let descending x y = compare y x

(** Extensible exception printer *)

open Sexplib.Sexp

let failwithf = Core_printf.failwithf
let invalid_argf = Core_printf.invalid_argf
let exitf = Core_printf.exitf

let equal = Caml.(=)

let phys_equal = Caml.(==)
let (==) _ _ = `Consider_using_phys_equal
let (!=) _ _ = `Consider_using_phys_equal

(** Equivalent to Filename.concat *)
let ( ^/ ) = Core_filename.concat

type decimal = float with sexp, bin_io
let sexp_of_decimal x = Sexplib.Sexp.Atom (sprintf "%.12G" x)
let decimal_of_sexp = function
    Sexplib.Sexp.Atom s -> float_of_string s
  | s -> Sexplib.Conv.of_sexp_error
      "decimal_of_sexp: Expected Atom, found List" s

type ('a,'b) result = ('a,'b) Result.t = Ok of 'a | Error of 'b

type 'a bound = Incl of 'a | Excl of 'a | Unbounded
type passfail = Pass | Fail of string

exception Validation_error of string list with sexp
exception Unimplemented of string with sexp
exception Bug of string with sexp
exception Uninitialized_value of string with sexp

let kprintf _ = `Please_use_ksprintf
