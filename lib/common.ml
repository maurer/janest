(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

open StdLabels
open MoreLabels
open Sexplib.Conv

open Bin_prot
open Bin_prot.Std

let seek_out = `Deprecated_use_out_channel
let pos_out = `Deprecated_use_out_channel
let out_channel_length = `Deprecated_use_out_channel
let seek_in = `Deprecated_use_in_channel
let pos_in = `Deprecated_use_in_channel
let in_channel_length = `Deprecated_use_in_channel
let modf = `Deprecated_use_float_modf
let truncate = `Deprecated_use_float_round_towards_zero

include struct
  open In_channel
  let close_in = close
  let read_lines = read_lines
  let input_lines = input_lines
end

include struct
  open Out_channel
  let close_out = close
  let write_lines = write_lines
end

let read_wrap ?binary ~f _ =
  ignore binary;
  ignore f;
 `Deprecated_use_In_channel_with_file
let write_wrap ?binary ~f _ =
  ignore binary;
  ignore f;
 `Deprecated_use_Out_channel_with_file

(** handy types for marking things read-only and read-write *)
type read_only with bin_io, sexp
type immutable  = private read_only with bin_io, sexp
type read_write = private read_only with bin_io, sexp
type write_only with bin_io, sexp

let sexp_of_immutable _ = failwith "attempt to convert abstract type immutable"
let immutable_of_sexp = sexp_of_immutable
let sexp_of_read_only _ = failwith "attempt to convert abstract type read_only"
let read_only_of_sexp = sexp_of_read_only
let sexp_of_read_write _ = failwith "attempt to convert abstract type read_write"
let read_write_of_sexp = sexp_of_read_write
let sexp_of_write_only _ = failwith "attempt to convert abstract type write_only"
let write_only_of_sexp = sexp_of_write_only

type never_returns
let never_returns (_ : never_returns) = assert false

exception Finally = Exn.Finally

let protectx = Exn.protectx
let protect = Exn.protect

let critical_section = Mutex0.critical_section

let (|!) = Fn.(|!)
let ident = Fn.id
let const = Fn.const
let (==>) a b = (not a) || b

let uw = function Some x -> x | None -> raise Not_found

let is_none = Option.is_none
let is_some = Option.is_some

let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let trd3 (_,_,z) = z

external ascending : 'a -> 'a -> int = "%compare"
let descending x y = compare y x

open Sexplib

let failwithf = Core_printf.failwithf
let invalid_argf = Core_printf.invalid_argf
let exitf = Core_printf.exitf

(* module With_return only exists to avoid circular dependencies *)
include With_return

let equal = Caml.(=)

let phys_equal = Caml.(==)
let (==) _ _ = `Consider_using_phys_equal
let (!=) _ _ = `Consider_using_phys_equal

let force = Lazy.force

let ( ^/ ) = Core_filename.concat


type decimal = float with bin_io
let sexp_of_decimal x = Sexp.Atom (Core_printf.sprintf "%.12G" x)
let decimal_of_sexp = function
  | Sexp.Atom s ->
    let result = Float.of_string s in
    begin match Pervasives.classify_float result with
    | FP_normal
    | FP_subnormal
    | FP_zero ->
      result
    | FP_infinite
    | FP_nan ->
      Conv.of_sexp_error "decimal_of_sexp: nan or inf" (Sexp.Atom s)
    end
  | s ->
    Conv.of_sexp_error "decimal_of_sexp: Expected Atom, found List" s

type 'a bound = Incl of 'a | Excl of 'a | Unbounded

type passfail = Pass | Fail of string

exception Validation_error of string list with sexp
exception Unimplemented of string with sexp
exception Bug of string with sexp
