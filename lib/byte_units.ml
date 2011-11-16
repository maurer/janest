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

(* Conversions between units of measure based on bytes. *)

open Sexplib.Std
open Bin_prot.Std
open Std_internal

let bytes_per_word =
  let module W = Word_size in
  match W.word_size with
    | W.W32 -> 4.
    | W.W64 -> 8.

let kbyte = 1024.
let mbyte = kbyte *. kbyte
let gbyte = kbyte *. mbyte

(* External.t - used just for custom sexp convertors *)
module External = struct
  type t =
    [
    | `Bytes of float
    | `Kilobytes of float
    | `Megabytes of float
    | `Gigabytes of float
    | `Words of float
    ]
  with bin_io, sexp
end

module Measure = struct

  type t = [ `Bytes | `Kilobytes | `Megabytes | `Gigabytes | `Words ]
  with bin_io

  let bytes = function
    | `Bytes -> 1.
    | `Kilobytes -> kbyte
    | `Megabytes -> mbyte
    | `Gigabytes -> gbyte
    | `Words -> bytes_per_word

end

module T = struct

  type t = {
    preferred_measure : Measure.t; (* for printing/externalizing *)
    bytes : float;
  } with bin_io

  let number_of_preferred_measures t = t.bytes /. Measure.bytes t.preferred_measure

  let create m n = {
    preferred_measure = m;
    bytes = (n *. Measure.bytes m);
  }

  let externalize t =
    let n = number_of_preferred_measures t in
    match t.preferred_measure with
      | `Bytes      -> `Bytes n
      | `Kilobytes  -> `Kilobytes n
      | `Megabytes  -> `Megabytes n
      | `Gigabytes  -> `Gigabytes n
      | `Words      -> `Words n

  let internalize t =
    match t with
      | `Bytes     n -> create `Bytes n
      | `Kilobytes n -> create `Kilobytes n
      | `Megabytes n -> create `Megabytes n
      | `Gigabytes n -> create `Gigabytes n
      | `Words     n -> create `Words n

  let t_of_sexp sexp = internalize (External.t_of_sexp sexp)
  let sexp_of_t t = External.sexp_of_t (externalize t)

  let bytes t = t.bytes

  type sexpable = t
  type stringable = t
  type binable = t

  let of_string s =
    let length = String.length s in
    if length < 2 then
      invalid_argf "'%s' passed to Byte_units.of_string - too short" s ();
    let base =
      try
        Float.of_string (String.sub s ~pos:0 ~len:(length - 1))
      with
      | _ ->
        invalid_argf "'%s' passed to Byte_units.of_string - first part cannot be \
          converted to float " s ()
    in
    match Char.lowercase s.[length - 1] with
    | 'b' -> create `Bytes base
    | 'k' -> create `Kilobytes base
    | 'm' -> create `Megabytes base
    | 'g' -> create `Gigabytes base
    | 'w' -> create `Words base
    | _   -> invalid_argf "'%s' passed to Byte_units.of_string - illegal extension" s ()

  let to_string t =
    let fmt e = sprintf "%g%c" (number_of_preferred_measures t) e in
    match t.preferred_measure with
    | `Bytes -> fmt 'b'
    | `Kilobytes -> fmt 'k'
    | `Megabytes -> fmt 'm'
    | `Gigabytes -> fmt 'g'
    | `Words -> fmt 'w'

  let kilobytes t = bytes t /. kbyte
  let megabytes t = bytes t /. mbyte
  let gigabytes t = bytes t /. gbyte
  let words t = bytes t /. bytes_per_word

  let compare t1 t2 = Float.compare (bytes t1) (bytes t2)

  let equal t1 t2 = bytes t1 = bytes t2
  let hash = Hashtbl.hash
end

include T
include Comparable.Make(T)
include Hashable.Make(T)
