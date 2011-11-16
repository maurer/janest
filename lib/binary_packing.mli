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

(* Packs and unpacks various types of integers into and from strings.

   Functions ending in _int should not be used on 32 bit programs because native ocaml
   ints will not be big enough.

   [pos] arguments refer to the location in the buf string.

   We support big and little endian ints.  Note that for an 8 bit (1 byte) integer, there
   is no difference because endian-ness only changes the order of bytes, not bits.
*)

type endian = [ `Big_endian | `Little_endian ]

val unpack_signed_8      :                      buf:string -> pos:int -> int
val   pack_signed_8      :                      buf:string -> pos:int -> int -> unit

val unpack_unsigned_8    :                      buf:string -> pos:int -> int
val   pack_unsigned_8    :                      buf:string -> pos:int -> int -> unit

val unpack_signed_16     : byte_order:endian -> buf:string -> pos:int -> int
val   pack_signed_16     : byte_order:endian -> buf:string -> pos:int -> int -> unit

val unpack_signed_32     : byte_order:endian -> buf:string -> pos:int -> int32
val unpack_signed_32_int : byte_order:endian -> buf:string -> pos:int -> int
val   pack_signed_32     : byte_order:endian -> buf:string -> pos:int -> Int32.t -> unit
val   pack_signed_32_int : byte_order:endian -> buf:string -> pos:int -> int -> unit

val unpack_signed_64     : byte_order:endian -> buf:string -> pos:int -> int64
val unpack_signed_64_int : byte_order:endian -> buf:string -> pos:int -> int
val   pack_signed_64     : byte_order:endian -> buf:string -> pos:int -> Int64.t -> unit
val   pack_signed_64_int : byte_order:endian -> buf:string -> pos:int -> int -> unit


val unpack_float         : byte_order:endian -> buf:string -> pos:int -> float
val pack_float           : byte_order:endian -> buf:string -> pos:int -> float -> unit

val test : unit -> unit
