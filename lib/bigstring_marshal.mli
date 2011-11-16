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

(** Utility functions for marshalling to and from bigstring

    @author Markus Mottl <mmottl\@janestreet.com>
*)
INCLUDE "config.mlh"

IFDEF LINUX_EXT THEN


open Bigstring

(** {6 Marshalling to/from bigstrings} *)

(** [marshal_blit ?flags v ?pos ?len buf] marshals value [v] to bigstring
    [buf] starting at position [pos] and at most [len] bytes.

    @raise Invalid_argument if the designated range is out of bounds.
    @raise Failure if [buf] cannot hold enough data for marshalling.

    @param flags default = []
    @param pos default = 0
    @param len default = [length buf - pos]
*)
val marshal_blit :
  ?flags : Marshal.extern_flags list -> 'a ->
  ?pos : int -> ?len : int -> t -> int

(** [marshal ?flags v] marshals value [v] to a bigstring using marshalling
    flags [flags].  This function may need two times more memory than
    [marshal_blit].

    @param flags default = []
*)
val marshal : ?flags : Marshal.extern_flags list -> 'a -> t

(** [marshal_data_size ?pos buf] @return the length of marshalled data in
    bigstring [buf] starting at position [pos].

    @raise Invalid_argument if the position is out of bounds considering
    a valid marshal header.

    @param pos default = 0
*)
val marshal_data_size : ?pos : int -> t -> int

(** [unmarshal ?pos buf] unmarshals data contained in [buf] starting
    at position [pos].

    @raise Invalid_argument if the position is out of bounds, or if
    there is not enough data for unmarshalling.

    @param pos default = 0
*)
val unmarshal : ?pos : int -> t -> 'a

(** [unmarshal_next ?pos buf] unmarshals data contained in [buf] starting
    at position [pos].  @return [(v, next_pos)], where [v] is the
    unmarshalled value, and [next_pos] designates the start of the byte
    following the unmarshalled data.

    @raise Invalid_argument if the position is out of bounds, or if
    there is not enough data for unmarshalling.

    @param pos default = 0
*)
val unmarshal_next : ?pos : int -> t -> 'a * int

(** [skip ?pos buf] skips the marshalled data starting at position [pos].
    @return the start of the byte following the unmarshalled data.

    @raise Invalid_argument if the position is out of bounds, or if
    there is not enough data for unmarshalling.

    @param pos default = 0
*)
val skip : ?pos : int -> t -> int

(** [marshal_to_sock ?buf sock v] marshals data [v] to socket [sock]
    using marshalling buffer [buf], and marshalling flags [flags].
    Raises input errors as in {!really_send_no_sigpipe_bigstring}.

    @raise Failure if [buf] cannot hold enough data for marshalling.

    @param flags default = []
    @param buf default = determined dynamically
*)
val marshal_to_sock :
  ?buf : t -> ?flags : Marshal.extern_flags list -> Unix.file_descr -> 'a -> unit

(** [unmarshal_from_sock ?buf sock] unmarshals data from socket [sock]
    using unmarshalling buffer [buf].  Raises input errors as in
    {!really_recv_bigstring}.

    @raise Failure if [buf] cannot hold enough data for unmarshalling.

    @param buf default = determined dynamically
*)
val unmarshal_from_sock : ?buf : t -> Unix.file_descr -> 'a
ENDIF
