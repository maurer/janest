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

open Std_internal

(* Represented as a number of seconds since midnight *)
type t = private float

include Binable with type binable = t
include Comparable_binable with type comparable = t
include Floatable with type floatable = t (* seconds since midnight *)
include Hashable_binable with type hashable = t
include Robustly_comparable with type robustly_comparable = t
include Sexpable with type sexpable = t
include Stringable with type stringable = t

val create : ?hr:int -> ?min:int -> ?sec:int -> ?ms:int -> ?us:int -> unit -> t

val to_parts : t -> Span.Parts.t

(* smallest and largest valid ofdays *)
val start_of_day : t
val end_of_day : t

val to_span_since_start_of_day : t -> Span.t
val of_span_since_start_of_day : Span.t -> t

(* Due to a circular reference, this function is defined in Core.Std. *)
(* val now : unit -> t *)

(** [add t s] shifts the time of day [t] by the span [s].  It returns None if
    the result is not in the same day.
*)
val add : t -> Span.t -> t option
val sub : t -> Span.t -> t option

(** [diff t1 t2] returns the difference in time between two ofdays, as if they occurred on
    the same day *)
val diff : t -> t -> Span.t

(** since midnight *)
val to_sec : t -> float
val of_sec : float -> t

val pp : Format.formatter -> t -> unit

(* Returns the time-span separating the two of-days, ignoring the hour information, and
   assuming that the of-days represent times that are within a half-hour of each other.
   This is useful for comparing two ofdays in unknown time-zones. *)
val small_diff : t -> t -> Span.t

(** trailing seconds and subseconds are trimmed off if they are 0 *)
val to_string_trimmed : t -> string

(** trailing milliseconds are trimmed *)
val to_sec_string : t -> string

val of_string_iso8601_extended : ?pos:int -> ?len:int -> string -> t

(** with milliseconds *)
val to_millisec_string : t -> string
