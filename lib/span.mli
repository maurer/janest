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

type t = private float                  (* number of seconds *)

(* Parts represents the individual parts of a Span as if it were written out (it is the
   counterpart to create).  For example, (Span.of_sec 90.) is represented by {Parts.hr = 0;
   min = 1; sec = 30; ms = 0}.  The fields will always be positive. *)
module Parts : sig
  type t = private {
      sign : Float.Sign.t;
      hr   : int;
      min  : int;
      sec  : int;
      ms   : int;
      us   : int;
    }
    with sexp
end

include Binable with type binable = t
include Comparable_binable with type comparable = t
include Floatable with type floatable = t (* in units of seconds *)
include Hashable_binable with type hashable = t
include Robustly_comparable with type robustly_comparable = t
include Sexpable with type sexpable = t

(* String converters and sexp converters allow for specifying of time spans in various
    units.  An unadorned float is interpreted as being in seconds.  Other formats are
    achieved by appending a string to the end indicating the unit, e.g. 12ms for 12
    milliseconds 5.1h for 5.1 hours.  The endings are as follows:

    ms - milliseconds
    s - seconds
    m - minutes
    h - hours
    The outgoing conversion functions use these units as well, choosing the largest
    available type.  I.e., if it's a bit greater than or equal to 1 day, the span will be
    rendered in days, e.g., Time.to_string (Time.of_string "66m") = "1.1h".
*)
val to_string : t -> string
val of_string : string -> t

(* values *)
val nanosecond : t
val microsecond : t
val millisecond : t
val second : t
val minute : t
val hour : t
val day : t
(* 10^-6 seconds, used in robustly comparable operators (<., >., =., ...) to determine
   equality *)
val epsilon : t
val zero : t


val create :
  ?day:int
  -> ?hr:int
  -> ?min:int
  -> ?sec:int
  -> ?ms:int
  -> ?us:int
  -> unit
  -> t

val to_parts : t -> Parts.t

(* converters *)
val of_ns      : float -> t
val of_us      : float -> t
val of_ms      : float -> t
val of_sec     : float -> t
val of_int_sec : int   -> t
val of_min     : float -> t
val of_hr      : float -> t
val of_day     : float -> t

val to_ns  : t -> float
val to_us  : t -> float
val to_ms  : t -> float
val to_sec : t -> float
val to_min : t -> float
val to_hr  : t -> float
val to_day : t -> float

(** {6 Basic operations on spans} *)
val add   : t -> t -> t 
val sub   : t -> t -> t 
val abs   : t -> t (** absolute value *)
val scale : t -> float -> t
val (/)   : t -> float -> t
val (//)  : t -> t -> float




(** [randomize t ~percent] returns a random span between [t - percent * t]
    and [t + percent * t] *)
val randomize : t -> percent:float -> t
val pp : Format.formatter -> t -> unit
