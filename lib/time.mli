(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(** Our time module.  This module wraps up unix times, including various
    convenience functions for accessing them.
*)

open Common
open Std_internal

(** A timespan. *)
module Span : sig
  (* Parts represents the individual parts of a Span as if it were written out (it is
  the counterpart to create).  For example, (Span.of_sec 90.) is represented by
  {Parts.hr = 0; min = 1; sec = 30; ms = 0} *)
  module Parts : sig
    type t = {
        hr: int;
        min: int;
        sec: int;
        ms: int
      }
  end

  type t

  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Comparable with type comparable = t
  include Robustly_comparable with type robustly_comparable = t
  include Floatable with type floatable = t

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
  (* Time spans can also be converted to strings of the form "H:MM:SS.mmm" for greater
     readability. Note that Span.of_string cannot convert these strings back to time
     spans.
  *)
  val to_string_hum : t -> string

  (* values *)
  val min_value : t
  val max_value : t
  val second : t
  val minute : t
  val hour : t
  val day : t
  val epsilon : t
  val zero : t

  val create :
    ?day:int -> ?hr:int -> ?min:int -> ?sec:int -> ?ms:int -> unit -> t

  val to_parts : t -> Parts.t

  (* converters *)
  val of_ms : float -> t
  val of_sec : float -> t
  val of_int_sec : int -> t
  val of_min : float -> t
  val of_hr : float -> t
  val of_day : float -> t

  val to_ms : t -> float
  val to_sec : t -> float
  val to_min : t -> float
  val to_hr : t -> float
  val to_day : t -> float

  (** {6 Basic operations on spans} *)
  val add : t -> t -> t 
  val sub : t -> t -> t 
  val abs : t -> t (** computes absolute value of span *)
  
  val scale : float -> t -> t
  val (/) : t -> t -> float
  

  (** [randomize t ~percent] returns a random span between [t - percent * t]
      and [t + percent * t] *)
  val randomize : t -> percent:float -> t
  val pp : Format.formatter -> t -> unit
end

(** A time of day. *)

module Ofday : sig
  type t

  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Comparable with type comparable = t
  include Robustly_comparable with type robustly_comparable = t
  include Stringable with type stringable = t
  include Floatable with type floatable = t

  val create : ?hr:int -> ?min:int -> ?sec:int -> ?ms:int -> unit -> t

  val to_parts : t -> Span.Parts.t

  
  val min_value : t
  val max_value : t

  val to_span_since_midnight : t -> Span.t
  val of_span_since_midnight : Span.t -> t

  val start_of_day : t
  val end_of_day : t

  

  (** [add t s] shifts the time of day [t] by the span [s].  It returns None if
      the result is not in the same day.
  *)
  val add : t -> Span.t -> t option
  val sub : t -> Span.t -> t option

  (** since midnight *)
  val to_sec : t -> float
  val of_sec : float -> t

  (* We have two times which try to represent the same moment
     but may be off a little bit (<30min) and which may use different timezone
     we need to figure out how much they differ.

     If one of the [t] is [min_value] or [max_value], the result is [None]
     to avoid the [nan] problem. Be careful [None] does not mean "no diff".
  *)
  val small_diff : t -> t -> Span.t option

  val pp : Format.formatter -> t -> unit

  (** [to_string_trimmed t] return a string with trailing seconds and subseconds
    trimmed off if they are 0 *)
  val to_string_trimmed : t -> string

  (** [to_sec_string t] returns a string with trailing milliseconds trimmed*)
  val to_sec_string : t -> string

  val of_string_iso8601_extended : ?pos:int -> ?len:int -> string -> t
end

(** {6 String conversions} *)

module Date : sig
  type t = private { y: int; m: Month.t; d: int } with sexp, bin_io

  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Hashable_binable with type hashable = t
  (** converts a string to a date, in formats:
   * m/d/y
   * y-m-d (* valid iso8601_extended *)
   * DD MMM YYYY
   * DDMMMYYYY
   * YYYYMMDD *)
  include Stringable with type stringable = t
  include Comparable with type comparable = t

  (** [sexp_of_t_style] controls how [sexp_of_t] works
       sexp_ot_t_style    sexp
       ----------------   -----------------------
       `List_ymd          ((y 2008) (m 4) (d 29))
       `Atom_yyyy_mm_dd   2008-04-29
  *)
  val sexp_of_t_style : [ `List_ymd | `Atom_yyyy_mm_dd ] ref

  val create : y:int -> m:Month.t -> d:int -> t

  val of_tm : Core_unix.tm -> t

  (* For details on this ISO format, see:

      http://www.wikipedia.org/wiki/iso8601
  *)
  val to_string_iso8601_extended : t -> string (* YYYY-MM-DD *)
  val to_string_iso8601_basic : t -> string    (* YYYYMMDD *)
  val to_string_old : t -> string              (* MM/DD/YYYY *)

  val min_value : t
  val max_value : t

  val pp : Format.formatter -> t -> unit
  val day : t -> int
  val month : t -> Month.t
  val year : t -> int

  val today : unit -> t

  val day_of_week : t -> Weekday.t

  val is_weekday : t -> bool

  val is_business_day : t -> is_holiday:(t -> bool) -> bool

  val add_days : t -> int -> t

  (** [diff t1 t2] returns date [t1] minus date [t2] in days. *)
  val diff : t -> t -> int

  (** [add_weekdays t n] returns t when n=0 even if t is not a weekday *)
  val add_weekdays : t -> int -> t

  (** [add_business_days t ~is_holiday n] returns t when n=0 even if t is not
      a business day. [add_business_days ~is_holiday:(fun _ -> false) ...] is the
      same as [add_weekdays]. *)
  val add_business_days : t -> is_holiday:(t -> bool) -> int -> t

  (* the following returns a closed interval (endpoints included) *)
  val dates_between : min:t -> max:t -> t list

  val business_dates_between : min:t -> max:t -> is_holiday:(t -> bool) -> t list

  val weekdays_between : min:t -> max:t -> t list

  val previous_weekday : t -> t
end

(** A date+time. *)
type t

include Hashable with type hashable = t
include Comparable with type comparable = t
include Robustly_comparable with type robustly_comparable = t
include Sexpable with type sexpable = t
include Binable with type binable = t
include Stringable with type stringable = t
include Floatable with type floatable = t

(** {6 Basic operations on times} *)

(** [add t s] adds the span [s] to time [t] and returns the resulting time.

    NOTE: adding spans as a means of adding days is not accurate, and may run into trouble
    due to shifts in daylight savings time, float arithmetic issues, and leap seconds.
    See the comment at the top of TZ.mli for a more complete discussion of some of the
    issues of time-keeping.  For spans that cross date boundaries, use date functions
    instead.
*)
val add : t -> Span.t -> t

(** [sub t s] subtracts the span [s] from time [t] and returns the
    resulting time.  See important note for [add]. *)
val sub : t -> Span.t -> t

(** [diff t1 t2] returns time [t1] minus time [t2]. *)
val diff : t -> t -> Span.t

(** [diff t1 t2] returns the absolute span of time [t1] minus time [t2]. *)
val abs_diff : t -> t -> Span.t

(** {6 Constants} *)

(** Minimum value. *)
val min_value : t

(** Maximum value. *)
val max_value : t

(** {6 Conversions} *)
(** All these conversion functions use the current time zone. Unless marked _utc,
    in which case they use Universal Coordinated Time *)

val of_date_ofday : Date.t -> Ofday.t -> t
val to_date_ofday : t -> Date.t * Ofday.t

(** Assume the specified date and time of day are UTC *)
val of_date_ofday_utc : Date.t -> Ofday.t -> t

(** Produce the current UTC date and time of day *)
val to_date_ofday_utc : t -> Date.t * Ofday.t

val to_date : t -> Date.t
val to_ofday : t -> Ofday.t

(** Other string conversions  *)
(** [to_filename_string t] converts [t] to string with format YYYY-MM-DD_HH-MM-SS.mmm
    which is suitable for using in filenames *)
val to_filename_string : t -> string
(** [of_filename_string s] converts [s] that has format YYYY-MM-DD_HH-MM-SS.mmm into time *)
val of_filename_string : string -> t


val to_string_fix_proto : [`Utc | `Local] -> t -> string
val of_string_fix_proto : [`Utc | `Local] -> string -> t

val to_string_old : t -> string  (* MM/DD/YYYY HH:MM:SS.MS *)

(** [to_string_trimmed t] Same as to_string, but removes trailing seconds and
  milliseconds if they are 0 *)
val to_string_trimmed : t -> string
val of_date_time_strings : string -> string -> t

val pp : Format.formatter -> t -> unit

(** {6 Miscellaneous} *)

(** @return the current time. *)
val now : unit -> t

(** Pause (and don't throw an exception) *)
val pause : Span.t -> unit

(** [ ofday_occurrence ofday side now ] returns a Time.t that is the occurrence of ofday
   which is the latest occurrence before now or the earliest occurrence after now,
   according to side.
   NOTE: This function is a little bit wrong near daylight savings time *)
val ofday_occurrence : Ofday.t -> [ `right_after | `right_before ] -> t -> t

val ofday_occurrence_utc : Ofday.t -> [ `right_after | `right_before ] -> t -> t
