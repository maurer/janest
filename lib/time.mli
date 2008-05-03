(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(** Our time module.  This module wraps up unix times, including various
    convenience functions for accessing them.
*)

open Common
open Std_internal

(** A timespan. *)
module Span : sig
  type t

  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Comparable with type comparable = t
  include Robustly_comparable with type robustly_comparable = t
  include Floatable with type floatable = t

  (* String converters and sexp converters allow for specifying of time spans in various
     units.  An unadorned float is interpreted as being in seconds.  Other formats are
     achieved by appending a string to the end indicating the unit, e.g. 12ms for 12
     milliseconds or 5d for 5 days.  The endings are as follows:

     ms - milliseconds
     s - seconds
     m - minutes
     h - hours
     The outgoing conversion functions use these units as well, choosing the largest
     available type.  I.e., if it's a bit greater than or equal to 1 day, the span will be
     rendered as the string "1d".
  *)
  val to_string : t -> string
  val of_string : string -> t

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
  val add : t -> t -> t (* CRv2 sweeks: rename as + *)
  val sub : t -> t -> t (* CRv2 sweeks: rename as - *)
  val abs : t -> t (** computes absolute value of span *)
  val scale : float -> t -> t

  val pp : Format.formatter -> t -> unit
end

(** A time of day. *)
(* CR cfalls: Ofdays should always have a timezone! *)
module Ofday : sig
  type t

  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Comparable with type comparable = t
  include Robustly_comparable with type robustly_comparable = t
  include Stringable with type stringable = t
  include Floatable with type floatable = t

  val create : ?hr:int -> ?min:int -> ?sec:int -> ?ms:int -> unit -> t

  (* CR sweeks: It seems like we should have some invariant that Ofday.t is
     uniquely represented between midnight at 23:59:59.999.  If so, it seems
     weird that min_value and max_value are not in that range.
  *)
  val min_value : t
  val max_value : t

  val to_span_since_midnight : t -> Span.t
  val of_span_since_midnight : Span.t -> t

  val start_of_day : t

  (** [add t s] shifts the time of day [t] by the span [s].  It returns None if
      the result is not in the same day.
  *)
  val add : t -> Span.t -> t option
  val sub : t -> Span.t -> t option
    
  (** since midnight *)
  val to_sec : t -> float
  val of_sec : float -> t

  (* we have two times which try to represent the same moment
     but may be off a little bit (<30min) and which may use different timezone
     we need to figure out how much they differ *)
  val small_diff : t -> t -> Span.t

  val pp : Format.formatter -> t -> unit

  (** [to_string_trimmed t] return a string with trailing seconds and subseconds
    trimmed off if they are 0 *)
  val to_string_trimmed : t -> string

  val of_string_iso8601_extended : ?pos:int -> ?len:int -> string -> t
end

(** {6 String conversions} *)

module Date : sig
  type t = private { y: int; m: Month.t; d: int } with sexp, bin_io

  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Hashable with type hashable = t
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
    
  val to_string_iso8601_extended : t -> string
  val to_string_iso8601_basic : t -> string

  val min_value : t
  val max_value : t

  val pp : Format.formatter -> t -> unit
  val day : t -> int
  val month : t -> Month.t
  val year : t -> int

  val today : unit -> t

  val is_weekday : t -> bool

  val add_days : t -> int -> t

  (** [add_weekdays t n] returns t when n=0 even if t is not a weekday *)
  val add_weekdays : t -> int -> t

  val dates_between : min:t -> max:t -> t list
end

(** A date+time. *)
type t

include Comparable with type comparable = t
include Robustly_comparable with type robustly_comparable = t
include Sexpable with type sexpable = t
include Binable with type binable = t
include Stringable with type stringable = t
include Floatable with type floatable = t

(** {6 Basic operations on times} *)

val add : t -> Span.t -> t
val sub : t -> Span.t -> t
val diff : t -> t -> Span.t
val abs_diff : t -> t -> Span.t

(** {6 Constants} *)
val min_value : t
val max_value : t

(** {6 Conversions} *)

val of_date_ofday : Date.t -> Ofday.t -> t
val to_date_ofday : t -> Date.t * Ofday.t

val to_date : t -> Date.t
val to_ofday : t -> Ofday.t

(** Other string conversions  *)
(* CRv2 OG: comments *)
val to_filename_string : t -> string
val of_filename_string : string -> t

(* CRv2 sweeks: shouldn't these fix functions be in the fix library? *)
val to_string_fix_proto : t -> string
val of_string_fix_proto : string -> t
  
val to_string_old : t -> string  (* MM/DD/YYYY HH:MM:SS.MS *)

(** [to_string_trimmed t] Same as to_string, but removes trailing seconds and
  milliseconds if they are 0 *)
val to_string_trimmed : t -> string
val of_date_time_strings : string -> string -> t

val pp : Format.formatter -> t -> unit

(** {6 Miscellaneous} *)

val now : unit -> t

val pause : Span.t -> unit

(** [ ofday_occurrence ofday side now ] returns a Time.t that is the occurrence of ofday
   which is the latest occurrence before now or the earliest occurrence after now,
   according to side.
   NOTE: This function is a little bit wrong near daylight savings time *)
val ofday_occurrence : Ofday.t -> [ `right_after | `right_before ] -> t -> t

