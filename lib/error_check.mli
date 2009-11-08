open Sexplib
open Std_internal

(** Module for 'toggle' error checking.  This simply means
    multiple failures in a row do not call fail_alert multiple
    times.  success_alert is called upon return from a failed
    state to a successful one. *)
module Toggle : sig
  type (-'param, +'alert) t
  val sexp_of_t : _ -> _ -> ('param, 'alert) t -> Sexp.t
  val create : assertion:('param -> bool)
            -> fail_alert:('param -> 'alert)
            -> success_alert:('param -> 'alert)
                -> ('param, 'alert) t
  val check : ('param, 'alert) t -> 'param -> 'alert option
  val state : ('param, 'alert) t -> bool
end

(** ToggleN error checking is the same as Toggle, except
  here you may specify a maximum number of failures that will
  be reported.  On the (n+1)th alert, final_fail_alert will be used to
  generate the alert instead of fail_alert.  *)
module ToggleN : sig
  type (-'param, +'alert) t
  val sexp_of_t : _ -> _ -> ('param, 'alert) t -> Sexp.t
  val create : assertion:('param -> bool)
            -> fail_alert:('param -> 'alert)
            -> final_fail_alert:('param -> 'alert)
            -> success_alert:('param -> 'alert)
            -> max_consecutive_fail_alerts:int
                -> ('param, 'alert) t
  val check : ('param, 'alert) t -> 'param -> 'alert option
  val state : ('param, 'alert) t -> bool
end

(** With the Timer error checker, the same failure alert will be repeated
  but only after min_alert_interval has passed. *)

module Timer : sig
  type (-'param, +'alert) t
  val sexp_of_t : _ -> _ -> ('param, 'alert) t -> Sexp.t
  val create : assertion:('param -> bool)
            -> fail_alert:('param -> 'alert)
            -> success_alert:('param -> 'alert)
            -> min_alert_interval:Time.Span.t
                -> ('param, 'alert) t
  val check : ('param, 'alert) t -> 'param -> Time.t -> 'alert option
  val state : ('param, 'alert) t -> bool
end

(** Module for 'step' error checking with a moving threshold.
  Every time the threshold is breached, fail_alert is called
  and the threshold is subsequently adjusted (usually loosened).
  success_alert is called on return from a failed state to a successful
  one, but _only_ when successful with respect to the _original_ threshold
  (not the adjusted one).  The adjustment function may tighten the
  threshold or return it to the original value (or loosen it) without
  a problem. *)
module Step : sig
  type ('threshold, -'param, +'alert) t
  val sexp_of_t : _ -> _ -> _ -> ('threshold, 'param, 'alert) t -> Sexp.t
  val create : threshold:'threshold
            -> adjust:('param -> threshold:'threshold -> 'threshold)
            -> assertion:('param -> threshold:'threshold -> bool)
            -> fail_alert:('param -> 'alert)
            -> success_alert:('param -> 'alert)
                -> ('threshold, 'param, 'alert) t
  val check : ('threshold, 'param, 'alert) t -> 'param -> 'alert option
  val state : ('threshold, 'param, 'alert) t -> bool
end


(** Module for reporting every occurrence of the failure state but only the first time
    the success state is entered. *)

module ReportAllFails : sig
  type (-'param, +'alert) t
  val sexp_of_t : _ -> _ -> ('param, 'alert) t -> Sexp.t
  val create : assertion:('param -> bool)
            -> fail_alert:('param -> 'alert)
            -> success_alert:('param -> 'alert)
                -> ('param, 'alert) t
  val check : ('param, 'alert) t -> 'param -> 'alert option
  val state : ('param, 'alert) t -> bool
end
