(** Timed events

    This implementation uses a priority queue (heap) for efficient adding
    of timed events.  A timer thread keeps waking up at regular tick
    intervals to check for events that need to be executed.  The timer
    can handle more than a million scheduled events.  Timed events can
    also be efficiently removed (in O(log(n))).

*)

(** Type of timers *)
type t

(** Type of events.  Returned when adding event handlers to the timer, and
    needed for removing them again. *)
type event

(** Type of intervals for repeated events *)
type interval =
  | INone  (** No repetition of events *)

  (** Regular repetition of events.  [span] is greater than zero. *)
  | INormal of Time.Span.t

  (** Randomized repetition of events.  [span] is greater than zero.
      The float specifies the maximum ratio with which [span] will be
      multiplied and added to itself, and is in the range \[0.0, 1.0\]. *)
  | IRandom of Time.Span.t * float

val create : ?min_size : int -> ?tick_unit : Time.Span.t -> unit -> t
(** [create ?min_size ?tick_unit ()] creates a new timer.  It will poll
    for new events every [tick_unit] seconds, which must be greater than
    zero.  The minimum size of the heap used by the timer is [min_size].
    @return the timer.

    @raise Invalid_argument if [tick_unit] < 0.0

    @param min_size default = 1000
    @param tick_unit default = 0.1
*)

val deactivate : t -> unit
(** [deactivate timer] deactives a timer.  At most one scheduled event
    will get executed after this function returns. *)

val add :
  t ->
  (event -> Time.t -> unit) ->
  ?randomize : float ->
  ?interval : Time.Span.t ->
  Time.Span.t -> event
(** [add timer handler ?randomize ?interval span] @return a scheduled
    event.  [handler] will be executed [span] seconds at the earliest
    after this function returns, and it gets its associated event as
    argument (useful for letting interval timers remove themselves)
    and the time at which the timer thread woke up.

    NOTE: the [handler] must not allow any exceptions to escape.  [span]
    must be greater than zero.  If the same handler is used in multiple
    timers, then the handler must be reentrant.  The handler must not
    block, and should return as quickly as possible, eventually passing
    off work to other threads if handling an event would take too long.

    An [interval] can be specified to keep rescheduling the event.
    [interval] can be randomized (e.g. for proteanism): the float
    specifies the maximum ratio with which [interval] will be multiplied
    and added to itself, and must be in the range \[0.0, 1.0\].

    @raise Failure if timer is deactivated.
    @raise Invalid_argument if [interval] is a time span <= 0.0.
    @raise Invalid_argument if maximum random ratio not within \[0.0, 1.0\].

    @param randomize default = none
    @param interval default = none
*)

val add_abs :
  t ->
  (event -> Time.t -> unit) ->
  ?randomize : float ->
  ?interval : Time.Span.t ->
  Time.t -> event
(** [add_abs timer handler ?randomize ?interval time] same as {!add}, but
    takes an absolute time [time] for scheduling the event rather than
    a span.  This prevents a time-induced race condition if there is
    a long time between the internal reading of the current time and
    the scheduling of the event, which would artificially delay event
    execution. *)

val remove : event -> unit
(** [remove event] removes [event] from its associated timer.

    NOTE: there is no guarantee that the event will not happen anymore
    if this function returns.  The timer thread may be about to start
    the callback, which leads to an inevitable race condition here.
    Users should be aware of this situation and make sure to handle
    it correctly.

    @raise Failure if timer is deactivated.
*)

val reschedule :
  event ->
  ?randomize : float ->
  ?interval : Time.Span.t ->
  Time.Span.t -> unit
(** [reschedule event ?randomize ?interval span] reschedules [event]
    to start by time span [span] later than originally scheduled, and
    change its interval behaviour as described for {!Timer.add}.

    @raise Failure if timer is deactivated.
    @raise Invalid_argument if [interval] is a time span <= 0.0.
    @raise Invalid_argument if maximum random ratio not within \[0.0, 1.0\].
    @raise Failure if [event] was not already scheduled.
*)

val get_timer : event -> t
(** [get_timer event] @return timer associated with [event]. *)

val get_event_time : event -> Time.t
(** [get_event_time event] @return the time at which [event] is scheduled
    to be run. *)

val get_event_interval : event -> interval
(** [get_event_interval event] @return event interval associated with
    [event]. *)

val is_activated : t -> bool
(** [is_activated timer] @return [true] iff timer is activated. *)
