(** Threading model:

    Only one thread is running Async code at a time.  This is enforced by a single lock in
    Async's scheduler data structure.  There are any number of threads running code
    without holding the lock that get data from the outside world and want to affect the
    Async world.  They do this by calling [Thread_safe.run_in_async*], which acquires the
    lock, does a computation (e.g. fills an ivar), and then runs a "cycle" of Async
    computations. *)

open Core.Std
open Import

type t = Raw_scheduler.t with sexp_of

(** [t ()] returns the Async scheduler.  If the scheduler hasn't been created yet, this
    will create it and acquire the Async lock. *)
val t : unit -> t

(** [go ?raise_unhandled_exn ()] passes control to Async, at which point Async starts
    running handlers, one by one without interruption, until there are no more handlers to
    run.  When Async is out of handlers it blocks until the outside world schedules more
    of them.  Because of this, Async programs do not exit until [shutdown] is called.

    [go ()] calls [handle_signal Sys.sigpipe], which causes the SIGPIPE signal to be
    ignored.  Low-level syscalls (e.g. write) still raise EPIPE.

    If any Async job raises an unhandled exception that is not handled by any monitor,
    Async execution ceases.  Then, by default, Async pretty prints the exception, and
    exits with status 1.  If you don't want this, pass [~raise_unhandled_exn:true], which
    will cause the unhandled exception to be raised to the caller of [go ()]. *)
val go
  :  ?raise_unhandled_exn:bool (** default is [false] *)
  -> unit
  -> never_returns

(** [go_main] is like [go], except that one supplies a [main] function that will be run to
    initialize the Async computation, and that [go_main] will fail if any Async has been
    used prior to [go_main] being called.  Moreover it allows to configure more static
    options of the scheduler. *)
val go_main
  :  ?raise_unhandled_exn:bool (** default is [false] *)
  -> ?file_descr_watcher:Config.File_descr_watcher.t (** default to [Config] *)
  -> ?max_num_open_file_descrs:int                   (** default to [Config] *)
  -> ?max_num_threads:int                            (** default to [Config] *)
  -> main:(unit -> unit)
  -> unit
  -> never_returns

type 'a with_options =
  ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> 'a

(* val current_execution_context : unit -> Execution_context.t *)

(** [within_context context f] runs [f ()] right now with the specified execution
    context.  If [f] raises, then the exception is sent to the monitor of [context], and
    [Error ()] is returned. *)
val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t

(** [within' f ~monitor ~priority] runs [f ()] right now, with the specified
    block group, monitor, and priority set as specified.  They will be reset to their
    original values when [f] returns.  If [f] raises, then the result of [within'] will
    never become determined, but the exception will end up in the specified monitor. *)
val within' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options

(** [within] is like [within'], but doesn't require thunk to return a deferred. *)
val within : ((unit -> unit) -> unit) with_options

(** [within_v] is like [within], but allows a value to be returned by [f]. *)
val within_v : ((unit -> 'a) -> 'a option) with_options

(** [with_local key value ~f], when run in the current execution context, [e], runs [f]
    right now in a new execution context, [e'], that is identical to [e], except that
    [find_local key = value].  As usual, [e'] will be in effect in asynchronous
    computations started by [f].  When [with_local] returns, the execution context is
    restored to [e]. *)
val with_local : 'a Univ_map.Key.t -> 'a option -> f:(unit -> 'b) -> 'b

(** [find_local key] returns the value associated to [key] in the current execution
    context. *)
val find_local : 'a Univ_map.Key.t -> 'a option

(** Just like [within'], but instead of running thunk right now, adds
    it to the Async queue to be run with other Async jobs. *)
val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options

(** Just like schedule', but doesn't require thunk to return a deferred. *)
val schedule : ((unit -> unit) -> unit) with_options

(** [preserve_execution_context t f] saves the current execution context and returns a
    function [g] such that [g a] runs [f a] in the saved execution context.  [g a] becomes
    determined when [f a] becomes determined. *)
val preserve_execution_context  : ('a -> unit)          -> ('a -> unit)          Staged.t
val preserve_execution_context' : ('a -> 'b Deferred.t) -> ('a -> 'b Deferred.t) Staged.t

(** [cycle_start ()] returns the result of [Time.now ()] called at the beginning of
    cycle. *)
val cycle_start : unit -> Time.t

(** [cycle_times ()] returns a stream that will have one element for each cycle that Async
    runs, with the amount of time that the cycle took (as determined by calls to Time.now
    at the beginning and end of the cycle). *)
val cycle_times : unit -> Time.Span.t Stream.t

(** [report_long_cycle_times ?cutoff ()] sets up something that will print a warning to
    stderr whenever there is an Async cycle that is too long, as specified by [cutoff],
    whose default is 1s. *)
val report_long_cycle_times : ?cutoff:Time.Span.t -> unit -> unit

(** [cycle_count ()] returns the total number of Async cycles that have happened. *)
val cycle_count : unit -> int

(** [force_current_cycle_to_end ()] causes no more normal priority jobs to run in the
    current cycle, and for the end-of-cycle jobs (i.e. writes) to run, and then for the
    cycle to end. *)
val force_current_cycle_to_end : unit -> unit

(** [is_running ()] returns true if the scheduler has been started. *)
val is_running : unit -> bool

(** [set_max_num_jobs_per_priority_per_cycle int] sets the maximum number of jobs that
    will be done at each priority within each Async cycle.  The default is [500]. *)
val set_max_num_jobs_per_priority_per_cycle : int -> unit

(** [set_max_inter_cycle_timeout span] sets the maximum amount of time the scheduler will
    remain blocked (on epoll or select) between cycles. *)
val set_max_inter_cycle_timeout : Time.Span.t -> unit

(** [set_check_invariants do_check] sets whether Async should check invariants of its
    internal data structures.  [set_check_invariants true] can substantially slow down
    your program. *)
val set_check_invariants : bool -> unit

(** [set_detect_invalid_access_from_thread do_check] sets whether Async routines should
    check if they are being accessed from some thread other than the thread currently
    holding the Async lock, which is not allowed and can lead to very confusing
    behavior. *)
val set_detect_invalid_access_from_thread : bool -> unit

(** [set_record_backtraces do_record] sets whether Async should keep in the execution
    context the history of stack backtraces (obtained via [Backtrace.get]) that led to the
    current job.  If an Async job has an unhandled exception, this backtrace history will
    be recorded in the exception.  In particular the history will appear in an unhandled
    exception that reaches the main monitor.  This can have a substantial performance
    impact, both in running time and space usage. *)
val set_record_backtraces : bool -> unit

(** [fold_fields ~init folder] folds [folder] over each field in the scheduler.  The
    fields themselves are not exposed -- [folder] must be a polymorphic function that
    can work on any field.  So, it's only useful for generic operations, e.g. getting
    the size of each field. *)
type 'b folder = { folder : 'a. 'b -> t -> (t, 'a) Field.t -> 'b }
val fold_fields : init:'b -> 'b folder -> 'b

val is_ready_to_initialize : unit -> bool

(** If a process that has already created, but not started, the Async scheduler would like
    to fork, and would like the child to have a clean Async, i.e. not inherit any of the
    Async work that was done in the parent, it can call [reset_in_forked_process] at the
    start of execution in the child process.  After that, the child can do Async stuff and
    then start the Async scheduler. *)
val reset_in_forked_process : unit -> unit

(** Async supports "busy polling", which runs a thread that busy loops running
    user-supplied polling functions.  The busy-loop thread is distinct from Async's
    scheduler thread.

    Busy polling is useful for a situation like a shared-memory ringbuffer being used for
    IPC.  One can poll the ringbuffer with a busy poller, and then when data is detected,
    fill some ivar that causes Async code to handle the data.

    [add_busy_poller poll] adds [poll] to the busy loop.  [poll] will be called
    continuously, once per iteration of the busy loop, until it returns [`Stop_polling a]
    at which point the result of [add_busy_poller] will become determined.  [poll] will
    hold the Async lock while running, so it is fine to do ordinary Async operations,
    e.g. fill an ivar.  The busy loop will run an ordinary Async cycle if any of the
    pollers add jobs.

    [poll] will run in monitor in effect when [add_busy_poller] was called; exceptions
    raised by [poll] will be sent asynchronously to that monitor.  If [poll] raises, it
    will still be run on subsequent iterations of the busy loop. *)
val add_busy_poller
  :  (unit -> [ `Continue_polling | `Stop_polling of 'a ])
  -> 'a Deferred.t

(** [handle_thread_pool_stuck f] causes [f] to run whenever Async detects its thread pool
    is stuck (i.e. hasn't completed a job for over a second and has work waiting to
    start).  Async checks every second.  By default, if thread pool has been stuck for
    less than 60s, Async will [eprintf] a message.  If more than 60s, Async will send an
    exception to the main monitor, which will abort the program unless there is a custom
    handler for the main monitor.

    Calling [handle_thread_pool_stuck] replaces whatever behavior was previously there. *)
val handle_thread_pool_stuck : (stuck_for:Time.Span.t -> unit) -> unit

(** [yield ()] returns a deferred that becomes determined after the current cycle
    completes.  This can be useful to improve fairness by [yield]ing within a computation
    to give other jobs a chance to run. *)
val yield : unit -> unit Deferred.t

(** [yield_every ~n] returns a function that will act as [yield] every [n] calls and as
    [Deferred.unit] the rest of the time.  This is useful for improving fairness in
    circumstances where you don't have good control of the batch size, but can insert a
    deferred into every iteration.

    [yield_every] raises if [n <= 0]. *)
val yield_every : n:int -> (unit -> unit Deferred.t) Staged.t
