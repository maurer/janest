(*pp $(pwd)/pp.sh *)
(*
#include <unistd.h>
end-pp-include*)

(** Interface to additional Unix-system calls *)
open Unix

(** {2 Utility functions} *)

external unix_error : int -> string -> string -> 'a = "unix_error_stub"
(** @raise [Unix_error] with a given errno, function name and argument *)

(** {2 Filesystem functions} *)

val mknod :
  ?file_kind : file_kind ->
  ?perm : int ->
  ?major : int ->
  ?minor : int ->
  string -> unit


(** {2 I/O vectors} *)

(** I/O-vectors for scatter/gather-operations *)
module IOVec : sig
  open Bigarray

  (** Representation of I/O-vectors.
      NOTE: DO NOT CHANGE THE MEMORY LAYOUT OF THIS TYPE!!! *)
  type 'buf t = private
    {
      buf : 'buf;  (** Buffer holding the I/O-vector *)
      pos : int;  (** Position of I/O-vector in buffer *)
      len : int;  (** Length of I/O-vector in buffer *)
    }

  type 'buf kind  (** Kind of I/O-vector buffers *)

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  val string_kind : string kind
  val bigstring_kind : bigstring kind

  val empty : 'buf kind -> 'buf t
  (** [empty] the empty I/O-vector. *)

  val of_string : ?pos : int -> ?len : int -> string -> string t
  (** [of_string ?pos ?len str] @return an I/O-vector designated by
      position [pos] and length [len] in string [str].

      @raise [Invalid_argument] if designated substring out of bounds.

      @param pos default = 0
      @param len default = [String.length str - pos]
  *)

  val of_bigstring : ?pos : int -> ?len : int -> bigstring -> bigstring t
  (** [of_bigstring ?pos ?len bstr] @return an I/O-vector designated by
      position [pos] and length [len] in bigstring [bstr].

      @raise [Invalid_argument] if designated substring out of bounds.

      @param pos default = 0
      @param len default = [String.length str - pos]
  *)

  val drop : 'buf t -> int -> 'buf t
  (** [drop iovec n] drops [n] characters from [iovec].  @return resulting
      I/O-vector.

      @raise [Failure] if [n] is greater than length of [iovec].
  *)

  val max_iovecs : int
end


(** {2 I/O functions} *)

external int_of_file_descr : file_descr -> int = "%identity"
(** [int_of_file_descr fd] converts file descriptor [fd] to the internal
    integer value. *)

external file_descr_of_int : int -> file_descr = "%identity"
(** [file_descr_of_int n] converts an integer to a file descriptor. *)

external dirfd : dir_handle -> file_descr = "unix_dirfd"
(** Extract a file descriptor from a directory handle. *)

external sync : unit -> unit = "unix_sync"
(** Synchronize all filesystem buffers with disk. *)

external fsync : file_descr -> unit = "unix_fsync"
(** Synchronize the kernel buffers of a given file descriptor with disk. *)

external fdatasync : file_descr -> unit = "unix_fdatasync"
(** Synchronize the kernel buffers of a given file descriptor with disk,
    but do not necessarily write file attributes. *)

external readdir_ino : dir_handle -> string * int = "unix_readdir_ino_stub"
(** [readdir_ino dh] return the next entry in a directory (([(filename,
    inode)]).  @raise End_of_file when the end of the directory has been
    reached. *)

val read_assume_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> string -> int
(** [read_assume_nonblocking fd ?pos ?len buf] calls the system call
    [read] ASSUMING THAT IT IS NOT GOING TO BLOCK.  Reads at most [len]
    bytes into buffer [buf] starting at position [pos].  @return the
    number of bytes actually read.

    @raise [Invalid_argument] if buffer range out of bounds.
    @raise [Unix_error].

    @param pos = 0
    @param len = [String.length buf - pos]
*)

val write_assume_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> string -> int
(** [write_assume_nonblocking fd ?pos ?len buf] calls the system call
    [write] ASSUMING THAT IT IS NOT GOING TO BLOCK.  Writes at most [len]
    bytes from buffer [buf] starting at position [pos].  @return the
    number of bytes actually written.

    @raise [Invalid_argument] if buffer range out of bounds.
    @raise [Unix_error].

    @param pos = 0
    @param len = [String.length buf - pos]
*)

val writev_assume_nonblocking :
  file_descr -> ?count : int -> string IOVec.t array -> int
(** [writev_assume_nonblocking fd ?count iovecs] calls the system call
    [writev] ASSUMING THAT IT IS NOT GOING TO BLOCK using [count]
    I/O-vectors [iovecs].  @return the number of bytes actually written.

    @raise [Invalid_argument] if the designated ranges are invalid.
    @raise [Unix_error] on Unix-errors.
*)

external pselect :
  file_descr list -> file_descr list -> file_descr list -> float -> int list ->
  file_descr list * file_descr list * file_descr list = "unix_pselect_stub"
(** [pselect rfds wfds efds timeout sigmask] like {!Unix.select} but
    also allows one to wait for the arrival of signals. *)


(** {2 Clock functions} *)

#if defined(_POSIX_MONOTONIC_CLOCK) && (_POSIX_MONOTONIC_CLOCK > -1)
(** Type of Unix-clocks *)
type clock

external clock_gettime : clock -> float = "unix_clock_gettime"
(** [clock_gettime clock] @return time in seconds associated with [clock].
    @raise [Unix_error]. *)

external clock_settime : clock -> float -> unit = "unix_clock_settime"
(** [clock_settime clock time] sets [clock] to the [time] in seconds.
    @raise [Unix_error]. *)

external clock_getres : clock -> float = "unix_clock_getres"
(** [clock_getres clock] @return the resolution in seconds of [clock].
    @raise [Unix_error]. *)

external pthread_getcpuclockid :
  Thread.t -> clock = "unix_pthread_getcpuclockid"
(** [pthread_getcpuclockid tid] @return the CPU-clock associated with
    the thread having thread ID [tid].
    @raise [Unix_error]. *)
#else
#warning "POSIX MON not present; clock functions undefined"
#endif


(** {2 Resource limits} *)

module RLimit : sig
  type limit = Limit of int64 | Infinity

  type t = { cur : limit; max : limit }

  type resource =
    | CORE
    | CPU
    | DATA
    | FSIZE
    | NOFILE
    | STACK
    | AS
end

external getrlimit : RLimit.resource -> RLimit.t = "unix_getrlimit"
external setrlimit: RLimit.resource -> RLimit.t -> unit = "unix_setrlimit"


(** {2 Resource usage} *)
module RUsage : sig
  type who = SELF | CHILDREN

  type t = {
    ru_utime : float;
    ru_stime : float;
    ru_maxrss : int64;
    ru_ixrss : int64;
    ru_idrss : int64;
    ru_isrss : int64;
    ru_minflt : int64;
    ru_majflt : int64;
    ru_nswap : int64;
    ru_inblock : int64;
    ru_oublock : int64;
    ru_msgsnd : int64;
    ru_msgrcv : int64;
    ru_nsignals : int64;
    ru_nvcsw : int64;
    ru_nivcsw : int64;
  }

  val ru_utime : t -> float
  val ru_stime : t -> float
  val ru_maxrss : t -> int64
  val ru_ixrss : t -> int64
  val ru_idrss : t -> int64
  val ru_isrss : t -> int64
  val ru_minflt : t -> int64
  val ru_majflt : t -> int64
  val ru_nswap : t -> int64
  val ru_inblock : t -> int64
  val ru_oublock : t -> int64
  val ru_msgsnd : t -> int64
  val ru_msgrcv : t -> int64
  val ru_nsignals : t -> int64
  val ru_nvcsw : t -> int64
  val ru_nivcsw : t -> int64

(** [RUsage.add ru1 ru2] adds two rusage structures (e.g. your resource
    usage and your children's). *)
  val add : t -> t -> t
end

external getrusage : RUsage.who -> RUsage.t = "unix_getrusage"


(** {2 System configuration} *)

type sysconf =
  | ARG_MAX
  | CHILD_MAX
  | HOST_NAME_MAX
  | LOGIN_NAME_MAX
  | OPEN_MAX
  | PAGESIZE
  | RE_DUP_MAX
  | STREAM_MAX
  | SYMLOOP_MAX
  | TTY_NAME_MAX
  | TZNAME_MAX
  | POSIX_VERSION
  | PHYS_PAGES
  | AVPHYS_PAGES
  | IOV_MAX

external sysconf : sysconf -> int64 = "unix_sysconf"


(** {2 POSIX thread functions} *)

external mutex_timedlock : Mutex.t -> float -> bool = "unix_mutex_timedlock"
(** [mutex_timedlock mtx timeout] tries to lock [mtx], but returns once
    [timeout] expires.  Note that [timeout] is an absolute Unix-time
    to prevent time-related race conditions.  @return [false] iff
    the timer expired without the lock being acquired.  See [man
    pthread_mutex_timedlock] for details. *)

external condition_timedwait :
  Condition.t -> Mutex.t -> float -> bool = "unix_condition_timedwait"
(** [condition_timedwait cnd mtx timeout] waits on condition variable
    [cond] with mutex [mtx] until either the condition is signalled,
    or until [timeout] expires.  Note that [timeout] is an absolute
    Unix-time to prevent time-related race conditions. @return [false]
    iff the timer expired, but this does not mean that the condition is
    not true due to an unavoidable race condition in the system call.
    See [man pthread_cond_timedwait] for details. *)


(** {2 Pathname resolution} *)

external realpath : string -> string = "unix_realpath"
(** [realpath path] @return the canonicalized absolute pathname of [path].

    @raise Unix_error on errors. *)

(** {2 Temp dir creation} *)

external mkdtemp : string -> string = "unix_mkdtemp"
(** [mkdtemp pattern] pattern must end with XXXXXX.
    Will replace XXXXXX with unique characters and create a new directory
    with the generated name.
    @raise Unix_error on errors. *)

(** {2 Signal handling} *)

external abort : unit -> 'a = "unix_abort" "noalloc"

(** {2 User id, group id} *)

external initgroups : string -> int -> unit = "unix_initgroups"
