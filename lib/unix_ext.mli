(*pp $(pwd)/pp.sh *)
(*
#include <unistd.h>
end-pp-include*)
(** Interface to additional Unix-system calls *)

open Unix

(** {2 Utility functions} *)

external unix_error : int -> string -> string -> _ = "unix_error_stub"
(** @raise Unix_error with a given errno, function name and argument *)

external exit_immediately : int -> _ = "caml_sys_exit"
(** [exit_immediately exit_code] immediately calls the [exit] system call
    with the given exit code without performing any other actions
    (unlike Pervasives.exit).  Does not return. *)


(** {2 Filesystem functions} *)

val mknod :
  ?file_kind : file_kind ->
  ?perm : int ->
  ?major : int ->
  ?minor : int ->
  string -> unit
(** [mknod ?file_kind ?perm ?major ?minor path] creates a filesystem
    entry.  Note that only FIFO-entries are guaranteed to be supported
    across all platforms as required by the POSIX-standard.  On Linux
    directories and symbolic links cannot be created with this function.
    Use {!Unix.mkdir} and {!Unix.symlink} instead there respectively.

    @raise Invalid_argument if an unsupported file kind is used.
    @raise Unix_error if the system call fails.

    @param file_kind default = [S_REG] (= regular file)
    @param perm default = [0o600] (= read/write for user only)
    @param major default = [0]
    @param minor default = [0]
*)


(** {2 I/O vectors} *)

(** I/O-vectors for scatter/gather-operations *)
module IOVec : sig
  open Bigarray

  (** Representation of I/O-vectors.
      NOTE: DO NOT CHANGE THE MEMORY LAYOUT OF THIS TYPE!!!
      All C-functions in our bindings that handle I/O-vectors depend on it.
  *)
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

      @raise Invalid_argument if designated substring out of bounds.

      @param pos default = 0
      @param len default = [String.length str - pos]
  *)

  val of_bigstring : ?pos : int -> ?len : int -> bigstring -> bigstring t
  (** [of_bigstring ?pos ?len bstr] @return an I/O-vector designated by
      position [pos] and length [len] in bigstring [bstr].

      @raise Invalid_argument if designated substring out of bounds.

      @param pos default = 0
      @param len default = [String.length str - pos]
  *)

  val drop : 'buf t -> int -> 'buf t
  (** [drop iovec n] drops [n] characters from [iovec].  @return resulting
      I/O-vector.

      @raise Failure if [n] is greater than length of [iovec].
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

#if defined(_POSIX_SYNCHRONIZED_IO) && (_POSIX_SYNCHRONIZED_IO > -1)
external fdatasync : file_descr -> unit = "unix_fdatasync"
(** Synchronize the kernel buffers of a given file descriptor with disk,
    but do not necessarily write file attributes. *)
#else
#warning "POSIX_SYNCHRONIZED_IO not supported, fdatasync not available"
#endif

external readdir_ino :
  dir_handle -> string * nativeint = "unix_readdir_ino_stub"
(** [readdir_ino dh] return the next entry in a directory (([(filename,
    inode)]).  @raise End_of_file when the end of the directory has been
    reached. *)

val read_assume_fd_is_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> string -> int
(** [read_assume_fd_is_nonblocking fd ?pos ?len buf] calls the system call
    [read] ASSUMING THAT IT IS NOT GOING TO BLOCK.  Reads at most [len]
    bytes into buffer [buf] starting at position [pos].  @return the
    number of bytes actually read.

    @raise Invalid_argument if buffer range out of bounds.
    @raise Unix_error on Unix-errors.

    @param pos = 0
    @param len = [String.length buf - pos]
*)

val write_assume_fd_is_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> string -> int
(** [write_assume_fd_is_nonblocking fd ?pos ?len buf] calls the system call
    [write] ASSUMING THAT IT IS NOT GOING TO BLOCK.  Writes at most [len]
    bytes from buffer [buf] starting at position [pos].  @return the
    number of bytes actually written.

    @raise Invalid_argument if buffer range out of bounds.
    @raise Unix_error on Unix-errors.

    @param pos = 0
    @param len = [String.length buf - pos]
*)

val writev_assume_fd_is_nonblocking :
  file_descr -> ?count : int -> string IOVec.t array -> int
(** [writev_assume_fd_is_nonblocking fd ?count iovecs] calls the system call
    [writev] ASSUMING THAT IT IS NOT GOING TO BLOCK using [count]
    I/O-vectors [iovecs].  @return the number of bytes actually written.

    @raise Invalid_argument if the designated ranges are invalid.
    @raise Unix_error on Unix-errors.
*)

val writev : file_descr -> ?count : int -> string IOVec.t array -> int
(** [writev fd ?count iovecs] like {!writev_assume_fd_is_nonblocking}, but does
    not require the descriptor to not block.  If you feel you have to
    use this function, you should probably have chosen I/O-vectors that
    build on bigstrings, because this function has to internally blit
    the I/O-vectors (ordinary OCaml strings) to intermediate buffers on
    the C-heap.

    @return the number of bytes actually written.

    @raise Invalid_argument if the designated ranges are invalid.
    @raise Unix_error on Unix-errors.
*)

external pselect :
  file_descr list -> file_descr list -> file_descr list -> float -> int list ->
  file_descr list * file_descr list * file_descr list = "unix_pselect_stub"
(** [pselect rfds wfds efds timeout sigmask] like {!Core_unix.select} but
    also allows one to wait for the arrival of signals. *)

#if defined(_POSIX_MONOTONIC_CLOCK) && (_POSIX_MONOTONIC_CLOCK > -1)
(** {2 Clock functions} *)

(** Type of Unix-clocks *)



(* See the man pages for details. *)
module Clock : sig
  type t

  (** [get tid] @return the CPU-clock associated with the thread having
      thread ID [tid].  @raise Unix_error on Unix-errors. *)
  external get : Thread.t -> t = "unix_pthread_getcpuclockid"

  (** [get_time clock] @return time in seconds associated with [clock].
      @raise Unix_error on Unix-errors. *)
  external get_time : t -> float = "unix_clock_gettime"

  (** [set_time clock time] sets [clock] to the [time] in seconds.
      @raise Unix_error on Unix-errors. *)
  external set_time : t -> float -> unit = "unix_clock_settime"

  (** [get_resolution clock] @return the resolution in seconds of
      [clock].  @raise Unix_error on Unix-errors. *)
  external get_resolution : t -> float = "unix_clock_getres"

end
#warning "POSIX MON not present; clock functions undefined"
#endif


(** {2 Resource limits} *)

module RLimit : sig
  type limit = Limit of int64 | Infinity
  type t = {
    cur : limit;  (* soft limit *)
    max : limit;  (* hard limit (ceiling for soft limit) *)
  }

  
  type resource =
    | CORE
    | CPU
    | DATA
    | FSIZE
    | NOFILE
    | STACK
    | AS

  
  (* See man pages for "getrlimit" and "setrlimit" for details. *)
  external get : resource -> t = "unix_getrlimit"
  external set : resource -> t -> unit = "unix_setrlimit"
end


(** {2 Resource usage} -- For details, "man getrusage" *)
module Resource_usage : sig
  
  type who = SELF | CHILDREN

  type t

  val get : who -> t

  val utime : t -> float      (* user time used *)
  val stime : t -> float      (* system time used *)
  val maxrss : t -> int64     (* maximum resident set size *)
  val ixrss : t -> int64      (* integral shared memory size *)
  val idrss : t -> int64      (* integral unshared data size *)
  val isrss : t -> int64      (* integral unshared stack size *)
  val minflt : t -> int64     (* page reclaims *)
  val majflt : t -> int64     (* page faults *)
  val nswap : t -> int64      (* swaps *)
  val inblock : t -> int64    (* block input operations *)
  val oublock : t -> int64    (* block output operations *)
  val msgsnd : t -> int64     (* messages sent *)
  val msgrcv : t -> int64     (* messages received *)
  val nsignals : t -> int64   (* signals received *)
  val nvcsw : t -> int64      (* voluntary context switches *)
  val nivcsw : t -> int64     (* involuntary context switches *)

  (** [add ru1 ru2] adds two rusage structures (e.g. your resource usage
      and your children's). *)
  val add : t -> t -> t
end


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

(* These are low-level system calls with ugly types.  See the Mutex
   module for nicer versions. *)
#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)

external mutex_timedlock : Mutex.t -> float -> bool = "unix_mutex_timedlock"
(** [mutex_timedlock mtx timeout] tries to lock [mtx], but returns once
    [timeout] expires.  Note that [timeout] is an absolute Unix-time
    to prevent time-related race conditions.  @return [false] iff
    the timer expired without the lock being acquired.  See [man
    pthread_mutex_timedlock] for details. *)
#else
#warning "POSIX TMO not present; mutex_timedlock undefined"
#endif


external condition_timedwait :
  Condition.t -> Mutex.t -> float -> bool = "unix_condition_timedwait"
(** [condition_timedwait cnd mtx timeout] waits on condition variable
    [cond] with mutex [mtx] until either the condition is signalled,
    or until [timeout] expires.  Note that [timeout] is an absolute
    Unix-time to prevent time-related race conditions. @return [false]
    iff the timer expired, but this does not mean that the condition is
    not true due to an unavoidable race condition in the system call.
    See [man pthread_cond_timedwait] for details. *)


external create_error_checking_mutex :
  unit -> Mutex.t = "unix_create_error_checking_mutex"
(** [create_error_checking_mutex ()] like {!Mutex.create}, but creates
    an error-checking mutex.  Locking a mutex twice from the same thread,
    unlocking an unlocked mutex, or unlocking a mutex not held by the
    thread will result in a [Sys_error] exception. *)


(** {2 Pathname resolution} *)


external realpath : string -> string = "unix_realpath"
(** [realpath path] @return the canonicalized absolute pathname of [path].

    @raise Unix_error on errors. *)



(** {2 Temporary file and directory creation} *)

(** [mkstemp prefix] creates and opens a unique temporary file with
    [prefix], automatically appending a suffix of six random characters
    to make the name unique.

    @raise Unix_error on errors.
*)
external mkstemp : string -> string * Unix.file_descr = "unix_mkstemp"

(** [mkdtemp prefix] creates a temporary directory with [prefix],
    automatically appending a suffix of six random characters to make
    the name unique.

    @raise Unix_error on errors.
k*)
external mkdtemp : string -> string = "unix_mkdtemp"


(** {2 Signal handling} *)

(* Causes abnormal program termination unless the signal SIGABRT is
   caught and the signal handler does not return.  If the abort() function
   causes program termination, all open streams are closed and flushed,
   but OCaml finalizers will not be run.

   If the SIGABRT signal is blocked or ignored, the abort() function
   will still override it.
*)

external abort : unit -> _ = "unix_abort" "noalloc"


(** {2 User id, group id} *)

external initgroups : string -> int -> unit = "unix_initgroups"


(** {2 Globbing and shell expansion} *)

module Fnmatch_flags : sig
  type t

  type flag = [
    | `NOESCAPE
    | `PATHNAME
    | `PERIOD
    | `FILE_NAME
    | `LEADING_DIR
    | `CASEFOLD
  ]

  (** [make l] @return flags constructed from the list of flags [l]. *)
  val make : flag list -> t
end



val fnmatch : ?flags : Fnmatch_flags.t -> pat : string -> string -> bool

module Wordexp_flags : sig
  type t
  type flag = [ `NOCMD | `SHOWERR | `UNDEF ]

  (** [make l] @return flags constructed from the list of flags [l]. *)
  val make : flag list -> t
end

(* See man page for wordexp. *)
val wordexp : ?flags : Wordexp_flags.t -> string -> string array

#if defined(__linux__)
(** {2 Additional IP functionality} *)

(* [if_indextoname ifindex] If [ifindex] is an interface index, then
   the function returns the interface name.  Otherwise, it raises
   [Unix_error]. *)
external if_indextoname : int -> string = "unix_if_indextoname"

(** [mcast_join ?ifname sock addr] join a multicast group at [addr]
    with socket [sock], optionally using network interface [ifname].

    @param ifname default = any interface
*)
external mcast_join :
  ?ifname : string -> file_descr -> sockaddr -> unit = "unix_mcast_join"

(** [mcast_leave ?ifname sock addr] leaves a multicast group at [addr]
    with socket [sock], optionally using network interface [ifname].

    @param ifname default = any interface
*)
external mcast_leave :
  ?ifname : string -> file_descr -> sockaddr -> unit = "unix_mcast_leave"
#else
#warning "not on linux, multicast stuff not included"
#endif
