(*pp $(pwd)/pp.sh *)
(*
#include <unistd.h>
end-pp-include*)
(** Interface to Linux-specific system calls *)

open Unix
open Unix_ext

(** {2 Filesystem functions} *)

val sendfile : ?pos : int -> ?len : int -> fd : file_descr -> file_descr -> int
(** [sendfile ?pos ?len ~fd sock] sends mmap-able data from file
    descriptor [fd] to socket [sock] using offset [pos] and length [len].
    @return the number of characters actually written.

    NOTE: if the returned value is unequal to what was requested (=
    the initial size of the data by default), the system call may have
    been interrupted by a signal, the source file may have been truncated
    during operation, or a timeout occurred on the socket during sending.
    It is currently impossible to find out which of the events above
    happened.  Calling {!sendfile} several times on the same descriptor
    that only partially accepted data due to a timeout will eventually
    lead to the unix error [EAGAIN].

    @raise [Unix_error] on Unix-errors.

    @param default pos = 0
    @param default len = length of data (file) associated with descriptor [fd]
*)


(** {2 Non-portable TCP-functionality} *)

type tcp_bool_option =
  | TCP_CORK
  | TCP_NODELAY

external gettcpopt_bool :
  file_descr -> tcp_bool_option -> bool = "linux_gettcpopt_bool_stub"
(** [gettcpopt_bool sock opt] @return the current value of the boolean
    TCP socket option [opt] for socket [sock]. *)

external settcpopt_bool :
  file_descr -> tcp_bool_option -> bool -> unit = "linux_settcpopt_bool_stub"
(** [settcpopt_bool sock opt v] sets the current value of the boolean
    TCP socket option [opt] for socket [sock] to value [v]. *)

val send_nonblocking_no_sigpipe :
  file_descr -> ?pos : int -> ?len : int -> string -> int option
(** [send_nonblocking_no_sigpipe sock ?pos ?len buf] tries to do a
    nonblocking send on socket [sock] given buffer [buf], offset [pos]
    and length [len].  Prevents [SIGPIPE], i.e. raise a Unix-error in
    that case immediately.  @return [Some n_bytes] for the number of
    bytes written, or [None] if the operation would have blocked.

    @param pos default = 0
    @param len default = [String.length buf - pos]

    @raise [Invalid_argument] if the designated buffer range is invalid.
    @raise [Unix_error] on Unix-errors.
*)

val send_no_sigpipe :
  file_descr -> ?pos : int -> ?len : int -> string -> int
(** [send_no_sigpipe sock ?pos ?len buf] tries to do a
    blocking send on socket [sock] given buffer [buf], offset [pos]
    and length [len].  Prevents [SIGPIPE], i.e. raise a Unix-error in
    that case immediately.  @return the number of bytes written.

    @param pos default = 0
    @param len default = [String.length buf - pos]

    @raise [Invalid_argument] if the designated buffer range is invalid.
    @raise [Unix_error] on Unix-errors.
*)

val sendmsg_nonblocking_no_sigpipe :
  file_descr -> ?count : int -> string IOVec.t array -> int
(** [sendmsg_nonblocking_no_sigpipe sock ?count iovecs] tries to do
    a nonblocking send on socket [sock] using [count] I/O-vectors
    [iovecs].  Prevents [SIGPIPE], i.e. raises a Unix-error in that
    case immediately.  @return the number of bytes written or [-1]
    if the operation would have blocked.

    @raise [Invalid_argument] if the designated ranges are invalid.
    @raise [Unix_error] on Unix-errors.
*)


(** {2 Sending File Descriptors Between Processes} *)

val send_fd : sock : file_descr -> fd_to_send : file_descr -> unit
(** [send_fd ~sock ~fd_to_send] send [fd_to_send] to the process on
    the other side of [sock].  That process must be on the same computer,
    and [sock] must be a unix domain socket.  This is a blocking call,
    however if select has said [sock] is ready for writing, then this
    call will never block.

    @raise [Unix_error] on Unix-errors.
*)

val recv_fd : sock : file_descr -> file_descr
(** [recv_fd ~sock] receive a file descriptor from the process on the
    other side of [sock].  This is a blocking call, however if select
    has said [sock] is ready to read, then this call will never block.

    @raise [Unix_error] on Unix-errors
    @raise [Failure] if the thing received isn't a socket
*)


(** {2 Clock functions} *)
#if defined(_POSIX_MONOTONIC_CLOCK) && (_POSIX_MONOTONIC_CLOCK > -1)
val clock_process_cputime_id : clock
(** [clock_process_cputime_id] the clock measuring the CPU-time of a process. *)

val clock_thread_cputime_id : clock
(** [clock_thread_cputime_id] the clock measuring the CPU-time of a thread. *)


(** {2 Getting terminal size} *)

external get_terminal_size : unit -> int * int = "linux_get_terminal_size_stub"
(** [get_terminal_size ()] @return [(rows, cols)], the number of rows and
    columns of the terminal. *)
#else
#warning "POSIX MON not present; clock functions undefined"
#endif


(** {2 Parent death notifications} *)

external pr_set_pdeathsig : int -> unit = "linux_pr_set_pdeathsig_stub"
(** [pr_set_pdeathsig s] sets the signal [s] to be sent to the executing
    process when its parent dies.  NOTE: the parent may have died
    before or while executing this system call.  To make sure that you
    do not miss this event, you should call {!Unix.getppid ()} to get
    the parent process id after this system call.  If the parent has
    died, the returned parent PID will be 1, i.e. the init process will
    have adopted the child.  You should then either send the signal to
    yourself using {!Unix.kill}, or execute an appropriate handler. *)

external pr_get_pdeathsig : unit -> int = "linux_pr_get_pdeathsig_stub"
(** [pr_get_pdeathsig ()] get the signal that will be sent to the
    currently executing process when its parent dies. *)


(** {2 Pathname resolution} *)

val file_descr_realpath : file_descr -> string
(** [file_descr_realpath fd] @return the canonicalized absolute
    pathname of the file associated with file descriptor [fd].

    @raise Unix_error on errors.
*)

val out_channel_realpath : out_channel -> string
(** [out_channel_realpath oc] @return the canonicalized absolute
    pathname of the file associated with output channel [oc].

    @raise Unix_error on errors.
*)

val in_channel_realpath : in_channel -> string
(** [in_channel_realpath ic] @return the canonicalized absolute
    pathname of the file associated with input channel [ic].

    @raise Unix_error on errors.
*)


(** {2 Epoll} *)

(** NOTE: please read the [epoll] man page for reference! *)

module Epoll : sig
  (** {6 Epoll flags} *)

  type flag = IN | OUT | PRI | ERR | HUP | ET | ONESHOT
  (** Type of Epoll event flag *)

  type flags
  (** Type of Epoll event flags *)

  external make_flags : flag array -> flags = "linux_epoll_make_flags_stub"
  (** [make_flags ar] @return flags constructed from the array of flags [ar]. *)

  external get_flags : flags -> flag array = "linux_epoll_get_flags_stub"
  (** [get_flags flags] @return the array of all flags set in [flags]. *)

  external has_in : flags -> bool = "linux_epoll_has_EPOLLIN_stub" "noalloc"
  (** [has_in flags] @return [true] iff the [EPOLLIN] flag is set in [flags]. *)

  external has_out : flags -> bool = "linux_epoll_has_EPOLLOUT_stub" "noalloc"
  (** [has_out flags] @return [true] iff the [EPOLLOUT] flag is set in
      [flags]. *)

  external has_pri : flags -> bool = "linux_epoll_has_EPOLLPRI_stub" "noalloc"
  (** [has_pri flags] @return [true] iff the [EPOLLPRI] flag is set in
      [flags]. *)

  external has_err : flags -> bool = "linux_epoll_has_EPOLLERR_stub" "noalloc"
  (** [has_err flags] @return [true] iff the [EPOLLERR] flag is set in
      [flags]. *)

  external has_hup : flags -> bool = "linux_epoll_has_EPOLLHUP_stub" "noalloc"
  (** [has_hup flags] @return [true] iff the [EPOLLHUP] flag is set in
      [flags]. *)

  external has_et : flags -> bool = "linux_epoll_has_EPOLLET_stub" "noalloc"
  (** [has_et flags] @return [true] iff the [EPOLLET] flag is set in [flags]. *)

  external has_oneshot :
    flags -> bool = "linux_epoll_has_EPOLLONESHOT_stub" "noalloc"
  (** [has_oneshot flags] @return [true] iff the [EPOLLONESHOT] flag is
      set in [flags]. *)

  val flag_to_string : flag -> string


  (** {6 Epoll functions} *)

  external create : int -> file_descr = "linux_epoll_create_stub"
  (** [create n] @return epoll file descriptor that preallocates [n]
      event backing stores for file descriptors.

      @raise Unix_error
  *)

  external add : epfd : file_descr -> fd : file_descr -> flags -> unit
    = "linux_epoll_add_stub"
  (** [add ~epfd ~fd flags] add file descriptor [fd] to epoll file
      descriptor [epfd] using event flags [flags].

      @raise Unix_error
  *)

  external modify : epfd : file_descr -> fd : file_descr -> flags -> unit
    = "linux_epoll_modify_stub"
  (** [modify ~epfd ~fd flags] modifies file descriptor [fd] controlled by
      epoll file descriptor [epfd] using event flags [flags].

      @raise Unix_error
  *)

  external del :
    epfd : file_descr -> fd : file_descr -> unit = "linux_epoll_del_stub"
  (** [del ~epfd ~fd] removes file descriptor [fd] from epoll file
      descriptor [epfd].

      @raise Unix_error
  *)

  external wait :
    file_descr -> maxevents : int -> timeout : int
    -> (file_descr * flags) array = "linux_epoll_wait_stub"
  (** [wait epfd ~maxevents ~timeout] waits for at most [maxevents]
      events on epoll file descriptor [epfd] using timeout [timeout].
      @return the array of received events.  If a timeout occurs, the
      returned array will be empty.

      @raise Failure iff [maxevents <= 0]
      @raise Unix_error
  *)
end


(** {6 Splicing - zero-copies between kernel buffers} *)

(* FIXME: not yet available with Fedora 5; the kernel supports this
   but not GLIBC.  Centos 5 seems to work fine with these functions.
   Uncomment this region and the corresponding stubs in linux_ext_stubs.c
   once the switch to Centos 5 is done.  STILL NEEDS TESTING!!!

(* Example usage diagram:

   In the below diagram, starting at the left upper corner, we first
   splice a socket into a pipe.  Then we duplicate this pipe into two
   other pipes using "tee".  The first pipe is spliced into a file
   descriptor (e.g. to log data coming in from a socket connection to
   a file).  The second pipe is used by the user to actually read data.

   After handling the received data, the user puts the new data to
   be sent out into an output buffer and vmsplices it into a pipe.
   Use double buffering ( = switching between two buffers) to prevent
   data inconsistencies while the kernel may be reading from the user
   provided pages, and make sure not to let buffers be reclaimed by the
   GC as long as they may still be in use!  These buffers currently need
   not be larger than 64KB each, which is the size of kernel buffers
   (= Unix-pipes).

   The end of the output pipe is then duplicated into two more output
   pipes using "tee" again.  If these two "tees" have seen all the data
   vmspliced from a user buffer, the user can safely switch to it again
   from the other double buffer.

   Finally, the first pipe is used to splice data into a socket to send
   out the user data, and the second pipe is used to stream this data
   to a file.  Note that using more pipes and more "tee"-calls one can
   very cheaply duplicate the data to even more destinations!

                       tee              splice
                           +----> pipe ----+---> fd
             splice       /
     socket ----+---> pipe
                          \             read
                           +----> pipe ---+--> user input space
                       tee                       |
                                                 + do stuff
                                                 |
                                               user output space
                                                 |
                                                 + vmsplice (double buffer!)
                                                 |
                                                pipe
                                                 |
                                                / \
                                           tee +   + tee
                                               |   |
                                             pipe pipe
                                             /       \
                                     splice +         + splice
                                           /           \
                                         sock           fd
*)

module Splice : sig
  (** {6 Splice flags} *)

  (** Type of Splice event flag *)
  type flag =
    | MOVE
    | NONBLOCK
    | MORE
    | GIFT

  (** Type of Splice event flags *)
  type flags

  external make_flags : flag array -> flags = "linux_splice_make_flags_stub"
  (** [make_flags ar] @return flags constructed from the array of flags [ar]. *)


  (** {6 Splice functions} *)

  val splice :
    ?assume_nonblocking : bool ->
    fd_in : file_descr -> ?off_in : int ->
    fd_out : file_descr -> ?off_out : int ->
    len : int ->
    flags
    -> int * int * int
  (** [splice ?assume_nonblocking ~fd_in ?off_in ~fd_out ?off_out
      ~len flags] see man-page for details.  @return the triple [(ret,
      ret_off_in, ret_off_out)], where [ret] corresponds to the return
      value of the system call, [ret_off_in] to the final input offset,
      and [ret_off_out] to the final output offset.

      @raise Unix_error
      @raise [Invalid_argument] if the offsets or length are invalid

      @param assume_nonblocking default = false
      @param off_in default = 0
      @param off_out default = 0
  *)

  val tee :
    ?assume_nonblocking : bool ->
    fd_in : file_descr -> fd_out : file_descr -> int -> flags -> int
  (** [tee ?assume_nonblocking ~fd_in ~fd_out len flags] see man-page
      for details.

      @raise Unix_error
      @raise [Invalid_argument] if the length is invalid

      @param assume_nonblocking default = false
  *)

  val vmsplice :
    ?assume_nonblocking : bool ->
    file_descr -> Bigstring.t IOVec.t array -> ?count : int -> flags -> int
  (** [vmsplice ?assume_nonblocking fd iovecs ?count flags] see man-page
      for details.

      @raise Unix_error
      @raise [Invalid_argument] if the count is invalid

      @param assume_nonblocking default = false
      @param count default = [Array.length iovecs]
  *)
end
*)
