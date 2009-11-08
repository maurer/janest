(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
open Core.Std.Unix

(** {2 Epoll} *)

(** NOTE: please read the [epoll] man page for reference! *)

module Epoll : sig
  (** {6 Epoll flags} *)

  type flag = IN | OUT | PRI | ERR | HUP | ET | ONESHOT with sexp, bin_io
  (** Type of Epoll event flag *)

  type flags
  (** Type of Epoll event flags *)

  val make_flags : flag array -> flags
  (** [make_flags ar] @return flags constructed from the array of flags [ar]. *)

  val get_flags : flags -> flag array
  (** [get_flags flags] @return the array of all flags set in [flags]. *)

  val has_in : flags -> bool
  (** [has_in flags] @return [true] iff the [EPOLLIN] flag is set in [flags]. *)

  val has_out : flags -> bool
  (** [has_out flags] @return [true] iff the [EPOLLOUT] flag is set in
      [flags]. *)

  val has_pri : flags -> bool
  (** [has_pri flags] @return [true] iff the [EPOLLPRI] flag is set in
      [flags]. *)

  val has_err : flags -> bool
  (** [has_err flags] @return [true] iff the [EPOLLERR] flag is set in
      [flags]. *)

  val has_hup : flags -> bool
  (** [has_hup flags] @return [true] iff the [EPOLLHUP] flag is set in
      [flags]. *)

  val has_et : flags -> bool
  (** [has_et flags] @return [true] iff the [EPOLLET] flag is set in [flags]. *)

  val has_oneshot : flags -> bool
  (** [has_oneshot flags] @return [true] iff the [EPOLLONESHOT] flag is
      set in [flags]. *)

  val flag_to_string : flag -> string


  (** {6 Epoll functions} *)

  val create : int -> file_descr
  (** [create n] @return epoll file descriptor that preallocates [n]
      event backing stores for file descriptors.

      @raise Unix_error on Unix-errors.
  *)

  val add : epfd : file_descr -> fd : file_descr -> flags -> unit
  (** [add ~epfd ~fd flags] add file descriptor [fd] to epoll file
      descriptor [epfd] using event flags [flags].

      @raise Unix_error on Unix-errors.
  *)

  val modify : epfd : file_descr -> fd : file_descr -> flags -> unit
  (** [modify ~epfd ~fd flags] modifies file descriptor [fd] controlled by
      epoll file descriptor [epfd] using event flags [flags].

      @raise Unix_error on Unix-errors.
  *)

  val del :
    epfd : file_descr -> fd : file_descr -> unit
  (** [del ~epfd ~fd] removes file descriptor [fd] from epoll file
      descriptor [epfd].

      @raise Unix_error on Unix-errors.
  *)

  val wait :
    file_descr -> maxevents : int -> timeout : int -> (file_descr * flags) array
  (** [wait epfd ~maxevents ~timeout] waits for at most [maxevents]
      events on epoll file descriptor [epfd] using timeout [timeout].
      @return the array of received events.  If a timeout occurs, the
      returned array will be empty.

      @raise Failure iff [maxevents <= 0]
      @raise Unix_error on Unix-errors.
  *)
end


(** Reporting of open file descriptors *)
module Open_fds : sig
  type file = { name : string; deleted : bool } with sexp, bin_io

  type kind =
    | File of file
    | Socket of int64
    | Pipe of int64
    | Inotify
    | Unknown of string
  with sexp, bin_io

  type t = { fd : int; kind : kind } with sexp, bin_io

  val discover : ?pid : int -> unit -> t list
  (** [discover ?pid ()] discovers file descriptors opened by process with
      process id [pid] by looking at the proc file system.

      @param pid default = process id of calling process
  *)
end

