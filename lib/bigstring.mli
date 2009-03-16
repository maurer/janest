(*pp $(pwd)/pp.sh *)
(*
#include <unistd.h>
#include <netinet/in.h>
#undef SEEK_SET
end-pp-include*)
open Unix
open Bigarray

(** {6 Types and exceptions} *)

(** Type of bigstrings *)
type t = (char, int8_unsigned_elt, c_layout) Array1.t

(** Type of I/O errors *)
exception IOError of
  int *  (** Number of bytes successfully read/written before error *)
  exn  (** The occurred exception (e.g. Unix_error, End_of_file) *)


(** {6 Creation and string conversion} *)

val create : int -> t
(** [create length] @return a new bigstring having [length]. *)

val of_string : ?pos : int -> ?len : int -> string -> t
(** [of_string ?pos ?len str] @return a new bigstring that is equivalent
    to the substring of length [len] in [str] starting at position [pos].

    @param pos default = 0
    @param len default = [String.length str - pos]
*)

val to_string : ?pos : int -> ?len : int -> t -> string
(** [to_string ?pos ?len bstr] @return a new string that is equivalent
    to the substring of length [len] in [bstr] starting at position [pos].

    @param pos default = 0
    @param len default = [length bstr - pos]

    @raise [Invalid_argument] if the string would exceed runtime limits.
*)


(** {6 Checking} *)

val check_args : loc : string -> t -> pos : int -> len : int -> unit

val get_opt_len : t -> pos : int -> int option -> int


(** {6 Accessors} *)

val length : t -> int
(** [length bstr] @return the length of bigstring [bstr]. *)

(* CRv2 sweeks: [sub] may be a misleading name because it does not make a copy of
   the substring, unlike [Array.sub] and [String.sub]. 
*)
val sub : ?pos : int -> ?len : int -> t -> t
(** [sub ?pos ?len bstr] @return the sub-bigstring in [bstr] that start
    at position [pos] and has length [len].

    @param pos default = 0
    @param len default = [Bigstring.length bstr - pos]
*)

external is_mmapped : t -> bool = "bigstring_is_mmapped_stub" "noalloc"
(** [is_mmapped bstr] @return whether the bigstring [bstr] is
    memory-mapped. *)

(** {6 Blitting} *)

val blit : ?src_pos : int -> src : t -> ?dst_pos : int -> dst : t -> int -> unit
(** [blit ?src_pos ~src ?dst_pos ~dst len] blits [len] characters from
    bigstring [src] starting at position [src_pos] to bigstring [dst]
    at position [dst_pos].

    @raise [Invalid_argument] if the designated ranges are out of bounds.

    @param src_pos default = 0
    @param dst_pos default = 0
*)

val blit_string_bigstring :
  ?src_pos : int -> string -> ?dst_pos : int -> t -> len : int -> unit
(** [blit_string_bigstring ?src_pos str ?dst_pos bstr ~len] blits [len]
    characters from string [str] starting at position [src_pos] to
    bigstring [bstr] at position [dst_pos].

    @raise [Invalid_argument] if the designated ranges are out of bounds.

    @param src_pos default = 0
    @param dst_pos default = 0
*)

val blit_bigstring_string :
  ?src_pos : int -> t -> ?dst_pos : int -> string -> len : int -> unit
(** [blit_bigstring_string ?src_pos bstr ?dst_pos str ~len] blits [len]
    characters from bigstring [bstr] starting at position [src_pos]
    to string [str] at position [dst_pos].

    @raise [Invalid_argument] if the designated ranges are out of bounds.

    @param src_pos default = 0
    @param dst_pos default = 0
*)


(** {6 Input functions} *)

val read : ?min_len : int -> file_descr -> ?pos : int -> ?len : int -> t -> int
(** [read ?min_len fd ?pos ?len bstr] reads at least [min_len] (must be
    greater than or equal zero) and at most [len] (must be greater than
    or equal to [min_len]) bytes from file descriptor [fd], and writes
    them to bigstring [bstr] starting at position [pos].  @return the
    number of bytes actually read.

    NOTE: even if [len] is zero, there may still be errors when reading
    from the descriptor!

    @raise [Invalid_argument] if the designated ranges are out of bounds.

    @raise [IOError] in the case of input errors, or on EOF if the
    minimum length could not be read.

    @param pos default = 0
    @param min_len default = 0
    @param len default = [length bstr - pos]
*)

val really_read : file_descr -> ?pos : int -> ?len : int -> t -> unit
(** [really_read fd ?pos ?len bstr] reads [len] bytes from file descriptor
    [fd], and writes them to bigstring [bstr] starting at position [pos].

    @raise [Invalid_argument] if the designated range is out of bounds.
    @raise [IOError] in the case of input errors, or on EOF.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val really_recv : file_descr -> ?pos : int -> ?len : int -> t -> unit
(** [really_recv sock ?pos ?len bstr] receives [len] bytes from socket
    [sock], and writes them to bigstring [bstr] starting at position
    [pos].  If [len] is zero, the function returns immediately without
    performing the underlying system call.

    @raise [Invalid_argument] if the designated range is out of bounds.
    @raise [IOError] in the case of input errors, or on EOF.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val read_assume_nonblocking : file_descr -> ?pos : int -> ?len : int -> t -> int
(** [read_assume_nonblocking fd ?pos ?len bstr] reads up to [len] bytes
    into bigstring [bstr] starting at position [pos] from file descriptor
    [fd] without yielding to other OCaml-threads.  @return the number
    of bytes actually read.

    @raise [Unix_error] in the case of input errors.
    @raise [Invalid_argument] if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val input : ?min_len : int -> in_channel -> ?pos : int -> ?len : int -> t -> int
(** [input ?min_len ic ?pos ?len bstr] tries to read [len] bytes
    (guarantees to read at least [min_len] bytes (must be greater than
    or equal to zero and smaller or equal to [len]), if possible, before
    returning) from input channel [ic], and writes them to bigstring
    [bstr] starting at position [pos].  @return the number of bytes
    actually read.

    NOTE: even if [len] is zero, there may still be errors when reading
    from the descriptor, which will be done if the internal buffer
    is empty!

    NOTE: if at least [len] characters are available in the input channel
    buffer and if [len] is not zero, data will only be fetched from the
    channel buffer.  Otherwise data will be read until at least [min_len]
    characters are available.

    @raise [Invalid_argument] if the designated range is out of bounds.
    @raise [IOError] in the case of input errors, or on premature EOF.

    @param pos default = 0
    @param min_len default = 0
    @param len default = [length bstr - pos]
*)

val really_input : in_channel -> ?pos : int -> ?len : int -> t -> unit
(** [really_input ic ?pos ?len bstr] reads exactly [len] bytes from
    input channel [ic], and writes them to bigstring [bstr] starting at
    position [pos].

    @raise [Invalid_argument] if the designated range is out of bounds.
    @raise [IOError] in the case of input errors, or on premature EOF.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)


(** {6 Output functions} *)

val really_write : file_descr -> ?pos : int -> ?len : int -> t -> unit
(** [really_write fd ?pos ?len bstr] writes [len] bytes in bigstring
    [bstr] starting at position [pos] to file descriptor [fd].

    @raise [Invalid_argument] if the designated range is out of bounds.
    @raise [IOError] in the case of output errors.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

#if defined(MSG_NOSIGNAL)
val really_send_no_sigpipe : file_descr -> ?pos : int -> ?len : int -> t -> unit
(** [really_send_no_sigpipe sock ?pos ?len bstr] sends [len] bytes in
    bigstring [bstr] starting at position [pos] to socket [sock] without
    blocking and ignoring [SIGPIPE].

    @raise [Invalid_argument] if the designated range is out of bounds.
    @raise [IOError] in the case of output errors.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

#else
#warning "MSG_NOSIGNAL not defined; really_send_no_sigpipe"
#warning "not implemented."
#warning "Try compiling on Linux?"
#endif

#if defined(MSG_NOSIGNAL)
(* CRv2 sweeks for MM: It would be clearer to return a variant rather than using [-1] to
   indicate blocking.
*)

val send_nonblocking_no_sigpipe :
  file_descr -> ?pos : int -> ?len : int -> t -> int option
(** [send_nonblocking_no_sigpipe sock ?pos ?len bstr] tries to send
    [len] bytes in bigstring [bstr] starting at position [pos] to socket
    [sock].  @return [Some bytes_sent] with the actual number of bytes
    sent, or [None] if the operation would have blocked.

    @raise [Invalid_argument] if the designated range is out of bounds.
    @raise [Unix_error] in the case of output errors.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)
#else
#warning "MSG_NOSIGNAL not defined; send_nonblocking_no_sigpipe"
#warning "not implemented."
#warning "Try compiling on Linux?"
#endif

val write : file_descr -> ?pos : int -> ?len : int -> t -> int
(** [write fd ?pos ?len bstr] writes [len]
    bytes in bigstring [bstr] starting at position [pos] to file
    descriptor [fd].  @return the number of bytes actually written.

    @raise [Unix_error] in the case of output errors.
    @raise [Invalid_argument] if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val write_assume_nonblocking :
  file_descr -> ?pos : int -> ?len : int -> t -> int
(** [write_assume_nonblocking fd ?pos ?len bstr] writes [len]
    bytes in bigstring [bstr] starting at position [pos] to file
    descriptor [fd] without yielding to other OCaml-threads.  @return the
    number of bytes actually written.

    @raise [Unix_error] in the case of output errors.
    @raise [Invalid_argument] if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)

val writev :
  file_descr -> ?count : int -> t Unix_ext.IOVec.t array -> int
(** [writev fd ?count iovecs] writes [count] [iovecs] of
    bigstrings to file descriptor [fd].  @return the number of bytes
    written.

    @raise [Unix_error] in the case of output errors.
    @raise [Invalid_argument] if count is out of range.

    @param count default = [Array.length iovecs]
*)

val writev_assume_nonblocking :
  file_descr -> ?count : int -> t Unix_ext.IOVec.t array -> int
(** [writev_assume_nonblocking fd ?count iovecs] writes [count]
    [iovecs] of bigstrings to file descriptor [fd] without yielding to
    other OCaml-threads.  @return the number of bytes actually written.

    @raise [Unix_error] in the case of output errors.
    @raise [Invalid_argument] if the designated range is out of bounds.

    @param count default = [Array.length iovecs]
*)

val output :
  ?min_len : int -> out_channel -> ?pos : int -> ?len : int -> t -> int
(** [output ?min_len oc ?pos ?len bstr] tries to output
    [len] bytes (guarantees to write at least [min_len] bytes (must be
    equal to or greater than zero), if possible, before returning) from
    bigstring [bstr] starting at position [pos] to output channel [oc].
    @return the number of bytes actually written.

    NOTE: you may need to flush [oc] to make sure that the data is
    actually sent.

    NOTE: if [len] characters fit into the channel buffer completely,
    they will be buffered.  Otherwise writes will be attempted until at
    least [min_len] characters have been sent.

    @raise [Invalid_argument] if the designated range is out of bounds.

    @raise [IOError] in the case of output errors.  The [IOError]-argument
    counting the number of successful bytes includes those that have
    been transferred to the channel buffer before the error.

    @param pos default = 0
    @param min_len default = 0
    @param len default = [length bstr - pos]
*)

val really_output :
  out_channel -> ?pos : int -> ?len : int -> t -> unit
(** [really_output oc ?pos ?len bstr] outputs exactly [len]
    bytes from bigstring [bstr] starting at position [pos] to output
    channel [oc].

    @raise [Invalid_argument] if the designated range is out of bounds.

    @raise [IOError] in the case of output errors.  The [IOError]-argument
    counting the number of successful bytes includes those that have
    been transferred to the channel buffer before the error.

    @param pos default = 0
    @param len default = [length bstr - pos]
*)


(** {6 Memory mapping} *)

val map_file : shared : bool -> file_descr -> int -> t
(** [map_file shared fd n] memory-maps [n] characters of the data
    associated with descriptor [fd] to a bigstring.  Iff [shared] is
    [true], all changes to the bigstring will be reflected in the file. *)


(** {6 File I/O}} *)

val load_file : ?pos : int -> ?len : int -> string -> t
(** [load_file ?pos ?len fname] loads [len] bytes of of file [fname]
    into a bigstring starting at position [pos].

    @raise [Invalid_argument] if the designated range is out of bounds.

    @param pos default = 0
    @param len default = [file_size - pos]
*)

val store_file :
      ?create : bool -> ?exclusive : bool -> ?append : bool -> ?perm : int ->
      string -> ?pos : int -> ?len : int -> t -> unit
(** [store_file ?create ?exclusive ?append ?perm fname ?pos ?len bstr]
    store [len] bytes starting at position [pos] of bigstring [bstr]
    in file [fname] using the appropriate flags for opening (creation,
    exclusivity, appending, permissions).

    @param create default = [true]
    @param exclusive default = [true]
    @param append default = [true]
    @param perm default = 0o600
    @param pos default = 0
    @param len default = [length bstr - pos]
*)


(** {6 Unsafe functions} *)

(* CRv2 sweeks: Why do we need to expose any of these?  Is there any situation
   where the performance of the bounds checks matters? 
   CRv2 mmottl: at least for blitting it seems worthwhile to expose the
   unsafe function.
*)

external unsafe_blit :
  src_pos : int -> src : t -> dst_pos : int -> dst : t -> len : int -> unit
  = "bigstring_blit_stub"
(** [unsafe_blit ~src_pos ~src ~dst_pos ~dst ~len] similar to
    {!Bigstring.blit}, but does not perform any bounds checks.  Will crash
    on bounds errors! *)

external unsafe_blit_string_bigstring :
  src_pos : int -> string -> dst_pos : int -> t -> len : int -> unit
  = "bigstring_blit_string_bigstring_stub" "noalloc"
(** [unsafe_blit_string_bigstring_stub ~src_pos str ~dst_pos bstr ~len]
    similar to {!Bigstring.blit_string_bigstring}, but does not perform
    any bounds checks.  Will crash on bounds errors! *)

external unsafe_blit_bigstring_string :
  src_pos : int -> t -> dst_pos : int -> string -> len : int -> unit
  = "bigstring_blit_bigstring_string_stub" "noalloc"
(** [unsafe_blit_bigstring_string ~src_pos bstr ~dst_pos str ~len]
    similar to {!Bigstring.blit_bigstring_string}, but does not perform
    any bounds checks.  Will crash on bounds errors! *)

external unsafe_read_assume_nonblocking :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_assume_nonblocking_stub"
(** [unsafe_read_assume_nonblocking fd ~pos ~len bstr]
    similar to {!Bigstring.read_assume_nonblocking}, but does
    not perform any bounds checks.  Will crash on bounds errors! *)

external unsafe_write :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_write_stub"
(** [unsafe_write fd ~pos ~len bstr] similar to
    {!Bigstring.write}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_write_assume_nonblocking :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_write_assume_nonblocking_stub"
(** [unsafe_write_assume_nonblocking fd ~pos ~len bstr]
    similar to {!Bigstring.write_assume_nonblocking}, but does
    not perform any bounds checks.  Will crash on bounds errors! *)

external unsafe_read :
  min_len : int -> file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_stub"
(** [unsafe_read ~min_len fd ~pos ~len bstr] similar to
    {!Bigstring.read}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_really_recv :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_recv_stub"
(** [unsafe_really_recv sock ~pos ~len bstr] similar to
    {!Bigstring.really_recv}, but does not perform any
    bounds checks.  Will crash on bounds errors! *)

external unsafe_input :
  min_len : int -> in_channel -> pos : int -> len : int -> t -> int
  = "bigstring_input_stub"
(** [unsafe_input ~min_len ic ~pos ~len bstr] similar to
    {!Bigstring.input}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_really_write :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_write_stub"
(** [unsafe_really_write fd ~pos ~len bstr] similar to
    {!Bigstring.write}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

#if defined(MSG_NOSIGNAL)
external unsafe_really_send_no_sigpipe :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_send_no_sigpipe_stub"
(** [unsafe_really_send_no_sigpipe sock ~pos ~len bstr]
    similar to {!Bigstring.send}, but does not perform any
    bounds checks.  Will crash on bounds errors! *)

external unsafe_send_nonblocking_no_sigpipe :
  file_descr -> pos : int -> len : int -> t -> int option
  = "bigstring_send_nonblocking_no_sigpipe_stub"
(** [unsafe_send_nonblocking_no_sigpipe sock ~pos ~len bstr] similar to
    {!Bigstring.send_nonblocking_no_sigpipe}, but does not perform any
    bounds checks.  Will crash on bounds errors! *)

#else
#warning "MSG_NOSIGNAL not defined; bigstring_send{,msg}_noblocking_no_sigpipe"
#warning "not implemented."
#warning "Try compiling on Linux?"
#endif

external unsafe_output :
  min_len : int -> out_channel -> pos : int -> len : int -> t -> int
  = "bigstring_output_stub"
(** [unsafe_output ~min_len oc ~pos ~len bstr] similar to
    {!Bigstring.output}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

external unsafe_writev :
  file_descr -> t Unix_ext.IOVec.t array -> int -> int
  = "bigstring_writev_stub"
(** [unsafe_writev fd iovecs count] similar to
    {!Bigstring.writev}, but does not perform any bounds checks.
    Will crash on bounds errors! *)

#if defined(MSG_NOSIGNAL)
external unsafe_sendmsg_nonblocking_no_sigpipe :
  file_descr -> t Unix_ext.IOVec.t array -> int -> int
  = "bigstring_sendmsg_nonblocking_no_sigpipe_stub"
(** [unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count]
    similar to {!Bigstring.sendmsg_nonblocking_no_sigpipe}, but
    does not perform any bounds checks.  Will crash on bounds errors! *)


val sendmsg_nonblocking_no_sigpipe :
  file_descr -> ?count : int -> t Unix_ext.IOVec.t array -> int
(** [sendmsg_nonblocking_no_sigpipe sock ?count iovecs] sends
    [count] [iovecs] of bigstrings to socket [sock] .  @return the
    number of bytes written.

    @raise [Unix_error] in the case of output errors.
    @raise [Invalid_argument] if count is out of range.

    @param count default = [Array.length iovecs]
*)
#else
#warning "MSG_NOSIGNAL not defined; bigstring_send{,msg}_noblocking_no_sigpipe"
#warning "not implemented."
#warning "Try compiling on Linux?"
#endif
