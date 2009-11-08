(*pp $(pwd)/pp.sh *)
(*
#include <unistd.h>
end-pp-include*)
open Printf
open Unix

external sync : unit -> unit = "unix_sync"
external fsync : file_descr -> unit = "unix_fsync"
#if defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO > 0
external fdatasync : file_descr -> unit = "unix_fdatasync"
#else
#warning "_POSIX_SYNCHRONIZED_IO not net or <= 0, fdatasync not available"
#endif
external dirfd : dir_handle -> file_descr = "unix_dirfd"

external readdir_ino :
  dir_handle -> string * nativeint = "unix_readdir_ino_stub"



external unix_error : int -> string -> string -> _ = "unix_error_stub"
external int_of_file_descr : file_descr -> int = "%identity"
external file_descr_of_int : int -> file_descr = "%identity"

external exit_immediately : int -> _ = "caml_sys_exit"

external unsafe_read_assume_fd_is_nonblocking :
  file_descr -> string -> pos : int -> len : int -> int
    = "unix_read_assume_fd_is_nonblocking_stub"

let check_string_args ~loc str ~pos ~len =
  if pos < 0 then invalid_arg (loc ^ ": pos < 0");
  if len < 0 then invalid_arg (loc ^ ": len < 0");
  let str_len = String.length str in
  if str_len < pos + len then
    invalid_arg (sprintf "Unix_ext.%s: length(str) < pos + len" loc)

let get_opt_pos ~loc = function
  | Some pos ->
      if pos < 0 then invalid_arg (sprintf "Unix_ext.%s: pos < 0" loc);
      pos
  | None -> 0

let get_opt_len str ~pos = function
  | Some len -> len
  | None -> String.length str - pos

let read_assume_fd_is_nonblocking fd ?pos ?len buf =
  let loc = "read_assume_fd_is_nonblocking" in
  let pos = get_opt_pos ~loc pos in
  let len = get_opt_len buf ~pos len in
  check_string_args ~loc buf ~pos ~len;
  unsafe_read_assume_fd_is_nonblocking fd buf ~pos ~len

external unsafe_write_assume_fd_is_nonblocking :
  file_descr -> string -> pos : int -> len : int -> int
    = "unix_write_assume_fd_is_nonblocking_stub"

let write_assume_fd_is_nonblocking fd ?pos ?len buf =
  let loc = "write_assume_fd_is_nonblocking" in
  let pos = get_opt_pos ~loc pos in
  let len = get_opt_len buf ~pos len in
  check_string_args ~loc buf ~pos ~len;
  unsafe_write_assume_fd_is_nonblocking fd buf ~pos ~len


(* Filesystem functions *)

external mknod :
  string -> file_kind -> int -> int -> int -> unit = "unix_mknod_stub"

let mknod
    ?(file_kind = S_REG) ?(perm = 0o600) ?(major = 0) ?(minor = 0) pathname =
  mknod pathname file_kind perm major minor

#if defined(_POSIX_MONOTONIC_CLOCK) && (_POSIX_MONOTONIC_CLOCK > -1)
(* Clock functions *)
module Clock = struct
  type t

  external get_time : t -> float = "unix_clock_gettime"
  external set_time : t -> float -> unit = "unix_clock_settime"
  external get_resolution : t -> float = "unix_clock_getres"
  external get : Thread.t -> t = "unix_pthread_getcpuclockid"
end
#else
#warning "POSIX MON not present; clock functions undefined"
#endif

(* Resource limits *)

module RLimit = struct
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

  external get : resource -> t = "unix_getrlimit"
  external set : resource -> t -> unit = "unix_setrlimit"
end


(** {2 Resource usage} -- For details, "man getrusage" *)
module Resource_usage = struct
  type who = SELF | CHILDREN

  type t = {
    utime : float;
    stime : float;
    maxrss : int64;
    ixrss : int64;
    idrss : int64;
    isrss : int64;
    minflt : int64;
    majflt : int64;
    nswap : int64;
    inblock : int64;
    oublock : int64;
    msgsnd : int64;
    msgrcv : int64;
    nsignals : int64;
    nvcsw : int64;
    nivcsw : int64;
  }

  external getrusage : who -> t = "unix_getrusage"
  let get who = getrusage who

  let utime t = t.utime
  let stime t = t.stime
  let maxrss t = t.maxrss
  let ixrss t = t.ixrss
  let idrss t = t.idrss
  let isrss t = t.isrss
  let minflt t = t.minflt
  let majflt t = t.majflt
  let nswap t = t.nswap
  let inblock t = t.inblock
  let oublock t = t.oublock
  let msgsnd t = t.msgsnd
  let msgrcv t = t.msgrcv
  let nsignals t = t.nsignals
  let nvcsw t = t.nvcsw
  let nivcsw t = t.nivcsw

  let add t1 t2 = {
    utime = t1.utime +. t2.utime;
    stime = t1.stime +. t2.stime;
    maxrss = Int64.add t1.maxrss t2.maxrss;
    ixrss = Int64.add t1.ixrss t2.ixrss;
    idrss = Int64.add t1.idrss t2.idrss;
    isrss = Int64.add t1.isrss t2.isrss;
    minflt = Int64.add t1.minflt t2.minflt;
    majflt = Int64.add t1.majflt t2.majflt;
    nswap = Int64.add t1.nswap t2.nswap;
    inblock = Int64.add t1.inblock t2.inblock;
    oublock = Int64.add t1.oublock t2.oublock;
    msgsnd = Int64.add t1.msgsnd t2.msgsnd;
    msgrcv = Int64.add t1.msgrcv t2.msgrcv;
    nsignals = Int64.add t1.nsignals t2.nsignals;
    nvcsw = Int64.add t1.nvcsw t2.nvcsw;
    nivcsw = Int64.add t1.nivcsw t2.nivcsw;
  }
end



(* System configuration *)

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

#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)
(* POSIX thread functions *)
external mutex_timedlock : Mutex.t -> float -> bool = "unix_mutex_timedlock"
#else
#warning "POSIX TMO not present; unix_mutex_timedlock undefined"
#endif

external condition_timedwait :
  Condition.t -> Mutex.t -> float -> bool = "unix_condition_timedwait"

external create_error_checking_mutex :
  unit -> Mutex.t = "unix_create_error_checking_mutex"


(* I/O vectors *)

module IOVec = struct
  open Bigarray

  (* NOTE: DO NOT CHANGE THE MEMORY LAYOUT OF THIS TYPE!!! *)
  type 'buf t =
    {
      buf : 'buf;
      pos : int;
      len : int;
    }

  type 'buf kind = 'buf

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  let string_kind = ""
  let bigstring_kind = Array1.create Bigarray.char c_layout 0

  let empty kind =
    {
      buf = kind;
      pos = 0;
      len = 0;
    }

  let get_iovec loc ?pos ?len true_len buf =
    let pos =
      match pos with
      | None -> 0
      | Some pos ->
          if pos < 0 then invalid_arg (loc ^ ": pos < 0");
          pos
    in
    let len =
      match len with
      | None -> true_len
      | Some len ->
          if len < 0 then invalid_arg (loc ^ ": len < 0");
          len
    in
    if pos + len > true_len then invalid_arg (loc ^ ": pos + len > length buf");
    {
      buf = buf;
      pos = pos;
      len = len;
    }

  let of_string ?pos ?len str =
    let str_len = String.length str in
    get_iovec "IOVec.of_string" ?pos ?len str_len str

  let of_bigstring ?pos ?len bstr =
    let bstr_len = Array1.dim bstr in
    get_iovec "IOVec.of_bigstring" ?pos ?len bstr_len bstr

  let drop iovec n =
    if n > iovec.len then failwith "IOVec.drop: n > length iovec"
    else
      {
        buf = iovec.buf;
        pos = iovec.pos + n;
        len = iovec.len - n;
      }

  let max_iovecs =
    let n64 = sysconf IOV_MAX in
    if n64 > Int64.of_int Sys.max_array_length then Sys.max_array_length
    else Int64.to_int n64
end

let get_iovec_count loc iovecs = function
  | None -> Array.length iovecs
  | Some count ->
      if count < 0 then invalid_arg (loc ^ ": count < 0");
      let n_iovecs = Array.length iovecs in
      if count > n_iovecs then invalid_arg (loc ^ ": count > n_iovecs");
      count

external unsafe_writev_assume_fd_is_nonblocking :
  file_descr -> string IOVec.t array -> int -> int
  = "unix_writev_assume_fd_is_nonblocking_stub"

let writev_assume_fd_is_nonblocking fd ?count iovecs =
  let count = get_iovec_count "writev_assume_fd_is_nonblocking" iovecs count in
  unsafe_writev_assume_fd_is_nonblocking fd iovecs count

external unsafe_writev :
  file_descr -> string IOVec.t array -> int -> int = "unix_writev_stub"

let writev fd ?count iovecs =
  let count = get_iovec_count "writev" iovecs count in
  unsafe_writev fd iovecs count

external pselect :
  file_descr list -> file_descr list -> file_descr list -> float -> int list ->
  file_descr list * file_descr list * file_descr list = "unix_pselect_stub"


(* Pathname resolution *)

external realpath : string -> string = "unix_realpath"


(* Temporary file and directory creation *)

external mkstemp : string -> string * Unix.file_descr = "unix_mkstemp"
external mkdtemp : string -> string = "unix_mkdtemp"


(* Signal handling *)

external abort : unit -> 'a = "unix_abort" "noalloc"


(* User id, group id management *)

external initgroups : string -> int -> unit = "unix_initgroups"


(** Globbing and shell word expansion *)

module Fnmatch_flags = struct
  type internal =
    | NOESCAPE
    | PATHNAME
    | PERIOD
    | FILE_NAME
    | LEADING_DIR
    | CASEFOLD

  type flag = [
    | `NOESCAPE
    | `PATHNAME
    | `PERIOD
    | `FILE_NAME
    | `LEADING_DIR
    | `CASEFOLD
  ]

  
  
  let flag_to_internal = function
    | `NOESCAPE -> NOESCAPE
    | `PATHNAME -> PATHNAME
    | `PERIOD -> PERIOD
    | `FILE_NAME -> FILE_NAME
    | `LEADING_DIR -> LEADING_DIR
    | `CASEFOLD -> CASEFOLD

  type t = int32

  external internal_make : internal array -> t = "unix_fnmatch_make_flags"

  let make flags =
    internal_make (Array.map flag_to_internal (Array.of_list flags))
end

external fnmatch :
  Fnmatch_flags.t -> pat : string -> string -> bool = "unix_fnmatch"

let fnmatch ?(flags = Int32.zero) ~pat fname = fnmatch flags ~pat fname

module Wordexp_flags = struct
  type internal = NOCMD | SHOWERR | UNDEF
  type flag = [ `NOCMD | `SHOWERR | `UNDEF ]

  
  let flag_to_internal = function
    | `NOCMD -> NOCMD
    | `SHOWERR -> SHOWERR
    | `UNDEF -> UNDEF

  type t = int32

  external internal_make : internal array -> t = "unix_wordexp_make_flags"

  let make flags =
    internal_make (Array.map flag_to_internal (Array.of_list flags))
end

external wordexp : Wordexp_flags.t -> string -> string array = "unix_wordexp"

let wordexp ?(flags = Int32.zero) str = wordexp flags str


#if defined(__linux__)
(** {2 Additional IP functionality} *)

external if_indextoname : int -> string = "unix_if_indextoname"

external mcast_join :
  ?ifname : string -> file_descr -> sockaddr -> unit = "unix_mcast_join"

external mcast_leave :
  ?ifname : string -> file_descr -> sockaddr -> unit = "unix_mcast_leave"
#else
#warning "not on linux, multicast stuff not included"
#endif
