(*pp $(pwd)/pp.sh *)
(*
#include <unistd.h>
end-pp-include*)
open Printf
open Unix

external sync : unit -> unit = "unix_sync"
external fsync : file_descr -> unit = "unix_fsync"
external fdatasync : file_descr -> unit = "unix_fdatasync"
external dirfd : dir_handle -> file_descr = "unix_dirfd"
external readdir_ino : dir_handle -> string * int = "unix_readdir_ino_stub"
external unix_error : int -> string -> string -> 'a = "unix_error_stub"
external int_of_file_descr : file_descr -> int = "%identity"
external file_descr_of_int : int -> file_descr = "%identity"

external unsafe_read_assume_nonblocking :
  file_descr -> string -> pos : int -> len : int -> int
  = "unix_read_assume_nonblocking_stub"

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

let read_assume_nonblocking fd ?pos ?len buf =
  let loc = "read_assume_nonblocking" in
  let pos = get_opt_pos ~loc pos in
  let len = get_opt_len buf ~pos len in
  check_string_args ~loc buf ~pos ~len;
  unsafe_read_assume_nonblocking fd buf ~pos ~len

external unsafe_write_assume_nonblocking :
  file_descr -> string -> pos : int -> len : int -> int
  = "unix_write_assume_nonblocking_stub"

let write_assume_nonblocking fd ?pos ?len buf =
  let loc = "write_assume_nonblocking" in
  let pos = get_opt_pos ~loc pos in
  let len = get_opt_len buf ~pos len in
  check_string_args ~loc buf ~pos ~len;
  unsafe_write_assume_nonblocking fd buf ~pos ~len


(* Filesystem functions *)

external mknod :
  string -> file_kind -> int -> int -> int -> unit = "unix_mknod_stub"

let mknod
    ?(file_kind = S_REG) ?(perm = 0o600) ?(major = 0) ?(minor = 0) pathname =
  mknod pathname file_kind perm major minor


#if defined(_POSIX_MONOTONIC_CLOCK) && (_POSIX_MONOTONIC_CLOCK > -1)
(* Clock functions *)
type clock

external clock_gettime : clock -> float = "unix_clock_gettime"
external clock_settime : clock -> float -> unit = "unix_clock_settime"
external clock_getres : clock -> float = "unix_clock_getres"

external pthread_getcpuclockid :
  Thread.t -> clock = "unix_pthread_getcpuclockid"
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
end

external getrlimit : RLimit.resource -> RLimit.t = "unix_getrlimit"
external setrlimit : RLimit.resource -> RLimit.t -> unit = "unix_setrlimit"


(** {2 Resource usage} *)
module RUsage = struct
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

  let ru_utime ru = ru.ru_utime
  let ru_stime ru = ru.ru_stime
  let ru_maxrss ru = ru.ru_maxrss
  let ru_ixrss ru = ru.ru_ixrss
  let ru_idrss ru = ru.ru_idrss
  let ru_isrss ru = ru.ru_isrss
  let ru_minflt ru = ru.ru_minflt
  let ru_majflt ru = ru.ru_majflt
  let ru_nswap ru = ru.ru_nswap
  let ru_inblock ru = ru.ru_inblock
  let ru_oublock ru = ru.ru_oublock
  let ru_msgsnd ru = ru.ru_msgsnd
  let ru_msgrcv ru = ru.ru_msgrcv
  let ru_nsignals ru = ru.ru_nsignals
  let ru_nvcsw ru = ru.ru_nvcsw
  let ru_nivcsw ru = ru.ru_nivcsw

  let add ru1 ru2 = {
    ru_utime = ru1.ru_utime +. ru2.ru_utime;
    ru_stime = ru1.ru_stime +. ru2.ru_stime;
    ru_maxrss = Int64.add ru1.ru_maxrss ru2.ru_maxrss;
    ru_ixrss = Int64.add ru1.ru_ixrss ru2.ru_ixrss;
    ru_idrss = Int64.add ru1.ru_idrss ru2.ru_idrss;
    ru_isrss = Int64.add ru1.ru_isrss ru2.ru_isrss;
    ru_minflt = Int64.add ru1.ru_minflt ru2.ru_minflt;
    ru_majflt = Int64.add ru1.ru_majflt ru2.ru_majflt;
    ru_nswap = Int64.add ru1.ru_nswap ru2.ru_nswap;
    ru_inblock = Int64.add ru1.ru_inblock ru2.ru_inblock;
    ru_oublock = Int64.add ru1.ru_oublock ru2.ru_oublock;
    ru_msgsnd = Int64.add ru1.ru_msgsnd ru2.ru_msgsnd;
    ru_msgrcv = Int64.add ru1.ru_msgrcv ru2.ru_msgrcv;
    ru_nsignals = Int64.add ru1.ru_nsignals ru2.ru_nsignals;
    ru_nvcsw = Int64.add ru1.ru_nvcsw ru2.ru_nvcsw;
    ru_nivcsw = Int64.add ru1.ru_nivcsw ru2.ru_nivcsw;
  }
end

external getrusage : RUsage.who -> RUsage.t = "unix_getrusage"


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


(* POSIX thread functions *)

external mutex_timedlock : Mutex.t -> float -> bool = "unix_mutex_timedlock"

external condition_timedwait :
  Condition.t -> Mutex.t -> float -> bool = "unix_condition_timedwait"


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

external unsafe_writev_assume_nonblocking :
  file_descr -> string IOVec.t array -> int -> int
  = "unix_writev_assume_nonblocking_stub"

let writev_assume_nonblocking fd ?count iovecs =
  let count =
    match count with
    | None -> Array.length iovecs
    | Some count ->
        if count < 0 then invalid_arg "writev_assume_nonblocking: count < 0";
        let n_iovecs = Array.length iovecs in
        if count > n_iovecs then
          invalid_arg "writev_assume_nonblocking: count > n_iovecs";
        count
  in
  unsafe_writev_assume_nonblocking fd iovecs count

external pselect :
  file_descr list -> file_descr list -> file_descr list -> float -> int list ->
  file_descr list * file_descr list * file_descr list = "unix_pselect_stub"


(* Pathname resolution *)

external realpath : string -> string = "unix_realpath"

(* Temp dir creation *)

external mkdtemp : string -> string = "unix_mkdtemp"

(* Signal handling *)

external abort : unit -> 'a = "unix_abort" "noalloc"

(* User id, group id management *)

external initgroups : string -> int -> unit = "unix_initgroups"
