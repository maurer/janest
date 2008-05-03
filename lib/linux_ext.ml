(*pp cpp *)
open Unix
open Unix_ext

external send_fd :
  sock : file_descr -> fd_to_send : file_descr -> unit = "linux_send_fd_stub"

external recv_fd :
  sock : file_descr -> file_descr = "linux_recv_fd_stub"

external sendfile :
  sock : file_descr -> fd : file_descr -> pos : int -> len : int -> int
  = "linux_sendfile_stub"

let sendfile ?(pos = 0) ?len ~fd sock =
  let len =
    match len with
    | Some len -> len
    | None -> (fstat fd).st_size - pos
  in
  sendfile ~sock ~fd ~pos ~len

(**)

type tcp_bool_option =
  | TCP_CORK
  | TCP_NODELAY

external gettcpopt_bool :
  file_descr -> tcp_bool_option -> bool = "linux_gettcpopt_bool_stub"

external settcpopt_bool :
  file_descr -> tcp_bool_option -> bool -> unit = "linux_settcpopt_bool_stub"

(**)

external unsafe_send_nonblocking_no_sigpipe :
  file_descr -> pos : int -> len : int -> string -> int option
  = "linux_send_nonblocking_no_sigpipe_stub"

external unsafe_send_no_sigpipe :
  file_descr -> pos : int -> len : int -> string -> int
  = "linux_send_no_sigpipe_stub"

let check_send_args ?pos ?len buf =
  let str_len = String.length buf in
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
        if pos < 0 then invalid_arg "send_nonblocking_no_sigpipe: pos < 0";
        if pos > str_len then
          invalid_arg "send_nonblocking_no_sigpipe: pos > str_len";
        pos
  in
  let len =
    match len with
    | None -> str_len - pos
    | Some len ->
        if len < 0 then invalid_arg "send_nonblocking_no_sigpipe: pos < 0";
        if pos + len > str_len then
          invalid_arg "send_nonblocking_no_sigpipe: pos + len > str_len";
        len
  in
  (pos, len)

let send_nonblocking_no_sigpipe sock ?pos ?len buf =
  let (pos, len) = check_send_args ?pos ?len buf in
  unsafe_send_nonblocking_no_sigpipe sock ~pos ~len buf

let send_no_sigpipe sock ?pos ?len buf =
  let (pos, len) = check_send_args ?pos ?len buf in
  unsafe_send_no_sigpipe sock ~pos ~len buf

external unsafe_sendmsg_nonblocking_no_sigpipe :
  file_descr -> string IOVec.t array -> int -> int
  = "linux_sendmsg_nonblocking_no_sigpipe_stub"

let sendmsg_nonblocking_no_sigpipe sock ?count iovecs =
  let count =
    match count with
    | None -> Array.length iovecs
    | Some count ->
        if count < 0 then
          invalid_arg "sendmsg_nonblocking_no_sigpipe: count < 0";
        let n_iovecs = Array.length iovecs in
        if count > n_iovecs then
          invalid_arg "sendmsg_nonblocking_no_sigpipe: count > n_iovecs";
        count
  in
  unsafe_sendmsg_nonblocking_no_sigpipe sock iovecs count


(**)
#if defined(_POSIX_MONOTONIC_CLOCK) && (_POSIX_MONOTONIC_CLOCK > 0)
external get_clock_process_cputime_id :
  unit -> clock = "linux_clock_process_cputime_id_stub"
let clock_process_cputime_id = get_clock_process_cputime_id ()

external get_clock_thread_cputime_id :
  unit -> clock = "linux_clock_thread_cputime_id_stub"
let clock_thread_cputime_id = get_clock_thread_cputime_id ()
#else
#warning "POSIX MON not present; clock functions undefined"
#endif

(**)

external get_terminal_size : unit -> int * int = "linux_get_terminal_size_stub"

(**)

external pr_set_pdeathsig : int -> unit = "linux_pr_set_pdeathsig_stub"
external pr_get_pdeathsig : unit -> int = "linux_pr_get_pdeathsig_stub"

(**)

let file_descr_realpath fd =
  realpath ("/proc/self/fd/" ^ string_of_int (int_of_file_descr fd))

let out_channel_realpath oc = file_descr_realpath (descr_of_out_channel oc)
let in_channel_realpath ic = file_descr_realpath (descr_of_in_channel ic)


(* Epoll *)

module Epoll = struct
  (* Epoll flags *)

  type flag = IN | OUT | PRI | ERR | HUP | ET | ONESHOT
  type flags

  external make_flags : flag array -> flags = "linux_epoll_make_flags_stub"
  external get_flags : flags -> flag array = "linux_epoll_get_flags_stub"
  external has_in : flags -> bool = "linux_epoll_has_EPOLLIN_stub" "noalloc"
  external has_out : flags -> bool = "linux_epoll_has_EPOLLOUT_stub" "noalloc"
  external has_pri : flags -> bool = "linux_epoll_has_EPOLLPRI_stub" "noalloc"
  external has_err : flags -> bool = "linux_epoll_has_EPOLLERR_stub" "noalloc"
  external has_hup : flags -> bool = "linux_epoll_has_EPOLLHUP_stub" "noalloc"
  external has_et : flags -> bool = "linux_epoll_has_EPOLLET_stub" "noalloc"
  external has_oneshot :
    flags -> bool = "linux_epoll_has_EPOLLONESHOT_stub" "noalloc"

  let flag_to_string = function
    | IN -> "IN"
    | OUT -> "OUT"
    | PRI -> "PRI"
    | ERR -> "ERR"
    | HUP -> "HUP"
    | ET -> "ET"
    | ONESHOT -> "ONESHOT"

  (* Epoll functions *)

  external create : int -> file_descr = "linux_epoll_create_stub"

  external add : epfd : file_descr -> fd : file_descr -> flags -> unit
    = "linux_epoll_add_stub"

  external modify : epfd : file_descr -> fd : file_descr -> flags -> unit
    = "linux_epoll_modify_stub"

  external del :
    epfd : file_descr -> fd : file_descr -> unit = "linux_epoll_del_stub"

  external wait :
    file_descr -> maxevents : int -> timeout : int
    -> (file_descr * flags) array = "linux_epoll_wait_stub"
end


(* Splicing - zero-copies between kernel buffers *)

(* FIXME: not yet available with Fedora 5; the kernel supports this
   but not GLIBC.  Centos 5 seems to work fine with these functions.
   Uncomment this region and the corresponding stubs in linux_ext_stubs.c
   once the switch to Centos 5 is done.  STILL NEEDS TESTING!!!

module Splice = struct
  type flag =
    | MOVE
    | NONBLOCK
    | MORE
    | GIFT

  type flags

  external make_flags : flag array -> flags = "linux_splice_make_flags_stub"

  external unsafe_splice :
    bool ->
    fd_in : file_descr -> off_in : int ->
    fd_out : file_descr -> off_out : int ->
    len : int ->
    flags
    -> int * int * int = "linux_splice_stub_bc" "linux_splice_stub"

  let splice
        ?(assume_nonblocking = false)
        ~fd_in ?(off_in = 0)
        ~fd_out ?(off_out = 0) ~len flags =
    if off_in < 0 then invalid_arg "Splice.splice: off_in < 0";
    if off_out < 0 then invalid_arg "Splice.splice: off_out < 0";
    if len < 0 then invalid_arg "Splice.splice: len < 0";
    unsafe_splice assume_nonblocking ~fd_in ~off_in ~fd_out ~off_out ~len flags

  external unsafe_tee :
    bool -> fd_in : file_descr -> fd_out : file_descr -> int -> flags -> int
    = "linux_tee_stub"

  let tee ?(assume_nonblocking = false) ~fd_in ~fd_out len flags =
    if len < 0 then invalid_arg "Splice.splice: len < 0";
    unsafe_tee assume_nonblocking ~fd_in ~fd_out len flags

  external unsafe_vmsplice :
    bool -> file_descr -> int -> flags -> int = "linux_vmsplice_stub"

  let vmsplice ?(assume_nonblocking = false) fd iovecs ?count flags =
    let count =
      match count with
      | None -> Array.length iovecs
      | Some count ->
          if count < 0 then invalid_arg "Splice.vmsplice: count < 0";
          let n_iovecs = Array.length iovecs in
          if count > n_iovecs then
            invalid_arg "Splice.vmsplice: count > n_iovecs";
          count
    in
    unsafe_vmsplice assume_nonblocking fd count flags
end
*)
