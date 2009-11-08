(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)


TYPE_CONV_PATH "Linux_ext"

open Unix
open Unix_ext

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

(* Raw result of sysinfo syscall *)
module Raw_sysinfo = struct
  type t = {
    uptime : int;
    load1 : int;
    load5 : int;
    load15 : int;
    total_ram : int;
    free_ram : int;
    shared_ram : int;
    buffer_ram : int;
    total_swap : int;
    free_swap : int;
    procs : int;
    totalhigh : int;
    freehigh : int;
    mem_unit : int;
  }
end

(* Result of sysinfo syscall *)
type sysinfo = {
  uptime : Time.Span.t;
  load1 : int;
  load5 : int;
  load15 : int;
  total_ram : int;
  free_ram : int;
  shared_ram : int;
  buffer_ram : int;
  total_swap : int;
  free_swap : int;
  procs : int;
  totalhigh : int;
  freehigh : int;
  mem_unit : int;
} with sexp, bin_io

external raw_sysinfo : unit -> Raw_sysinfo.t = "linux_sysinfo"

let sysinfo () =
  let raw = raw_sysinfo () in
  {
    uptime = Time.Span.of_int_sec raw.Raw_sysinfo.uptime;
    load1 = raw.Raw_sysinfo.load1;
    load5 = raw.Raw_sysinfo.load5;
    load15 = raw.Raw_sysinfo.load15;
    total_ram = raw.Raw_sysinfo.total_ram;
    free_ram = raw.Raw_sysinfo.free_ram;
    shared_ram = raw.Raw_sysinfo.shared_ram;
    buffer_ram = raw.Raw_sysinfo.buffer_ram;
    total_swap = raw.Raw_sysinfo.total_swap;
    free_swap = raw.Raw_sysinfo.free_swap;
    procs = raw.Raw_sysinfo.procs;
    totalhigh = raw.Raw_sysinfo.totalhigh;
    freehigh = raw.Raw_sysinfo.freehigh;
    mem_unit = raw.Raw_sysinfo.mem_unit;
  }

(* If you update this type, you also must update linux_tcpopt_bool,
   in the C stubs. (And do make sure you get the order correct) *)
type tcp_bool_option = TCP_CORK with sexp, bin_io

external gettcpopt_bool :
  file_descr -> tcp_bool_option -> bool = "linux_gettcpopt_bool_stub"

external settcpopt_bool :
  file_descr -> tcp_bool_option -> bool -> unit = "linux_settcpopt_bool_stub"

(**)

external unsafe_send_nonblocking_no_sigpipe :
  file_descr -> pos : int -> len : int -> string -> int
  = "linux_send_nonblocking_no_sigpipe_stub"

let unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf =
  let res = unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf in
  if res = -1 then None
  else Some res

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

let unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count =
  let res = unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count in
  if res = -1 then None
  else Some res

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

external get_clock_process_cputime_id :
  unit -> Clock.t = "linux_clock_process_cputime_id_stub"
let clock_process_cputime_id = get_clock_process_cputime_id ()

external get_clock_thread_cputime_id :
  unit -> Clock.t = "linux_clock_thread_cputime_id_stub"
let clock_thread_cputime_id = get_clock_thread_cputime_id ()

(**)

external get_terminal_size : unit -> int * int = "linux_get_terminal_size_stub"

(**)

external pr_set_pdeathsig : Signal.t -> unit = "linux_pr_set_pdeathsig_stub"
external pr_get_pdeathsig : unit -> Signal.t = "linux_pr_get_pdeathsig_stub"

(**)

let file_descr_realpath fd =
  realpath ("/proc/self/fd/" ^ string_of_int (int_of_file_descr fd))

let out_channel_realpath oc = file_descr_realpath (descr_of_out_channel oc)
let in_channel_realpath ic = file_descr_realpath (descr_of_in_channel ic)
