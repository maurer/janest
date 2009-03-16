(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` pa_type_conv.cmo pa_sexp_conv.cmo *)
(* CRv2 sweeks: expose the sexpable stuff in the mli *)

(* Core_unix wraps the standard unix functions with an exception handler that
   inserts an informative string in the third field of Unix_error.  The problem
   with the standard Unix_error that gets raised is that it doesn't include
   information about the arguments to the function that failed.
*)

(* CR ogunden: this seems like a lot of copied code that is a pain to maintain. *)

TYPE_CONV_PATH "Core_unix"

module Unix = Caml.UnixLabels
module Int = Core_int
module Int64 = Core_int64
module Sexp = Sexplib.Sexp
module List = ListLabels

open Unix
open Sexplib.Conv

module File_descr = struct
  type t = Unix.file_descr
  let to_int t = Unix_ext.int_of_file_descr t
  let sexp_of_t t = Int.sexp_of_t (to_int t)
end

let failwithf = Core_printf.failwithf

let atom x = Sexp.Atom x
let list x = Sexp.List x

let record l =
  list (List.map l ~f:(fun (name, value) -> list [atom name; value]))

let improve f make_arg_sexps =
  try f () with
  | Unix_error (e, s, _) ->
      raise (Unix_error (e, s, Sexp.to_string_hum (record (make_arg_sexps ()))))
;;

let dirname_r filename = ("dirname", atom filename)
let filename_r filename = ("filename", atom filename)
let file_perm_r perm = ("perm", atom (Printf.sprintf "0o%o" perm))
let len_r len = ("len", Int.sexp_of_t len)
let uid_r uid = ("uid", Int.sexp_of_t uid)
let gid_r gid = ("gid", Int.sexp_of_t gid)
let fd_r fd = ("fd", File_descr.sexp_of_t fd)
let dir_handle_r handle =
  ("dir_handle", File_descr.sexp_of_t (Unix_ext.dirfd handle))
;;

let unary make_r f x = improve (fun () -> f x) (fun () -> [make_r x])
let unary_fd f = unary fd_r f
let unary_filename f = unary filename_r f
let unary_dirname f = unary dirname_r f
let unary_dir_handle f = unary dir_handle_r f

type error =
Unix.error =
| E2BIG | EACCES | EAGAIN | EBADF | EBUSY | ECHILD | EDEADLK | EDOM | EEXIST
| EFAULT | EFBIG | EINTR | EINVAL | EIO | EISDIR | EMFILE | EMLINK
| ENAMETOOLONG | ENFILE | ENODEV | ENOENT | ENOEXEC | ENOLCK | ENOMEM | ENOSPC
| ENOSYS | ENOTDIR | ENOTEMPTY | ENOTTY | ENXIO | EPERM | EPIPE | ERANGE
| EROFS | ESPIPE | ESRCH | EXDEV | EWOULDBLOCK | EINPROGRESS | EALREADY
| ENOTSOCK | EDESTADDRREQ | EMSGSIZE | EPROTOTYPE | ENOPROTOOPT
| EPROTONOSUPPORT | ESOCKTNOSUPPORT | EOPNOTSUPP | EPFNOSUPPORT | EAFNOSUPPORT
| EADDRINUSE | EADDRNOTAVAIL | ENETDOWN | ENETUNREACH | ENETRESET
| ECONNABORTED | ECONNRESET | ENOBUFS | EISCONN | ENOTCONN | ESHUTDOWN
| ETOOMANYREFS | ETIMEDOUT | ECONNREFUSED | EHOSTDOWN | EHOSTUNREACH | ELOOP
| EOVERFLOW | EUNKNOWNERR of int

exception Unix_error = Unix.Unix_error

let error_message = error_message
let handle_unix_error = handle_unix_error
let environment = environment

let getenv var = try Some (Sys.getenv var) with Not_found -> None

let getenv_exn var =
  match getenv var with
  | Some x -> x
  | None -> failwithf "Sys.getenv_exn: environment variable %s is not set" var ()
;;

let putenv name value =
  improve (fun () -> putenv name value)
    (fun () -> [("name", atom name); ("value", atom value)])
;;

type process_status =
Unix.process_status =
| WEXITED of int
| WSIGNALED of int
| WSTOPPED of int

type wait_flag =
Unix.wait_flag =
| WNOHANG 
| WUNTRACED
with sexp_of

let prog_r prog = ("prog", atom prog)
let args_r args = ("args", sexp_of_array atom args)
let env_r env = ("env", sexp_of_array atom env)

let execv ~prog ~args =
  improve (fun () -> execv ~prog ~args)
    (fun () -> [prog_r prog; args_r args])
;;

let execve ~prog ~args ~env =
  improve (fun () -> execve ~prog ~args ~env)
    (fun () -> [prog_r prog; args_r args; env_r env])
;;

let execvp ~prog ~args =
  improve (fun () -> execvp ~prog ~args)
    (fun () -> [prog_r prog; args_r args])
;;

let execvpe ~prog ~args ~env =
  improve (fun () -> execvpe ~prog ~args ~env)
    (fun () -> [prog_r prog; args_r args; env_r env])
;;

let fork = fork
let wait = wait

let waitpid ~mode pid =
  improve (fun () -> waitpid ~mode pid)
    (fun () ->
      [("mode", sexp_of_list sexp_of_wait_flag mode);
       ("pid", Int.sexp_of_t pid)])
;;

let system s = improve (fun () -> system s) (fun () -> [("command", atom s)])

let getpid = getpid
let getppid = getppid
let nice i =
  improve (fun () -> nice i)
    (fun () -> [("priority", Int.sexp_of_t i)])
;;

type file_descr = Unix.file_descr

let stdin = stdin
let stdout = stdout
let stderr = stderr

type open_flag =
Unix.open_flag =
| O_RDONLY
| O_WRONLY
| O_RDWR
| O_NONBLOCK
| O_APPEND
| O_CREAT
| O_TRUNC
| O_EXCL
| O_NOCTTY
| O_DSYNC
| O_SYNC
| O_RSYNC
with sexp_of

type file_perm = int

let openfile filename ~mode ~perm =
  improve (fun () -> openfile filename ~mode ~perm)
    (fun () -> [filename_r filename;
                ("mode", sexp_of_list sexp_of_open_flag mode);
                file_perm_r perm])
;;

let close = unary_fd close

let read_write f fd ~buf ~pos ~len =
  improve (fun () -> f fd ~buf ~pos ~len)
    (fun () -> [fd_r fd; ("pos", Int.sexp_of_t pos); len_r len])
;;

let read = read_write read
let write = read_write write
let single_write = read_write single_write

let in_channel_of_descr = in_channel_of_descr
let out_channel_of_descr = out_channel_of_descr
let descr_of_in_channel = descr_of_in_channel
let descr_of_out_channel = descr_of_out_channel

type seek_command =
Unix.seek_command =
| SEEK_SET
| SEEK_CUR
| SEEK_END
with sexp_of

let lseek fd pos ~mode =
  improve (fun () -> lseek fd pos ~mode)
    (fun () -> [fd_r fd;
                ("pos", Int.sexp_of_t pos);
                ("mode", sexp_of_seek_command mode)])
;;

let truncate filename ~len =
  improve (fun () -> truncate filename ~len)
    (fun () -> [filename_r filename; len_r len])
;;

let ftruncate fd ~len =
  improve (fun () -> ftruncate fd ~len) (fun () -> [fd_r fd; len_r len])
;;

type file_kind = Unix.file_kind =
| S_REG
| S_DIR
| S_CHR
| S_BLK
| S_LNK
| S_FIFO
| S_SOCK

type stats =
Unix.stats = {
  st_dev : int;
  st_ino : int;
  st_kind : file_kind;
  st_perm : file_perm;
  st_nlink : int;
  st_uid : int;
  st_gid : int;
  st_rdev : int;
  st_size : int;
  st_atime : float;
  st_mtime : float;
  st_ctime : float;
}

let stat = unary_filename stat
let lstat = unary_filename lstat
let fstat = unary_fd fstat
let isatty = unary_fd isatty

module LargeFile = struct
  open LargeFile

  let lseek fd pos ~mode =
    improve (fun () -> lseek fd pos ~mode)
      (fun () -> [fd_r fd;
                  ("pos", Int64.sexp_of_t pos);
                  ("mode", sexp_of_seek_command mode)])
  ;;

  let len_r len = ("len", Int64.sexp_of_t len)
    
  let truncate filename ~len =
    improve (fun () -> truncate filename ~len)
      (fun () -> [filename_r filename; len_r len])
  ;;

  let ftruncate fd ~len =
    improve (fun () -> ftruncate fd ~len) (fun () -> [fd_r fd; len_r len])
  ;;

  type stats =
  Unix.LargeFile.stats = {
    st_dev : int;
    st_ino : int;
    st_kind : file_kind;
    st_perm : file_perm;
    st_nlink : int;
    st_uid : int;
    st_gid : int;
    st_rdev : int;
    st_size : int64;
    st_atime : float;
    st_mtime : float;
    st_ctime : float;
  }

  let stat = unary_filename stat
  let lstat = unary_filename lstat
  let fstat = unary_fd fstat
end

let src_dst f ~src ~dst =
  improve (fun () -> f ~src ~dst)
    (fun () -> [("src", atom src); ("dst", atom dst)])
;;

let unlink = unary_filename unlink
let rename = src_dst rename
let link = src_dst link

type access_permission = Unix.access_permission =
| R_OK
| W_OK
| X_OK
| F_OK
  with sexp_of

let chmod filename ~perm =
  improve (fun () -> chmod filename ~perm)
    (fun () -> [filename_r filename; file_perm_r perm])
;;

let fchmod fd ~perm =
  improve (fun () -> fchmod fd ~perm)
    (fun () -> [fd_r fd; file_perm_r perm])
;;

let chown filename ~uid ~gid = 
  improve (fun () -> chown filename ~uid ~gid)
    (fun () -> [filename_r filename; uid_r uid; gid_r gid])
;;

let fchown fd ~uid ~gid =
  improve (fun () -> fchown fd ~uid ~gid)
    (fun () -> [fd_r fd; uid_r uid; gid_r gid])
;;

let umask mode =
  improve (fun () -> umask mode)
    (fun () -> [("mode", atom (Printf.sprintf "0o%o" mode))])
;;

let access filename ~perm =
  improve (fun () -> access filename ~perm)
    (fun () -> [filename_r filename;
                ("perm", sexp_of_list sexp_of_access_permission perm)])
;;

let dup = unary_fd dup

let dup2 ~src ~dst =
  improve (fun () -> dup2 ~src ~dst)
    (fun () -> [("src", File_descr.sexp_of_t src);
                ("dst", File_descr.sexp_of_t dst)])
;;

let set_nonblock = unary_fd set_nonblock
let clear_nonblock = unary_fd clear_nonblock
let set_close_on_exec = unary_fd set_close_on_exec
let clear_close_on_exec = unary_fd clear_close_on_exec

let mkdir dirname ~perm =
  improve (fun () -> mkdir dirname ~perm)
    (fun () -> [dirname_r dirname; file_perm_r perm])
;;
let rmdir = unary_dirname rmdir
let chdir = unary_dirname chdir
let getcwd = getcwd
let chroot = unary_dirname chroot

type dir_handle = Unix.dir_handle

let opendir = unary_dirname opendir
let readdir = unary_dir_handle readdir
let rewinddir = unary_dir_handle rewinddir
let closedir = unary_dir_handle closedir

let pipe = pipe

let mkfifo name ~perm =
  improve (fun () -> mkfifo name ~perm)
    (fun () -> [("name", atom name); file_perm_r perm])
;;

let create_process ~prog ~args ~stdin ~stdout ~stderr =
  improve (fun () -> create_process ~prog ~args ~stdin ~stdout ~stderr)
    (fun () ->
      [("prog", atom prog);
       ("args", sexp_of_array atom args);
       ("stdin", File_descr.sexp_of_t stdin);
       ("stdout", File_descr.sexp_of_t stdout);
       ("stderr", File_descr.sexp_of_t stderr)])
;;

let create_process_env ~prog ~args ~env ~stdin ~stdout ~stderr =
  improve (fun () -> create_process ~prog ~args ~stdin ~stdout ~stderr)
    (fun () ->
      [("prog", atom prog);
       ("args", sexp_of_array atom args);
       ("env", sexp_of_array atom env);
       ("stdin", File_descr.sexp_of_t stdin);
       ("stdout", File_descr.sexp_of_t stdout);
       ("stderr", File_descr.sexp_of_t stderr)])
;;

let make_open_process f command =
  improve (fun () -> f command)
    (fun () -> [("command", atom command)])

let open_process_in = make_open_process open_process_in
let open_process_out = make_open_process open_process_out
let open_process = make_open_process open_process

let open_process_full command ~env =
  improve (fun () -> open_process_full command ~env)
    (fun () -> [("command", atom command);
                ("env", sexp_of_array atom env)])
;;

let close_process_in = close_process_in
let close_process_out = close_process_out
let close_process = close_process
let close_process_full = close_process_full

let symlink = src_dst symlink
let readlink = unary_filename readlink

let select ~read ~write ~except ~timeout =
  improve (fun () -> select ~read ~write ~except ~timeout)
    (fun () ->
      [("read", sexp_of_list File_descr.sexp_of_t read);
       ("write", sexp_of_list File_descr.sexp_of_t write);
       ("except", sexp_of_list File_descr.sexp_of_t except);
       ("timeout", sexp_of_float timeout)])
;;

type lock_command =
Unix.lock_command =
| F_ULOCK
| F_LOCK
| F_TLOCK
| F_TEST
| F_RLOCK
| F_TRLOCK
with sexp_of

let lockf fd ~mode ~len =
  improve (fun () -> lockf fd ~mode ~len)
    (fun () -> [fd_r fd; ("mode", sexp_of_lock_command mode); len_r len])
;;

let kill ~pid ~signal =
  improve (fun () -> kill ~pid ~signal)
    (fun () -> [("pid", Int.sexp_of_t pid);
                ("signal", Int.sexp_of_t signal)])
;;

type sigprocmask_command =
Unix.sigprocmask_command = 
| SIG_SETMASK
| SIG_BLOCK
| SIG_UNBLOCK
with sexp_of

let sigs_r sigs = ("sigs", sexp_of_list Int.sexp_of_t sigs)

let sigprocmask ~mode sigs =
  improve (fun () -> sigprocmask ~mode sigs)
    (fun () -> [("mode", sexp_of_sigprocmask_command mode); sigs_r sigs])
;;

let sigpending = sigpending

let sigsuspend sigs =
  improve (fun () -> sigsuspend sigs) (fun () -> [sigs_r sigs])
;;

let pause = pause

type process_times =
Unix.process_times = {
  tms_utime : float;
  tms_stime : float;
  tms_cutime : float;
  tms_cstime : float;
}

type tm =
Unix.tm = {
  tm_sec : int;
  tm_min : int;
  tm_hour : int;
  tm_mday : int;
  tm_mon : int;
  tm_year : int;
  tm_wday : int;
  tm_yday : int;
  tm_isdst : bool;
}

let time = time
let gettimeofday = gettimeofday
let gmtime = gmtime
let localtime = localtime
let mktime = mktime
let alarm = alarm
let sleep = sleep
let times = times
let utimes = utimes

type interval_timer =
Unix.interval_timer =
| ITIMER_REAL 
| ITIMER_VIRTUAL
| ITIMER_PROF

type interval_timer_status =
Unix.interval_timer_status = {
  it_interval : float;
  it_value : float;
}

let getitimer = getitimer
let setitimer = setitimer

let getuid = getuid
let geteuid = geteuid
let setuid uid =
  improve (fun () -> setuid uid) (fun () -> [("uid", Int.sexp_of_t uid)])
;;
let getgid = getgid
let getegid = getegid
let setgid gid =
  improve (fun () -> setgid gid) (fun () -> [("gid", Int.sexp_of_t gid)])
;;
let getgroups = getgroups

type passwd_entry =
Unix.passwd_entry = {
  pw_name : string;
  pw_passwd : string;
  pw_uid : int;
  pw_gid : int;
  pw_gecos : string;
  pw_dir : string;
  pw_shell : string;
}

type group_entry =
Unix.group_entry = {
  gr_name : string;
  gr_passwd : string;
  gr_gid : int;
  gr_mem : string array;
}

let getlogin = getlogin

let getpwnam name =
  improve (fun () -> getpwnam name) (fun () -> [("name", atom name)])
;;

let getgrnam name =
  improve (fun () -> getgrnam name) (fun () -> [("name", atom name)])
;;

let getpwuid uid =
  improve (fun () -> getpwuid uid) (fun () -> [("uid", Int.sexp_of_t uid)])
;;

let getgrgid gid =
  improve (fun () -> getgrgid gid) (fun () -> [("gid", Int.sexp_of_t gid)])
;;

type inet_addr = Unix.inet_addr
let inet_addr_of_string = inet_addr_of_string
let string_of_inet_addr = string_of_inet_addr
let inet_addr_any = inet_addr_any
let inet_addr_loopback = inet_addr_loopback
let inet6_addr_any = inet6_addr_any
let inet6_addr_loopback = inet6_addr_loopback

let sexp_of_inet_addr addr = Sexp.Atom (string_of_inet_addr addr)

type socket_domain = Unix.socket_domain =
| PF_UNIX
| PF_INET
| PF_INET6
with sexp_of

type socket_type = Unix.socket_type =
| SOCK_STREAM
| SOCK_DGRAM
| SOCK_RAW
| SOCK_SEQPACKET
with sexp_of

type sockaddr = Unix.sockaddr = 
| ADDR_UNIX of string
| ADDR_INET of inet_addr * int
with sexp_of

let domain_of_sockaddr = domain_of_sockaddr

let addr_r addr = ("addr", sexp_of_sockaddr addr)

let socket_or_pair f ~domain ~kind ~protocol =
  improve (fun () -> f ~domain ~kind ~protocol)
    (fun () -> [("domain", sexp_of_socket_domain domain);
                ("kind", sexp_of_socket_type kind);
                ("protocol", Int.sexp_of_t protocol)])
;;

let socket = socket_or_pair socket
let socketpair = socket_or_pair socketpair

let accept = unary_fd accept

let bind fd ~addr =
  improve (fun () -> bind fd ~addr) (fun () -> [fd_r fd; addr_r addr])
;;

let connect fd ~addr =
  improve (fun () -> connect fd ~addr) (fun () -> [fd_r fd; addr_r addr])
;;

let listen fd ~max =
  improve (fun () -> listen fd ~max)
    (fun () -> [fd_r fd; ("max", Int.sexp_of_t max)])
;;

type shutdown_command = Unix.shutdown_command =
| SHUTDOWN_RECEIVE
| SHUTDOWN_SEND
| SHUTDOWN_ALL
with sexp_of

let shutdown fd ~mode =
  improve (fun () -> shutdown fd ~mode)
    (fun () -> [fd_r fd; ("mode", sexp_of_shutdown_command mode)])
;;

let getsockname = unary_fd getsockname

let getpeername = unary_fd getpeername

type msg_flag =
Unix.msg_flag = 
| MSG_OOB
| MSG_DONTROUTE
| MSG_PEEK
with sexp_of

let recv_send f fd ~buf ~pos ~len ~mode =
  improve (fun () -> f fd ~buf ~pos ~len ~mode)
    (fun () ->
      [fd_r fd;
       ("pos", Int.sexp_of_t pos);
       len_r len;
       ("mode", sexp_of_list sexp_of_msg_flag mode)])
;;

let recv = recv_send recv
let recvfrom = recv_send recvfrom
let send = recv_send send

let sendto fd ~buf ~pos ~len ~mode ~addr =
  improve (fun () -> sendto fd ~buf ~pos ~len ~mode ~addr)
    (fun () ->
      [fd_r fd;
       ("pos", Int.sexp_of_t pos);
       len_r len;
       ("mode", sexp_of_list sexp_of_msg_flag mode);
       ("addr", sexp_of_sockaddr addr)])
;;

type socket_bool_option =
Unix.socket_bool_option =
| SO_DEBUG
| SO_BROADCAST
| SO_REUSEADDR
| SO_KEEPALIVE
| SO_DONTROUTE
| SO_OOBINLINE
| SO_ACCEPTCONN
| TCP_NODELAY
| IPV6_ONLY
with sexp_of

type socket_int_option =
Unix.socket_int_option =
| SO_SNDBUF
| SO_RCVBUF
| SO_ERROR
| SO_TYPE
| SO_RCVLOWAT
| SO_SNDLOWAT
with sexp_of

type socket_optint_option =
Unix.socket_optint_option =
| SO_LINGER
with sexp_of
                  
type socket_float_option =
Unix.socket_float_option =
| SO_RCVTIMEO
| SO_SNDTIMEO
with sexp_of

let make_sockopt get set sexp_of_opt sexp_of_val =
  let getsockopt fd opt =
    improve (fun () -> get fd opt)
      (fun () -> [fd_r fd; ("opt", sexp_of_opt opt)])
  in
  let setsockopt fd opt value =
    improve (fun () -> set fd opt value)
      (fun () ->
        [fd_r fd; ("opt", sexp_of_opt opt); ("val", sexp_of_val value)])
  in
  (getsockopt, setsockopt)
;;

let (getsockopt, setsockopt) =
  make_sockopt getsockopt setsockopt
    sexp_of_socket_bool_option sexp_of_bool
;;

let (getsockopt_int, setsockopt_int) =
  make_sockopt getsockopt_int setsockopt_int
    sexp_of_socket_int_option sexp_of_int
;;

let (getsockopt_optint, setsockopt_optint) =
  make_sockopt getsockopt_optint setsockopt_optint
    sexp_of_socket_optint_option (sexp_of_option sexp_of_int)
;;

let (getsockopt_float, setsockopt_float) =
  make_sockopt getsockopt_float setsockopt_float
    sexp_of_socket_float_option sexp_of_float
;;


let open_connection addr =
  improve (fun () -> open_connection addr) (fun () -> [addr_r addr])
;;

let shutdown_connection = shutdown_connection

let establish_server handle_connection ~addr =
  improve (fun () -> establish_server handle_connection ~addr)
    (fun () -> [addr_r addr])
;;

type host_entry =
Unix.host_entry = {
  h_name : string;
  h_aliases : string array;
  h_addrtype : socket_domain;
  h_addr_list : inet_addr array;
}

type protocol_entry =
Unix.protocol_entry = {
  p_name : string;
  p_aliases : string array;
  p_proto : int;
}

type service_entry =
Unix.service_entry = {
  s_name : string;
  s_aliases : string array;
  s_port : int;
  s_proto : string;
}

let gethostname = gethostname

let make_byname f name =
  improve (fun () -> f name) (fun () -> [("name", atom name)])
;;

let gethostbyname = make_byname gethostbyname

let gethostbyaddr addr =
  improve (fun () -> gethostbyaddr addr)
    (fun () -> [("addr", sexp_of_inet_addr addr)])
;;

let getprotobyname = make_byname getprotobyname

let getprotobynumber num =
  improve (fun () -> getprotobynumber num)
    (fun () -> [("number", Int.sexp_of_t num)])
;;

let getservbyname = make_byname getservbyname

let getservbyport num ~protocol =
  improve (fun () -> getservbyport num ~protocol)
    (fun () -> [("number", Int.sexp_of_t num);
                ("protocol", atom protocol)])
;;

type addr_info =
Unix.addr_info = {
  ai_family : socket_domain;
  ai_socktype : socket_type;
  ai_protocol : int;
  ai_addr : sockaddr;
  ai_canonname : string;
}
with sexp_of

type getaddrinfo_option =
Unix.getaddrinfo_option =
| AI_FAMILY of socket_domain
| AI_SOCKTYPE of socket_type
| AI_PROTOCOL of int
| AI_NUMERICHOST
| AI_CANONNAME
| AI_PASSIVE
with sexp_of

let getaddrinfo host service opts =
  improve (fun () -> getaddrinfo host service opts)
    (fun () ->
      [("host", atom host);
       ("service", atom service);
       ("opts", sexp_of_list sexp_of_getaddrinfo_option opts)])
;;

type name_info =
Unix.name_info = {
  ni_hostname : string;
  ni_service : string;
}

type getnameinfo_option =
Unix.getnameinfo_option =
| NI_NOFQDN
| NI_NUMERICHOST
| NI_NAMEREQD
| NI_NUMERICSERV
| NI_DGRAM
with sexp_of

let getnameinfo addr opts =
  improve (fun () -> getnameinfo addr opts)
    (fun () ->
      [("addr", sexp_of_sockaddr addr);
       ("opts", sexp_of_list sexp_of_getnameinfo_option opts)])
;;

type terminal_io =
Unix.terminal_io = {
  mutable c_ignbrk : bool;
  mutable c_brkint : bool;
  mutable c_ignpar : bool;
  mutable c_parmrk : bool;
  mutable c_inpck : bool;
  mutable c_istrip : bool;
  mutable c_inlcr : bool;
  mutable c_igncr : bool;
  mutable c_icrnl : bool;
  mutable c_ixon : bool;
  mutable c_ixoff : bool;
  mutable c_opost : bool;
  mutable c_obaud : int;
  mutable c_ibaud : int;
  mutable c_csize : int;
  mutable c_cstopb : int;
  mutable c_cread : bool;
  mutable c_parenb : bool;
  mutable c_parodd : bool;
  mutable c_hupcl : bool;
  mutable c_clocal : bool;
  mutable c_isig : bool;
  mutable c_icanon : bool;
  mutable c_noflsh : bool;
  mutable c_echo : bool;
  mutable c_echoe : bool;
  mutable c_echok : bool;
  mutable c_echonl : bool;
  mutable c_vintr : char;
  mutable c_vquit : char;
  mutable c_verase : char;
  mutable c_vkill : char;
  mutable c_veof : char;
  mutable c_veol : char;
  mutable c_vmin : int;
  mutable c_vtime : int;
  mutable c_vstart : char;
  mutable c_vstop : char;
} with sexp_of

let tcgetattr = unary_fd tcgetattr

type setattr_when = 
Unix.setattr_when = 
| TCSANOW
| TCSADRAIN
| TCSAFLUSH
with sexp_of

let tcsetattr fd ~mode termios =
  improve (fun () -> tcsetattr fd ~mode termios)
    (fun () -> [fd_r fd;
                ("mode", sexp_of_setattr_when mode);
                ("termios", sexp_of_terminal_io termios)])
;;

let tcsendbreak fd ~duration =
  improve (fun () -> tcsendbreak fd ~duration)
    (fun () -> [fd_r fd;
                ("duration", Int.sexp_of_t duration)])
;;

let tcdrain = unary_fd tcdrain

type flush_queue = 
Unix.flush_queue = 
| TCIFLUSH
| TCOFLUSH
| TCIOFLUSH
with sexp_of

let tcflush fd ~mode =
  improve (fun () -> tcflush fd ~mode)
    (fun () -> [fd_r fd; ("mode", sexp_of_flush_queue mode)])
;;

type flow_action = 
Unix.flow_action = 
| TCOOFF
| TCOON
| TCIOFF
| TCION
with sexp_of

let tcflow fd ~mode =
  improve (fun () -> tcflow fd ~mode)
    (fun () -> [fd_r fd; ("mode", sexp_of_flow_action mode)])
;;

let setsid = setsid
