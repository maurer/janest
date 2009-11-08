(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Extended_linux"

open Core.Std
open Unix

(* Epoll *)

module Epoll = struct
  (* Epoll flags *)

  type flag = IN | OUT | PRI | ERR | HUP | ET | ONESHOT with sexp, bin_io
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

  let flag_to_string flag = Sexplib.Sexp.to_string (sexp_of_flag flag)

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


(* Reporting of open file descriptors *)

module Open_fds = struct
  type file = { name : string; deleted : bool } with sexp, bin_io

  type kind =
    | File of file
    | Socket of int64
    | Pipe of int64
    | Inotify
    | Unknown of string
  with sexp, bin_io

  type t = { fd : int; kind : kind } with sexp, bin_io

  let rex = Pcre.regexp "^(?:(socket|pipe):\\[(\\d+)\\]|.*)$"

  let discover pid =
    let dir =
      Filename.concat (Filename.concat "/proc" (string_of_int pid)) "fd"
    in
    let dh = Unix.opendir dir in
    protectx dh ~finally:Unix.closedir ~f:(fun dh ->
      let rec loop acc =
        match try Some (readdir dh) with End_of_file -> None with
        | None -> List.rev acc
        | Some ("." | "..") -> loop acc
        | Some fd_name ->
            let fd = int_of_string fd_name in
            let name = readlink (Filename.concat dir fd_name) in
            let kind =
              match Pcre.extract ~rex name with
              | [| _; "socket"; sock_id |] -> Socket (Int64.of_string sock_id)
              | [| _; "pipe"; pipe_id |] -> Pipe (Int64.of_string pipe_id)
              | [| "inotify"; _; _ |] -> Inotify
              | [| name; _; _ |] ->
                  if String.length name = 0 || name.[0] <> '/' then
                    Unknown name
                  else begin
                    match String.rsplit2 name ~on:' ' with
                    | Some (name, "(deleted)") ->
                        File { name = name; deleted = true }
                    | _ -> File { name = name; deleted = false }
                  end
              | _ -> assert false  (* impossible *)
            in
            let el = { fd = fd; kind = kind } in
            loop (el :: acc)
      in
      loop [])

  let discover ?(pid = getpid ()) () =
    Exn.reraise_uncaught "Core.Linux_ext.Open_fds.discover" (fun () -> discover pid)
end


(* Splicing - zero-copies between kernel buffers *)



