open Unix
open OUnit
open Core.Std
open Linux_ext

let send_fd_test () = 
  (* This is a translation of W. Richard Stevens' mycat example (which
     was in C), which appeared in Unix Network Programming Chapter 14
     Section 7 *)
  let contents = "foo bar baz" in
  let generate_test_data () =
    let tmp_file = Filename.temp_file "" "" in
    let fd = open_out tmp_file in
    Pervasives.output_string fd (sprintf "%s\n" contents);
    close_out fd;
    tmp_file
  in
  let my_open path = 
    let (s, c) =
      Std_unix.socketpair
        ~domain:Std_unix.PF_UNIX
        ~kind:Std_unix.SOCK_STREAM
        ~protocol:0
    in
    let handler = ref (fun () -> ()) in
    let sigchld_handler = 
      Sys.signal Sys.sigchld (Sys.Signal_handle (fun _ -> !handler ()))
    in
    let cleanup () = 
      Sys.set_signal Sys.sigchld sigchld_handler;
      Unix.close s;
      Unix.close c
    in
    protect ~finally:cleanup ~f:(fun () ->
      handler := (fun () ->
        match Unix.wait () with
        | (_, Unix.WEXITED c) -> assert (c = 0)
        | _ -> assert false);
      let pid = Unix.fork () in
      if pid = 0 then begin
        try
          let fd = Unix.openfile path ~mode:[Unix.O_RDONLY] ~perm:0o644 in
          ignore (Unix.alarm 1); (* die after a second. in case we block forever *)
          Linux_ext.send_fd ~sock:c ~fd_to_send:fd;
          exit 0
        with _ -> exit 1
      end else
        Linux_ext.recv_fd ~sock:s)
  in
  let path = generate_test_data () in
  let fd = my_open path in
  let ic = Unix.in_channel_of_descr fd in
  let line = input_line ic in
  assert (contents = line)

let epoll_test () =
  let buf_size = 100_000 in
  let buf = String.create buf_size in
  let in_flags = Epoll.make_flags [| Epoll.IN |] in
  let out_flags = Epoll.make_flags [| Epoll.OUT |] in
  let epfd = Epoll.create 2 in
  let ifd, ofd = Unix.pipe () in
  set_nonblock ifd;
  set_nonblock ofd;
  Epoll.add ~epfd ~fd:ifd in_flags;
  Epoll.add ~epfd ~fd:ofd out_flags;
  let n_received_ref = ref 0 in
  let n_written_ref = ref 0 in
  let n_all = 100_000_000 in
  while !n_received_ref <> n_all do
    let events = Epoll.wait epfd ~maxevents:100 ~timeout:(-1) in
    if Array.length events = 0 then failwith "Timeout"
    else
      let act (fd, flags) =
        if fd = ifd then
          let () = assert (Epoll.has_in flags) in
          let n = read fd buf 0 buf_size in
          n_received_ref := !n_received_ref + n
        else
          if fd = ofd then
            let () = assert (Epoll.has_out flags) in
            let () = assert (!n_written_ref < n_all) in
            let n_to_write = min buf_size (n_all - !n_written_ref) in
            let n = write fd buf 0 n_to_write in
            n_written_ref := !n_written_ref + n;
            if !n_written_ref = n_all then close ofd
      in
      Array.iter events ~f:act
  done

let test =
  "Linux_ext" >:::
    [
      "epoll" >:: epoll_test;
      "send_fd" >:: send_fd_test;
    ]
