open Unix
open OUnit
open Core_extended.Std
open Core_extended.Extended_linux

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
      "epoll" >:: epoll_test
    ]
