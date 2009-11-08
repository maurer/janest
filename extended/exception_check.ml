open Core.Std

module Table = String.Table

type t = {
  known_exceptions : exn Table.t;
  scheduled_exceptions : exn Table.t;
  lock : Mutex.t;                     (* guards reading/writing of the two tables above *)
}


let t = ref None

let create ?(listen_port = 65100) exns =
  let ctx =
    { known_exceptions = Table.create 1024;
      scheduled_exceptions = Table.create 1024;
      lock = Mutex.create () }
  in
  List.iter exns ~f:(fun (c, exn) ->
    match Table.find ctx.known_exceptions c with
    | Some _ -> raise (Invalid_argument (sprintf "duplicate exception definition: %s" c))
    | None -> Table.replace ctx.known_exceptions ~key:c ~data:exn);
  let (_: Thread.t) =
    let module U = Unix in
    let clients = ref [] in
    let push fd = clients := fd :: !clients in
    let remove fd = clients := List.filter !clients ~f:(fun cl -> cl <> fd) in
    Thread.create (fun () ->
      let s = U.socket ~domain:U.PF_INET ~kind:U.SOCK_STREAM ~protocol:0 in
      U.bind s ~addr:(U.ADDR_INET (U.inet_addr_any, listen_port));
      U.listen s ~max:10;
      U.set_nonblock s;
      while true do
        try
          let { U.Select_fds.read = rd } =
            U.select ~read:(s :: !clients) ~write:[] ~except:[]
              ~timeout:(- 1.0)
          in
          if List.exists rd ~f:(fun fd -> fd = s) then
            push (fst (U.accept s));
          let rd = List.filter rd ~f:(fun fd -> fd <> s) in
          List.iter rd ~f:(fun fd ->
            let ic = U.in_channel_of_descr fd in
            let remove () =
              begin try U.close fd with _ -> () end;
              remove fd
            in
            try
              let line = input_line ic in
              critical_section ctx.lock ~f:(fun () ->
                match Table.find ctx.known_exceptions line with
                | None -> ()
                | Some exn ->
                    Table.replace ctx.scheduled_exceptions ~key:line ~data:exn)
            with _ -> remove ())
        with U.Unix_error ((U.EAGAIN | U.EINTR | U.EWOULDBLOCK), _, _) -> ()
      done)
      ()
  in
  t := Some ctx

let maybe_raise lst =
  match !t with
  | None -> ()
  | Some t ->
      critical_section t.lock ~f:(fun () ->
        List.iter lst ~f:(fun c ->
          match Table.find t.scheduled_exceptions c with
          | None -> ()
          | Some exn ->
              Table.remove t.scheduled_exceptions c;
              raise exn))
