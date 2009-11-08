
open Core.Std

(**
   Remembers the last n-characters appended to it....
*)
module Tail_buffer = struct
  (** remembers the output in a circular buffer.
      looped is used to tell whether we loop around the
      boundary of the buffer.
  *)
  type t = {
    buffer : string;
    length : int;
    mutable looped : bool;
    mutable position : int;
  }

  let contents b =
    if not b.looped then
      String.sub b.buffer ~pos:0 ~len:b.position
    else
      let dst = String.create (b.length + 3) in
      dst.[0] <- '.';
      dst.[1] <- '.';
      dst.[2] <- '.';
      String.blit
        ~src:b.buffer
        ~dst ~dst_pos:3
        ~src_pos:b.position
        ~len:(b.length - b.position);
      String.blit ~src:b.buffer
        ~dst
        ~dst_pos:(b.length - b.position + 3)
        ~src_pos:0
        ~len:(b.position);
      dst

  let create len = {
    buffer = String.create len;
    length = len;
    looped = false;
    position = 0
  }

  let add b src len =
    if b.length <= len then begin
      String.blit ~src ~dst:b.buffer ~dst_pos:0 ~src_pos:(len - b.length) ~len:(b.length);
      b.looped <- true;
      b.position <- 0
    end else
      let leftover =  b.length - b.position in
      if (len < leftover) then begin
        String.blit ~src ~dst:b.buffer ~dst_pos:b.position ~src_pos:0 ~len;
        b.position <- b.position + len;
      end else begin
        String.blit ~src ~dst:b.buffer ~dst_pos:b.position ~src_pos:0 ~len:leftover;
        b.looped <- true;
        let len = (len-leftover) in
        String.blit ~src ~dst:b.buffer ~dst_pos:0 ~src_pos:leftover ~len;
        b.position <- len
      end
end

module Command_result = struct
  type t= {
    status: Unix.Process_status.t;
    stdout_tail : string;
    stderr_tail : string
  }
end

let kill_child pid =
  (* Could use a thread to kill it and another to wait for it... *)
  (* Process already dead... *)
  if fst (Unix.waitpid ~mode:[Unix.WNOHANG] pid) <> pid then begin
    Signal.send ~process_must_exist:false Signal.term ~pid;
    Unix.sleep 1;
    if fst (Unix.waitpid ~mode:[Unix.WNOHANG] pid) <> pid then begin
      Signal.send ~process_must_exist:false Signal.kill ~pid;
      ignore (Unix.waitpid ~restart:true ~mode:[] pid)
    end
  end

(* set cwd to [dir] while running [f] *)
let pushd dir ~f =
  let owd = Sys.getcwd () in
  protectx
    ~finally: (fun _ -> Sys.chdir owd)
    ~f:(fun x ->
          Sys.chdir dir;
          f x
       )

exception Timeout of string * string (*stdout stderr*)

let run ?(timeout=(Time.Span.max_value)) ?working_dir ?input:(in_cnt="")
    ?(stdoutf=(fun _string _len -> ())) ?(stderrf=(fun _string _len -> ()))
    ?(tail_len = 256) ~prog ~args () =

  let timeout =
    if timeout = Time.Span.max_value then -1. else Time.Span.to_sec timeout
  in
  let run () = Unix.create_process ~prog ~args in
  let process_info =
    match working_dir with
    | None -> run ()
    | Some dir -> pushd dir ~f:run ()
  in

  let out_fd = process_info.Unix.Process_info.stdout
  and in_fd = process_info.Unix.Process_info.stdin
  and err_fd = process_info.Unix.Process_info.stderr
  and pid = process_info.Unix.Process_info.pid in

  let sbuf = String.create 4096
  and in_pos = ref 0
  and in_len = String.length in_cnt
  and in_pool = ref [in_fd]
  and out_pool = ref [out_fd;err_fd]
  and stdout_tail = Tail_buffer.create tail_len
  and stderr_tail = Tail_buffer.create tail_len
  in

  let old_sigpipe = Signal.signal Signal.pipe `Ignore in

  (* Close the process's in_channel iff we are done writing to it*)
  let in_autoclose () =
    if !in_pos = in_len then begin
      Unix.close in_fd;
      in_pool := []
    end
  in
  in_autoclose ();
  begin try
    while !in_pool <> [] || !out_pool <> [] do
      let { Unix.Select_fds.read = read; write = write } =
        Unix.select ~read:!out_pool ~write:!in_pool ~except:[] ~timeout
      in
      begin
        if (read = [] && write = []) then begin
          raise (Timeout (Tail_buffer.contents stdout_tail,
                          Tail_buffer.contents stderr_tail))
        end;
        match write with
        | [] -> ()
        | [in_fd] ->
            (try
                let len =
                  Unix.single_write in_fd
                    ~buf:in_cnt
                    ~pos:!in_pos
                    ~len:(in_len - !in_pos)
                in in_pos := !in_pos + len;
                in_autoclose ()
              with Unix.Unix_error (Unix.EPIPE,_,_) ->
                Unix.close in_fd;
                in_pool := [])
        | _ -> assert false
      end;
      List.iter read
        ~f:(fun fd ->
          let len = Unix.read fd
            ~buf:sbuf
            ~pos:0
            ~len:(String.length sbuf)
          in
          if len = 0 then begin
            Unix.close fd;
            out_pool := List.filter ~f:((<>) fd) !out_pool
          end else if fd = out_fd then begin
            Tail_buffer.add stdout_tail sbuf len;
            stdoutf sbuf len;
          end else if fd = err_fd then begin
            Tail_buffer.add stderr_tail sbuf len;
            stderrf sbuf len
          end else
              assert false
            )
    done
  with e ->
    List.iter (!out_pool@ !in_pool)
      ~f:(fun fd -> try Unix.close fd with _ -> ());
    (try kill_child pid with _ -> ());
    (try Signal.set Signal.pipe old_sigpipe with _ -> ());
    raise e
  end;
  Signal.set Signal.pipe old_sigpipe;
  {Command_result.
      status = snd (Unix.waitpid ~restart:true ~mode:[] pid);
      stdout_tail = Tail_buffer.contents stdout_tail;
      stderr_tail = Tail_buffer.contents stderr_tail
  }
