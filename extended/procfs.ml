open Core.Std

(* Learn more about this business by consulting proc(5) *)

type bigint = Big_int.big_int
let procdir = "/proc"

type stat =
    {
      (* pid: int; *)      (* The process ID -- no need to repeat *)
      comm:        string; (* The filename of the executable *)
      state:       char;   (* One  character from the string "RSDZTW" *)
      ppid:        int;    (* The PID of the parent. *)
      pgrp:        int;    (* The process group ID of the process. *)
      session:     int;    (* The session ID of the process. *)
      tty_nr:      int;    (* The tty the process uses. *)
      tpgid:       int;    (* The process group ID of the process which
                              currently owns the tty... *)
      flags:       bigint; (* The kernel flags word of the process. *)
      minflt:      bigint; (* The number of minor faults the process has
                              made which have not required loading a memory
                              page from disk. *)
      cminflt:     bigint; (* The number of minor faults that the process’s
                              waited-for children have made. *)
      majflt:      bigint; (* The number of major faults the process has made
                              which have required loading a page from disk. *)
      cmajflt:     bigint; (* The number of major faults that the process’s
                              waited-for children have made. *)
      utime:       bigint; (* The number of jiffies that this process has been
                              scheduled in user mode. *)
      stime:       bigint; (* The number of jiffies that this process has been
                              scheduled in kernel mode. *)
      cutime:      bigint; (* The number of jiffies that this process’s waited-for
                              children have been scheduled in user mode. *)
      cstime:      bigint; (* The number of jiffies that this process’s waited-for
                              children have been scheduled in kernel mode. *)
      priority:    bigint; (* The standard nice value, plus fifteen.  The value
                              is never negative in the kernel. *)
      nice:        bigint; (* The nice value ranges from 19 to -19*)
      unused:      bigint; (* placeholder for removed field *)
      itrealvalue: bigint; (* The time in jiffies before the next SIGALRM is
                              sent to the process due to an interval timer. *)
      starttime:   bigint; (* The time in jiffies the process started after
                              system boot. *)
      vsize:       bigint; (* Virtual memory size in bytes. *)
      rss:         bigint; (* Resident Set Size: number of pages the process
                              has in real memory. *)
      rlim:        bigint; (* Current limit in bytes on the rss of the process. *)
      startcode:   bigint; (* The address above which program text can run. *)
      endcode:     bigint; (* The address below which program text can run. *)
      startstack:  bigint; (* The address of the start of the stack. *)
      kstkesp:     bigint; (* The current value of esp (stack pointer) *)
      signal:      bigint; (* The bitmap of pending signals. *)
      blocked:     bigint; (* The bitmap of blocked signals. *)
      sigignore:   bigint; (* The bitmap of ignored signals. *)
      sigcatch:    bigint; (* The bitmap of caught signals. *)
      wchan:       bigint; (* This  is  the "channel" in which the
                              process is waiting. Address of a system call. *)
      nswap:       bigint; (* (no longer maintained) *)
      cnswap:      bigint; (* (no longer maintained) *)
      exit_signal: int;    (* Signal sent to parent when we die. *)
      processor:   int;    (* CPU number last executed on. *)
      rt_priority: bigint; (* Real-time scheduling priority. *)
      policy:      bigint; (* Scheduling policy *)
    }

type statm =
    {
      size:     bigint; (* total program size *)
      resident: bigint; (* resident set size *)
      share:    bigint; (* shared pages *)
      text:     bigint; (* text (code) *)
      lib:      bigint; (* library *)
      data:     bigint; (* data/stack *)
      dt:       bigint; (* dirty pages (unused) *)
    }

type status =
    {
      uid:   int; (* Real user ID *)
      euid:  int; (* Effective user ID *)
      suid:  int; (* Set user ID *)
      fsuid: int; (* FS user ID *)
    }

type process =
    {
      pid:         int;           (* Process ID *)
      cmdline:     string;        (* Command-line (not reliable). *)
      cwd:         string option; (* Symlink to working directory. *)
      environ:     string option; (* Process environment. *)
      exe:         string option; (* Symlink to executed command. *)
      root:        string option; (* Per-process root (e.g. chroot) *)
      stat:        stat;          (* Status information. *)
      statm:       statm;         (* Memory status information. *)
      status:      status;        (* Some more assorted status information. *)
      top_command: string;        (* Show what top would show for COMMAND *)
   }

(* lu and ld match the proc(5) format strings %lu and %ld *)
let lu x = Big_int.big_int_of_string x
let ld x = lu x   (* bigint everything to make math easier *)

let extract_command s =
 (*
  * extract_cmdline, for a stat string such as:
  *   "14574 (cat) R 10615 14574 10615 34820 14574 4194304 164 0..."
  * returns this tuple
  *   "cat", "R 10615 14574 10615..."
  *)
  let i = String.index s '(' in
  let j = String.rindex s ')' in
  (String.sub s ~pos:(i+1) ~len:(j-(i+1)),
   String.sub s ~pos:(j+1) ~len:((String.length s)-(j+1)))

let load_proc_stat s =
  let comm, rest = extract_command s in
  let a = Array.of_list (String.split (String.strip rest) ~on:' ') in
  let d x = int_of_string x in
  let c x = x.[0] in
  {
      (* pid      = (we already know the pid) *)
    comm        = comm;
    state       = c a.(0);
    ppid        = d a.(1);
    pgrp        = d a.(2);
    session     = d a.(3);
    tty_nr      = d a.(4);
    tpgid       = d a.(5);
    flags       = ld a.(6);
    minflt      = ld a.(7);
    cminflt     = ld a.(8);
    majflt      = ld a.(9);
    cmajflt     = ld a.(10);
    utime       = ld a.(11);
    stime       = ld a.(12);
    cutime      = ld a.(13);
    cstime      = ld a.(14);
    priority    = ld a.(15);
    nice        = ld a.(16);
    unused      = ld a.(17);
    itrealvalue = ld a.(18);
    starttime   = lu a.(19);
    vsize       = lu a.(20);
    rss         = ld a.(21);
    rlim        = lu a.(22);
    startcode   = lu a.(23);
    endcode     = lu a.(24);
    startstack  = lu a.(25);
    kstkesp     = lu a.(26);
    signal      = lu a.(27);
    blocked     = lu a.(28);
    sigignore   = lu a.(29);
    sigcatch    = lu a.(30);
    wchan       = lu a.(31);
    nswap       = lu a.(32);
    cnswap      = lu a.(33);
    exit_signal = d a.(34);
    processor   = d a.(35);
    rt_priority = lu a.(36);
    policy      = lu a.(37);
  }

let load_proc_statm s =
  let a = Array.of_list (String.split s ~on:' ') in
  {
    size     = lu a.(0);
    resident = lu a.(1);
    share    = lu a.(2);
    text     = lu a.(3);
    lib      = lu a.(4);
    data     = lu a.(5);
    dt       = lu a.(6);
  }

let load_proc_status s =
  (* Splits "foo: 1\nbar: 2\n" into [Some ("foo"," 1"); Some ("bar"," 2"); None] *)
  let records = List.map (String.split s ~on:'\n')
    ~f:(fun x -> String.lsplit2 x ~on:':')
  in
  let _, uids = Option.value_exn
    (List.find_exn records
       ~f:(fun kv -> match kv with
           | Some (k,_) -> (k = "Uid")
           | None -> false))  (* handle blank line *)
  in
  sscanf (String.strip uids) "%d %d %d %d"
    (fun a b c d -> { uid = a; euid = b; suid = c; fsuid = d })

let string_of_file fn = read_wrap ~f:In_channel.input_all fn

(** [load_proc procdir pid] loads process information from /<procdir>/<pid>/*
    into a process type.  This is similar to how ps and top get their
    information. *)
let load_proc procdir pid =
  let slurp f fn =
    try
      Some (f (sprintf "/%s/%d/%s" procdir pid fn))
    with
    | Sys_error _ -> None
    | Unix.Unix_error (Unix.EACCES, _, _) -> None
    | Unix.Unix_error (Unix.ENOENT, _, _) -> None
    | Unix.Unix_error (Unix.EINVAL, _, _) -> None
  in
  let slurp_file fn = slurp string_of_file fn in
  let slurp_link fn = slurp Unix.readlink fn in

  let required x = Option.value_exn x in

  (* These are required; we won't have a useful proc entry without them. *)
  let stat = load_proc_stat (required (slurp_file "stat")) in
  let statm = load_proc_statm (required (slurp_file "statm")) in
  let status = load_proc_status (required (slurp_file "status")) in
  let cmdline = required (slurp_file "cmdline") in

  (* These are optional; we don't count on them since they
     may required privileged access or have been discarded. *)
  let cwd = slurp_link "cwd" in
  let environ = slurp_file "environ" in
  let exe = slurp_link "exe" in
  let root = slurp_link "root" in

(*
 * Process command name is tricky.
 *
 * cmdline is ideal but not guaranteed to be there, because the kernel
 *  - may discard it in lomem situations
 *  - discard it for zombie processes
 *  - put nothing useful there for kernel processes
 *
 * The exe symlink might be useful, it's the name of the executable
 * which started the process, but permission is usually denied for
 * non-root/non-self viewers.
 *
 * The stat.command field will ALWAYS be there but is truncated
 * to 16 chars; we do here what top does: use cmdline if it is
 * populated, otherwise use stat.command.
 *)
  let top_command = String.strip (
    if cmdline = "" then
      stat.comm
    else
      String.tr ~target:'\x00' ~replacement:' ' cmdline)
  in
  {
    pid         = pid;
    cmdline     = cmdline;
    cwd         = cwd;
    environ     = environ;
    exe         = exe;
    root        = root;
    stat        = stat;
    statm       = statm;
    status      = status;
    top_command = top_command;
  }

let is_pid s =
  try
    let _ = int_of_string s in
    true
  with
  | Failure (_) -> false

let get_all_procs () =
  let procs = Sys.readdir procdir in
  let a = Array.filter_map
    ~f:(fun pid ->
          try
            Some (load_proc procdir (int_of_string pid))
          with
            (* Failures usually aren't a fatal *system* condition.
               procfs queries on Linux simply are not consistent.
               They're generally thwarted by terminating processes.
               We simply skip the proc entry on failure. *)
          | Not_found | Failure _ -> None) procs
  in
  Array.to_list a

let with_pid pid = load_proc procdir pid

let with_uid uid =
  List.filter (get_all_procs ()) ~f:(fun p -> p.status.uid = uid)

let with_username name =
  let p = Unix.getpwnam name in
  with_uid p.Unix.pw_uid

(*
 * This is a partial translation of
 *   sysinfo.c:init_Hertz_value from procps (top)
 *)
let hertz =
  let get_uptime () =
    let uptime1 = string_of_file (procdir^"/uptime") in
    sscanf uptime1 "%f" (fun x -> x)
  in
  let rec sample () =
    let up1 = get_uptime () in

    let stat = string_of_file (procdir^"/stat") in
    let statlines = String.split stat ~on:'\n' in

    (* On modern systems the second line is always cpu0 (even uni-processors) *)
    let statline = Option.value_exn (List.nth statlines 1) in

    let user_j, nice_j, sys_j, idle_j, iowait_j =
      sscanf statline "cpu0 %Lu %Lu %Lu %Lu %Lu" (fun a b c d e -> a,b,c,d,e)
    in
    let up2 = get_uptime() in
    if ((up2 -. up1) > 0.01) then
      sample ()  (* sampling latency too high.  try again *)
    else
      let (+) = Int64.(+) in
      user_j + nice_j + sys_j + idle_j + iowait_j, ((up1 +. up2) /. 2.)
  in
  let jiffies, seconds = sample () in
  (Int64.to_float jiffies) /. seconds

let memtotal =
  let meminfo = string_of_file (procdir^"/meminfo") in
  let memlines = String.split meminfo ~on:'\n' in
  let total = Option.value_exn (List.hd memlines) in
  sscanf total "MemTotal: %d kB" (fun x -> x)
