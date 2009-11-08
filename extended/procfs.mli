(**
   Process and system stats
*)

type bigint = Big_int.big_int
type stat =
    {
      (* pid: int; *)      (* The process ID -- no need to repeat *)
      comm:        string; (** The filename of the executable *)
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

(** [get_all_procs] returns a process list *)
val get_all_procs : unit -> process list

(** [with_pid pid] returns a single process that matches pid *)
val with_pid : int -> process

(** [with_uid uid] returns all processes which belong to uid *)
val with_uid : int -> process list

(** [with_username user] calls with_uid after looking up the user's uid *)
val with_username : string -> process list

(** [hertz] is the number of jiffies per second *)
val hertz : float

(** [memtotal] is the amount of physical memory in the system, in kB *)
val memtotal : int
