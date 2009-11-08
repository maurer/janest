

(** [daemonize ?close_stdio ?cd ?umask ()] makes the executing process a
    daemon, i.e. maximally independent from other processes and least
    intrusive on others.

    To achieve this the following steps are taken:

      1) Fork, parent exit
      2) Become session and process group leader.
      3) Fork again so that no terminal can ever get control over us again.
         second parent exits.
      4) Change current working directory to [cd] to avoid problems during
         unmounting of network file systems.  May need to be change from default
         if you want to leave core dumps somewhere.
      5) Set the umask to [umask].
      6) if [close_stdio] = [true], close stdin, stdout and stderr by opening
         /dev/null and duplicating the descriptor.

    Do not forget to set up logging to a file, with toplevel exception handling,
    as otherwise startup failures will be lost in the ether.

    @param close_stdio default = [true]
    @param cd default = ["/"]
    @param umask default = [0] to make all created files read- and
                           writeable by everybody by default.
    @raise Failure if fork was unsuccessful.
*)

val daemonize :
  ?close_stdio : bool
  -> ?cd : string
  -> ?umask : int
  -> unit
  -> unit


  
(** [daemonize_wait ?cd ?umask ()] makes the executing process a
    daemon, but can report information back to the launching process via exit codes
    and stdout/stderr for better startup/initialization error reporting.

    To achieve this the following steps are taken:

      1) Fork, parent waits for messages from child.
      2) Become session and process group leader.
      3) Fork again.
         parent waits for child/listens on a pipe for a message saying it
         can exit.
      4) Change current working directory to [cd].
      5) Set the umask to [umask].
      6) return closure which, when called will:
            close stdin, stdout and stderr by
              opening /dev/null and duplicating the descriptor.
            write to a pipe, which communicates success to parent,
            causing it to exit with 0.  that exit code bubbles up to
            grand-parent, which exits with 0 as well.

    Note that calling the closure (returned in step 6) will adjust SIGPIPE
    handling, so you should not rely on the delivery of this signal
    during this time.

    Any failures or premature exits by the child before the returned
    closure is called will cause the parent and then grandparent to exit
    with the same exit code as the child, thus communicating the failure
    back to the calling source.

    Do not forget to set up logging to a file, e.g. using the
    Syslog-module, for post-parental-release.  Before that you can use
    stdout/stderr.

    @param cd default = ["/"]
    @param umask default = [0] to make all created files read- and
                           writeable by everybody by default.

    @raise Failure if fork was unsuccessful.
*)

val daemonize_wait : ?cd : string -> ?umask : int -> unit -> (unit -> unit)
