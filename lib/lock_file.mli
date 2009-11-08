

(** mutual exclusion between processes using lockf

    These locks are OS-level and can be Mandatory (cannot be ignored)
    [on Linux, depending on file system mount(8) options, see fnctl(2)]
    but are Local (will not work across computers
    even if they mount the same directory).
*)


(** [create ~path ~message] tries to create a file at [path]
    containing the text [message].  It returns true on success, false
    on failure.  Note: there is no way to release the lock or the fd
    created inside!  It will only be released when the process dies.*)
val create : path : string -> message : string -> bool

(** [create_exn ~path ~message] is like [create] except that it throws
    an exception on failure instead of returning a boolean value *)
val create_exn : path : string -> message : string -> unit

(** [create_pid ~path] creates a file at [path] containing the pid of the process *)
val create_pid : path : string -> bool

(** [create_pid_exn ~path] creates a file at [path] containing the pid of the
 * process.  It throws an exception on failure.
 *)
val create_pid_exn : path : string -> unit

(** [blocking_create t] tries to create the lock. If another process holds
    the lock this function will wait until it is released. *)
val blocking_create : path : string -> message : string -> unit

(** [is_locked path] returns true when the file at [path] exists and
    is locked, false otherwise. *)
val is_locked : string -> bool
