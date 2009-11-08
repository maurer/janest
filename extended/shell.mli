(** Shell scripting in OCaml.

    This module contains basic blocks for shell scripting in OCaml. It tends to be
    much safer than just using [Unix.system] because it handles errors much more
    transparently.
    WARNING: it's undergoing some serious changes internally, then next public release of
    core should have the changes included
*)

open Core.Std

(** Colour printing on terminals *)
module Ansi : sig

  val is_color_tty : unit -> bool
  type style = [`Red | `Green | `Underline]

  val printf  : style -> ('a, out_channel, unit) format -> 'a
  val eprintf : style -> ('a, out_channel, unit) format -> 'a
end

(** Process dispatching *)
module Process : sig

  type status =  [ `Timeout of Time.Span.t | Unix.Process_status.t ]
  (** The termination status of a process.
      This is an extension of [Unix.Process_status.t] to allow timeouts.
  *)

  type t

  type result = {
    command : t;
    status  : status;
    stdout  : string;
    stderr  : string
  }

  exception Failed of result

  val to_string        : t -> string
  val status_to_string : status -> string

  val format_failed : result -> string

  val cmd    : string -> string list -> t
  val shell  : string -> t
  val remote : ?user:string -> host:string -> t -> t

  type 'a reader

  val content : string reader
  val discard : unit reader
  val lines   : string list reader
  val head    : string reader


  val run :
    ?timeout:Time.Span.t option
    -> ?working_dir:string
    -> ?expect: int list
    -> ?verbose:bool
    -> ?echo:bool
    -> ?input:string
    -> t
    -> 'a reader
    -> 'a

  val test :
    ?timeout:Time.Span.t option
    -> ?working_dir:string
    -> ?verbose:bool
    -> ?echo:bool
    -> ?input:string
    -> ?true_v:int list
    -> ?false_v:int list
    -> t
    -> bool
end


(** {6 Process handling }  *)


type 'a with_process_flags =
    ?timeout:Time.Span.t option
  -> ?working_dir:string (* rename to run_in? *)
  -> ?verbose:bool
  -> ?echo:bool
  -> ?input:string
  -> 'a
(**
   This type is an umbrella type for all the command that dispatch a process.
   It comes with a list of arguments whose default value can be tweaked by
   set_defaults.

   - [timeout] : the command will raise [Failed_command] if the program doesn't
     do any IO for this period of time
   - [working_dir] : run the command in this directory
   - [verbose] : prints the output of the command
   - [echo] : print out the command before running it
   - [input] : a string to pipe through the program's standard in
*)

type 'a with_run_flags = (?expect:int list ->'a ) with_process_flags
(**
   This is the list of flags for normal process dispatch. It is an extension of
   [with_process_flags].

   - [expect] : an int list of valid return codes. default value is [[0]], if
   the return code of the dispatched is not in this list we will blowup with
   [Process.Failure]
*)

(** {9 Basic run functions}

    In all the functions below the command is specified with two arguments the
    first one is string representing the process to run the second one is the
    list of arguments it takes.

    Although the arguments do not need to be escaped there is still a risk that
    they might be interpreted as flags when they aren't. Most basic unix
    utilities provide the ability to pass arguments after "--" to avoid this.

    Usage example:
{[
    let patch = run_full ~expect:[0;1] "diff" ["-u";"--";file1;file2]
]}
*)

type 'a cmd = string -> string list -> 'a

val run       : unit cmd with_run_flags
(** Runs a command and discards its output. *)

val run_lines : string list cmd with_run_flags
(** Runs a command and returns its output line separated. Note: most commands
    print a newline at the end of their output so the shell prompt appears on
    its own line. If the output ends in a newline, it is stripped before
    splitting the output into a string list to avoid there being a final
    element in the list containing just the empty string.

    In some cases, the newline should not be stripped (e.g., "cat" will not
    "add" a newline). If you care, use [run_full] for the entire buffer.
*)

val run_one   : string cmd with_run_flags
(** Returns the first line of the command's output.
    (This function might terminate the program early the same way that
    piping through grep would)
*)

val run_full  : string cmd with_run_flags
(** Return the full command's output in one string. See the note in
    [run_lines].
*)


(** {9 Dispatch to /bin/sh}

    All these function take a format (like printf) and run it through the shell.

    Usage example:
{[
    sh "cp -- %s %s" (Filename.quote file1)  (Filename.quote file2)
]}

    In general it is recommended to avoid using those too much and to prefer the
    run* family of function instead because it avoids pitfall like escaping
    issues and is much more straightforward to think about.
*)


type ('a,'ret) sh_cmd = (('a, unit, string,'ret) format4 -> 'a)

val sh       : ('a,unit)        sh_cmd with_run_flags
val sh_lines : ('a,string list) sh_cmd with_run_flags
val sh_one   : ('a,string)      sh_cmd with_run_flags
val sh_full  : ('a,string)      sh_cmd with_run_flags

(** {9 Test dispatches}

    Usage example:
{[
    if Shell.test "diff" ["-q";"--";file1;file2] then
       Printf.printf "Files %S and %S are the same\n%!" file1 file2;
]}

*)

(** This is the list of flags for dispatching processes in test mode. This is used to test the
    return code of the dispatched program. The return value of these functions will be :
    - [true] if the exit code is in [true_v].
    - [false] if the exit code is in [false_v] and not in [true_v].
    - Raises [Process.Failure] otherwise

    The default values are:
    - [true_v]: default value [[0]]
    - [false_v]: default_value [[1]]
*)
type 'a with_test_flags = (?true_v:int list -> ?false_v:int list ->'a ) with_process_flags

val test    : bool cmd with_test_flags
(** *)

val sh_test : ('a,bool) sh_cmd with_test_flags

(** {6 Small helper commands} *)

val mkdir_p : ?perm:Unix.file_perm -> string -> unit
val ls : string -> string list
val quote : string -> string

(**
   Returns true if the file exists and is a directory
*)
val is_directory : ?unlink:bool -> string -> bool
val is_file : ?unlink:bool -> string -> bool

val file_kind : string -> Unix.file_kind

(**
   Returns true if the file exists
*)
val file_exists : string -> bool

val copy_file :
  ?overwrite:bool
  -> ?perm:Unix.file_perm
  -> src:string
  -> dst:string
  -> unit

val rm : string -> unit

val cp : string -> string -> unit
(** Raises "Failed_command" *)

(** Get the username. By default, the effective username. If real is true, get
    the real username. *)
val whoami : ?real:bool -> unit -> string

(** Get the home of the effective user *)
val home : unit -> string

(** Get the names of the groups the user belongs to *)
val get_group_names : unit -> string list

(** Test if the user is in the given group *)
val in_group : string -> bool

val hostname : unit -> string

val which : string -> string option

(** Get an installed editor. The preferred editor is $EDITOR. If no editors can be
    found, returns None. Warning: ed is at the end of the search list. *)
val get_editor : unit -> string option


val mkdir_p : ?perm:Unix.file_perm -> string -> unit
val ls : string -> string list

val quote : string -> string

(**Concat and normalize*)
val (^/) : string -> string -> string


(** [ssh user host command] run command via ssh *)
val ssh : user:string -> host:string -> string -> unit


(** [scp user host from to] copy local file from to to *)
val scp : ?recurse:bool -> user:string -> host:string -> string -> string -> unit

val echo : ('a,out_channel,unit) format -> 'a
val warnf : ('a,unit,string,unit) format4 -> 'a


val email :
  string -> string Core.Std.List.container -> string -> string -> unit
