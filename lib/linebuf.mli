(** Line-by-line reading of a file.  A line buffer allows one to read
    one line of a file at a time, blocking until a line is available.
    Line buffers are distinct from Pervasives.read_line in that they
    "notice" new data arriving in the file more quickly. *)

(** The type of a line buffer. *)
type t

type error_type = Null_retry | Too_many_nulls | Exception of string * exn

type result = 
    Success of int * string
  | Nothing_available
  | Error of error_type
  | Fatal_error of string * exn

(** Open a line buffer from the passed filename.  If [close_on_eof] is
    set, when [eof] is read, the file will be closed and reopened if
    necessary. if [follow_deletes] is set, then when [eof] is read linebuf
    will stat the file, and if it has been deleted and recreated it
    will open the new file. *)
val open_linebuf : ?close_on_eof:bool -> ?null_hack:bool -> 
  ?follow_deletes:bool -> string -> t

(** Closes the line buffer (and the underlying file).  *)
val close_linebuf : t -> unit

(** Returns whether or not the line buffer is closed *)
val closed_linebuf : t -> bool

(** Tries to read a line from the file.  If no more lines are available, 
    returns [None]. *)
val try_read : t -> string option

(** [try_read_lnum] is like [try_read] except also provides the line number of the 
    read line. *)
val try_read_lnum : t -> (int * string) option

(** Like try_read, except that it returns more verbose errors *)
val try_read_lnum_verbose : t -> result

(** Calls try_read every 0.01 seconds and returns when a line is available. *)
val read : t -> string

(** Seeks to the end of the file and blocks until another line is available -- this new
    line is not returned. *)
val tail : t -> unit

(** Same as [tail] except it may return before a new line is available on the file
    (i.e. it doesn't block). *)
val unsafe_tail : t -> unit
