(** In_channel collects all of the pervasive functions that work on in_channels.
    * It adds some new functions (like [input_all] and [input_lines]).
    * It names things using the fact that there is no worry about toplevel name
      conflicts (since we are in a module).
    * It uses labelled arguments.
    * It returns an option rather than raising End_of_file.
*)

type t = in_channel

val stdin : t

val create : ?binary:bool -> string -> t

val close : t -> unit
val close_noerr : t -> unit

val input : t -> buf:string -> pos:int -> len:int -> int
val really_input : t -> buf:string -> pos:int -> len:int -> unit option
val input_byte : t -> int option
val input_char : t -> char option

val input_binary_int : t -> int option
val input_value : t -> 'a option
val input_all : t -> string

(** [input_line ?fix_win_eol t] reads a line from [t] and returns it, without
    the newline ("\n") character at the end, and, if [fix_win_eol] the trailing
    "\r\n" is dropped.
*)
val input_line : ?fix_win_eol:bool -> t -> string option

(** [fold_lines ?fix_win_eol t ~init ~f] folds over the lines read from [t]
    using [input_line].
*)
val fold_lines :
  ?fix_win_eol:bool -> t -> init:'a -> f:('a -> string -> 'a) -> 'a

(** [input_lines ?fix_win_eol t] returns the list of lines read from [t] using
    [input_line].
*)
val input_lines : ?fix_win_eol:bool -> t -> string list

(** [iter_lines ?fix_win_eol t ~f] applies [f] to each line read from [t] using
    [input_line]. *)
val iter_lines : ?fix_win_eol:bool -> t -> f:(string -> unit) -> unit

val seek : t -> int -> unit
val pos : t -> int
val length : t -> int

val set_binary_mode : t -> bool -> unit
