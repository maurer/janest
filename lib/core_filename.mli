(**
   [make_absolute ~dir file]
   If [file] is relative, returns the path of the file [file] relative to
   directory [dir].  If [file] is absolute, then returns [file].
*)
val make_absolute: string -> dir:string  -> string

(**
   [is_posix_valid f]
   @returns true if [f] is a valid name in a POSIX compliant OS
*)
val is_posix_valid: string -> bool

(**
   [temp_file ?temp_dir_name prefix suffix]

   Returns the name of a fresh temporary file in the temporary directory. The
   base name of the temporary file is formed by concatenating prefix, then, if
   needed, a 6-digit hex number, then suffix. The temporary file is created
   empty, with permissions [0o600] (readable and writable only by the file
   owner).  The file is guaranteed to be fresh, i.e. not already existing in
   the directory.

   @param temp_dir the directory in which to create the temporary file
*)
(* CRv2 achlipala: This and later functions could use labels on the prefix and suffix
   arguments. *)
(*
  tvaroquaux: As such, these functions are strict extensions of the ones
  provided by the OCaml standard library. If we add labels we shall break this.
  I really do not think it is worth the hassle
*)
val temp_file: ?temp_dir: string -> string -> string -> string
  
(**
   Same as temp_file but creates a temporary directory.
*)
val temp_dir: ?in_dir:string -> string -> string -> string

(** Same as {!Core_filename.temp_file}, but returns both the name of a fresh
   temporary file, and an output channel opened (atomically) on
   this file.  This function is more secure than [temp_file]: there
   is no risk that the temporary file will be modified (e.g. replaced
   by a symbolic link) before the program opens it.  The optional argument
   [mode] is a list of additional flags to control the opening of the file.
   It can contain one or several of [Open_append], [Open_binary],
   and [Open_text].  The default is [Open_text] (open in text mode). *)
val open_temp_file :
  ?mode: open_flag list ->
  ?temp_dir: string -> string -> string -> string * out_channel

(**
   Same as temp_file but creates a temporary directory.
*)
val temp_dir: ?in_dir:string -> string -> string -> string

(** {3 functions from the standard library}
    For these function definitions check the standard library
*)
val current_dir_name : string
val parent_dir_name : string
val concat : string -> string -> string
val is_relative : string -> bool
val is_implicit : string -> bool
val check_suffix : string -> string -> bool
val chop_suffix : string -> string -> string
val chop_extension : string -> string
val basename : string -> string
val dirname : string -> string
val temp_dir_name : string
val quote : string -> string
