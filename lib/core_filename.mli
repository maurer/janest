(**
   Warning! this library assumes we are in a POSIX compliant OS. It will not work properly under
   windows (it doesn't handle drivenames).
*)

val root : string
(**
   The path of the root.
*)

val normalize : string -> string
(**
   [normalize path]
   Removes as much "." and ".." from the path as possible. If the path is
   absolute they will all be removed.
*)


val expand : ?cwd:string -> string -> string
(**
   [expand]
   Makes a path absolute by expanding . to cwd and ~ to home directories.
   In case of error (e.g.: path home of a none existing user) raises [Failure]
   with a (hopefully) helpful message.
*)

val parent : string -> string
(**
   [parent path]
   @return the path to the parent of [path] the result is normalized
   The parent of the root directory is the root directory
*)


val unroot : path:string -> string -> string
(**
   [unroot ~path f]
   returns [f] relative to [path].

   @raise Failure if [is_relative f <> is_relative path]
*)

val is_posix_valid : string -> bool

(**
   [is_posix_valid f]
   @return true if [f] is a valid name in a POSIX compliant OS
*)


val temp_file: ?temp_dir: string -> string -> string -> string
(**
   [temp_file ?temp_dir_name prefix suffix]

   Returns the name of a fresh temporary file in the temporary directory. The
   base name of the temporary file is formed by concatenating prefix, then, if
   needed, a 6-digit hex number, then suffix. The temporary file is created
   empty, with permissions [0o600] (readable and writable only by the file
   owner).  The file is guaranteed to be fresh, i.e. not already existing in
   the directory.

   @param temp_dir the directory in which to create the temporary file

   Note that prefix and suffix will be changed when necessary to make the final filename
   valid POSIX.
*)

(* Splits a given path in a list of strings. *)
val explode : string -> string list
(* dual to explode *)
val implode : string list -> string
val normalize_path : string list -> string list

val temp_dir: ?in_dir:string -> string -> string -> string
(**
   Same as temp_file but creates a temporary directory.
*)


val open_temp_file :
  ?temp_dir: string -> string -> string -> string * out_channel
(** Same as {!Core_filename.temp_file}, but returns both the name of a fresh
   temporary file, and an output channel opened (atomically) on
   this file.  This function is more secure than [temp_file]: there
   is no risk that the temporary file will be modified (e.g. replaced
   by a symbolic link) before the program opens it. *)


(** {3 functions from the standard library}
    For these function definitions check the standard library
*)
val current_dir_name : string
val parent_dir_name : string
(* [concat] isn't quite like the standard library.  you should read the code until we fix
 * this comment *)
val concat : string -> string -> string
val is_relative : string -> bool
val is_implicit : string -> bool
val check_suffix : string -> string -> bool
val chop_suffix : string -> string -> string
val chop_extension : string -> string

val basename : string -> string
(** Respects the posix semantic.

   Split a file name into directory name / base file name.
   [concat (dirname name) (basename name)] returns a file name
   which is equivalent to [name]. Moreover, after setting the
   current directory to [dirname name] (with {!Sys.chdir}),
   references to [basename name] (which is a relative file name)
   designate the same file as [name] before the call to {!Sys.chdir}.

   The result is not specified if the argument is not a valid file name
   (for example, under Unix if there is a NUL character in the string). *)

val dirname : string -> string
(** See {!Filename.basename}. *)

(** [split filename] returns (dirname filename, basename filename) *)
val split : string -> string * string
val temp_dir_name : string
val quote : string -> string
