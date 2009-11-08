(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: sys.mli,v 1.49 2007/02/26 14:21:57 xleroy Exp $ *)

(** System interface. *)

val argv : string array
(** The command line arguments given to the process.
   The first element is the command name used to invoke the program.
   The following elements are the command-line arguments
   given to the program. *)

val executable_name : string
(** The name of the file containing the executable currently running. *)

val file_exists : string -> bool
(** Test if a file with the given name exists. *)


val is_directory : string -> bool
(** Returns [true] if the given name refers to a directory,
    [false] if it refers to another kind of file.
    Raise [Sys_error] if no file exists with the given name. *)

val remove : string -> unit
(** Remove the given file name from the file system. *)

val rename : string -> string -> unit
(** Rename a file. The first argument is the old name and the
   second is the new name. If there is already another file
   under the new name, [rename] may replace it, or raise an
   exception, depending on your operating system. *)

val getenv : string -> string option
val getenv_exn : string -> string

val command : string -> int
(** Execute the given shell command and return its exit code. *)

val chdir : string -> unit
(** Change the current working directory of the process. *)

val getcwd : unit -> string
(** Return the current working directory of the process. *)

val readdir : string -> string array
(** Return the names of all files present in the given directory.
   Names denoting the current directory and the parent directory
   (["."] and [".."] in Unix) are not returned.  Each string in the
   result is a file name rather than a complete path.  There is no
   guarantee that the name strings in the resulting array will appear
   in any specific order; they are not, in particular, guaranteed to
   appear in alphabetical order. *)

val interactive : bool ref
(** This reference is initially set to [false] in standalone
   programs and to [true] if the code is being executed under
   the interactive toplevel system [ocaml]. *)

val os_type : string
(** Operating system currently executing the Caml program. One of
-  ["Unix"] (for all Unix versions, including Linux and Mac OS X),
-  ["Win32"] (for MS-Windows, OCaml compiled with MSVC++ or Mingw),
-  ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin). *)

val word_size : int
(** Size of one word on the machine currently executing the Caml
   program, in bits: 32 or 64. *)


val max_string_length : int
(** Maximum length of a string. *)


val max_array_length : int
(** Maximum length of a normal array.  The maximum length of a float
    array is [max_array_length/2] on 32-bit machines and
    [max_array_length] on 64-bit machines. *)


exception Break
(** Exception raised on interactive interrupt if {!Sys.catch_break}
   is on. *)


val catch_break : bool -> unit
(** [catch_break] governs whether interactive interrupt (ctrl-C)
   terminates the program or raises the [Break] exception.
   Call [catch_break true] to enable raising [Break],
   and [catch_break false] to let the system
   terminate the program on user interrupt. *)


val ocaml_version : string;;
(** [ocaml_version] is the version of Objective Caml.
    It is a string of the form ["major.minor[.patchlevel][+additional-info]"],
    where [major], [minor], and [patchlevel] are integers, and
    [additional-info] is an arbitrary string. The [[.patchlevel]] and
    [[+additional-info]] parts may be absent. *)
