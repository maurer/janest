(** Extensions to [Core.Core_filename].*)

val compare: string -> string -> int
(**
   [with_open_temp_file prefix suffix ~f]
   runs f on the output_channel pointing to the temporary file and returns the
   name of the file.
*)
val with_open_temp_file:
  ?temp_dir: string -> string -> string ->
  f: (out_channel -> unit) -> string

(**
   Runs [f] with a temporary dir as option and removes the directory afterwards.
*)

val with_temp_dir: ?in_dir:string -> string -> string -> f:(string -> 'a) -> 'a


(* till: that would be an bool option. Sadly I think it is best to keep
   things this way because this is a syntactic check. A semantic check would be
   really hard (because of links/mounts/god knows what). In practice I think
   this function is usually what programmers expect.*)
val is_parent : string -> string -> bool
(**
   [is_parent dir1 dir2]
   returns [true] if [dir1] is a parent of [dir2]

   Note: This function is context independent, use [expand] if you want to
   consider relatives paths from a given point.
   In particular:

   _A directory is always the parent of itself.
   _The root is the parent of any directory
   _An absolute path is never the parent of relative one and vice versa.
   _This function is context independent so ["../../a"] is never the parent of
   ["."] even if this could be true given taken form the current working directory.
*)

