module String = Core_string
module List = Core_list
module Unix = Core_unix
let (|!) a f = f a
open Core_printf

include struct
  open Caml.Filename

  let check_suffix = check_suffix
  let chop_extension = chop_extension
  let chop_suffix = chop_suffix
  let current_dir_name = current_dir_name
  let is_implicit = is_implicit
  let is_relative = is_relative
  let parent_dir_name = parent_dir_name
  let quote = quote
  let temp_dir_name = temp_dir_name
end

let concat dir file =
  if dir = "" then "./" ^ file
  else if dir.[String.length dir - 1] = '/' then
    dir ^ file
  else
    dir ^ "/" ^ file

(*
  Fix for #0004549. (in the inria bug tracker)
*)
(* Finds the largest index i in [s] that is less than [from] and for which [f s.[i]]
   returns true. Then it returns [i+1]. Raises an exception if [from] isn't a valid index
   in [s]. *)

let string_rexists s ~f ~from:n =
  let rec loop n =
    if n = 0 then
      None
    else if f s.[n - 1] then
      Some n
    else
      loop (n - 1)
  in
  loop n
;;

let rec skip_end_slashes s ~from =
  match string_rexists s ~from ~f:(fun c -> c <> '/') with
  | Some v -> `Ends_at v
  | None   -> `All_slashes

let split = function
  | "" -> ".", "."
  | s ->
      match skip_end_slashes s ~from:(String.length s) with
      | `All_slashes -> "/", "/"
      | `Ends_at basename_end ->
          match string_rexists s ~f:(fun c -> c = '/') ~from:basename_end with
          | None -> ".", String.sub ~pos:0 ~len:basename_end s
          | Some basename_start ->
              let basename =
                String.sub s ~pos:basename_start
                  ~len:(basename_end - basename_start)
              in
              let dirname =
                match skip_end_slashes s ~from:basename_start with
                | `All_slashes -> "/"
                | `Ends_at dirname_end -> String.sub ~pos:0 ~len:dirname_end s
              in
              dirname, basename
;;

(*
  http://www.opengroup.org/onlinepubs/9699919799/utilities/basename.html
  http://www.opengroup.org/onlinepubs/9699919799/utilities/dirname.html
*)
let basename path = snd (split path)
let dirname path = fst (split path)

let explode path =
  let rec aux = function
    | "" | "." -> []
    | "/" -> ["/"]
    | path ->
        let dirname, basename = split path in
        basename :: aux dirname
  in
  List.rev (aux path)
;;

(* [max_filename_size] comes from getconf _POSIX_NAME_MAX / *)
let max_filename_size = 255



let is_posix_valid s =
  let module S = String in
  s <> "."
  && s <> ".."
  && 0 < S.length s && S.length s <= max_filename_size
  && not (S.contains s '/')
  && not (S.contains s '\000')

let implode = function
  | [] -> "."
  | "/"::rest -> "/" ^ (String.concat ~sep:"/" rest)
  | l -> String.concat ~sep:"/" l

(* Takes out all "../" and "./" in a path, except that if it's a relative path it may
   start with some "../../" stuff at the front. *)
let normalize_path p =
  List.fold_left p
    ~init:[]
    ~f:(fun acc path_element ->
      match path_element, acc with
      (* parent of root is root, and root can only appear as first part of path *)
      | "..", ["/"] -> ["/"]
      (* just pop the stack, e.g. /foo/bar/../ becomes just /foo/ *)
      | "..", h::rest when h <> ".." -> rest
      (* chains of ../../../ at beginning of relative path *)
      | _ -> path_element :: acc     (* accumulate regular dirs *)
    )
  |! List.rev

let unroot ~path f =
  if is_relative f <> is_relative path then
    failwithf "unroot ~path:%s %s: cannot work on an absolute path and a relative one" path f ();
  let rec aux = function
    | (h :: t), (h' :: t') when h = h' -> aux (t,t')
    | p, p' -> (List.map ~f:(fun _ -> parent_dir_name) p) @ p'
  in
  implode (aux (normalize_path (explode path), normalize_path (explode f)))

let normalize p =
  implode (normalize_path (explode p))


let user_home username =
  try
    match (Unix.getpwnam username).Unix.pw_dir with
    | "" -> failwithf "user's \"%s\"'s home is an empty string" username ()
    | s -> explode s
  with Not_found -> failwithf "user \"%s\" not found" username ()

let home () =
  try
    (* This can fail ; e.g. when winbind spontaneously combusts *)
    user_home (Unix.getlogin ())
  with _ ->
    match Unix.getenv "HOME" with
    | Some s -> explode s
    | None -> failwith "Your HOME environment variable is not set"

let expand ?(cwd=Sys.getcwd ()) p =
  (match explode p with
   | "/"::_ as r -> r (* absolute*)
   | "~"::t -> home () @ t
   | h::t when String.is_prefix h ~prefix:"~" ->
       user_home (String.chop_prefix h ~prefix:"~") @ t
   | l -> (explode cwd) @ l
  )
  |! normalize_path
  |! implode

let parent p = normalize (concat p parent_dir_name)

let prng = Random.State.make_self_init ()

(* try up to 1000 times to not get a Sys_error when opening a temp
   file / name: *)
let retry ?(temp_dir=temp_dir_name) ~f prefix suffix =
  let escape s =
    String.map s ~f:(function
                       | '/' | '\'' | '\000' | '\n' | '-' -> '_'
                       | c -> c)
  in
  let prefix = escape prefix in
  let suffix = escape suffix in
  let rec try_name counter =
    let name =
      if counter = 0 then
        prefix ^ suffix
      else
        let rnd = Random.State.bits prng land 0xFF_FFFF in
        (Printf.sprintf "%s%06x%s" prefix rnd suffix)
    in
    let name = concat temp_dir name in
    try
      f name
    with Sys_error _ | Unix.Unix_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in
  try_name 0

let open_temp_mode = [Open_wronly; Open_creat; Open_excl]

(* these functions are the same as the ones in the std lib but you
   can override the temporary directory you are working in.  They also try the
   exact filename specified by the user before reverting to the "try with"
   machinery.
*)

let temp_dir ?in_dir prefix suffix =
  retry ?temp_dir:in_dir prefix suffix
    ~f:(fun name -> Unix.mkdir ~perm:0o700 name; name)

let open_temp_file ?temp_dir prefix suffix =
  retry ?temp_dir prefix suffix
    ~f:(fun name -> (name, open_out_gen open_temp_mode 0o600 name))

let temp_file ?temp_dir prefix suffix =
  let (name, oc) = open_temp_file ?temp_dir prefix suffix in
  close_out oc;
  name

let root = "/"
