include Caml.Filename

open Std_internal

let make_absolute file ~dir =
  if Filename.is_relative file then
    dir ^/ file
  else
    file
;;

(* CRv2 sweeks: where does this constant come from? *)
let max_filename_size = 255

(**
   [is_posix_valid f]
   @returns true if [f] is a valid name in a POSIX compliant OS
*)
let is_posix_valid s =
  let module S = String in
  s <> "."
  && s <> ".." 
  && 0 < S.length s && S.length s <= max_filename_size
  && not (S.contains s '/')
  && not (S.contains s '\000')
;;

let prng = Random.State.make_self_init ()

(* try up to 1000 times to not get a Sys_error when opening a temp
   file / name: *)
let retry ?(temp_dir=temp_dir_name) ~f prefix suffix =
  let rec try_name counter =
    let name =
      if counter = 0 then
        prefix ^ suffix
      else
        let rnd = Random.State.bits prng land 0xFFFFFF in
        (Printf.sprintf ".%s%06x%s" prefix rnd suffix)
    in
    let name =
      String.map name ~f:(function '/' | '\'' | '\n' | '-' -> '_' | c -> c)
    in
    let name = temp_dir ^/ name in
    try
      f name
    with Sys_error _ | Unix.Unix_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in
  try_name 0
;;

let open_temp_mode = [Open_wronly; Open_creat; Open_excl]

let temp_file ?temp_dir prefix suffix =
  retry ?temp_dir prefix suffix
    ~f:(fun name -> close_out (open_out_gen open_temp_mode 0o600 name); name)
;;


let temp_dir ?in_dir prefix suffix =
  retry ?temp_dir:in_dir prefix suffix
    ~f:(fun name -> Unix.mkdir ~perm:0o700 name; name)
;;

let open_temp_file ?(mode = [Open_wronly;Open_creat;Open_excl])
    ?temp_dir prefix suffix =
  retry ?temp_dir prefix suffix
    ~f:(fun name -> (name, open_out_gen (mode @ open_temp_mode)  0o600 name))
;;

