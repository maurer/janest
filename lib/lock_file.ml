open Std_internal
module Unix = Core_unix


  
let create_internal ~mode ~path ~message =
  try
    let fd = Unix.openfile path ~mode:[Unix.O_RDWR; Unix.O_CREAT] ~perm:0o664 in
    let locked = ref false in
    protectx fd ~f:(fun fd ->
      
      Unix.lockf fd ~mode ~len:Int64.zero;
      Unix.ftruncate fd ~len:Int64.zero;
      let oc = Unix.out_channel_of_descr fd in
      fprintf oc "%s\n%!" message;
      Unix.lockf fd ~mode ~len:Int64.zero;
      locked := true
    ) ~finally:(fun fd -> if not !locked then Unix.close fd);
    !locked
  with Unix.Unix_error (_unix_error_type , _fn, _arg) -> false
;;

let create ~path ~message = create_internal ~mode:Unix.F_TLOCK ~path ~message;;

let create_exn ~path ~message =
  let success = create ~path ~message in
  if not success then
    failwithf "Lock_file.create_exn ~path:%s was unable to acquire the lock"
      path ()
;;

let pid_message () = sprintf "%d" (Unix.getpid ())
  
let create_pid ~path = create ~path ~message:(pid_message ())

let create_pid_exn ~path = create_exn ~path ~message:(pid_message ())

let blocking_create ~path ~message =
  let success = create_internal ~mode:Unix.F_LOCK ~path ~message in
  if not success then
    failwithf "Lock_file.blocking_create ~path:%s was unable to acquire the lock"
      path ()
;;

let is_locked path =
  try
    let fd = Unix.openfile path ~mode:[Unix.O_RDWR] ~perm:0o664 in
    protectx fd ~finally:Unix.close ~f:(fun fd ->
      Unix.lockf fd ~mode:Unix.F_TEST ~len:Int64.zero;
      false)
  with
  | Unix.Unix_error (err, _, _) as exn ->
      match err with
      | Unix.ENOENT -> false
      
      | Unix.EACCES | Unix.EAGAIN -> true
      | _ -> raise exn
;;
