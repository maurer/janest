open Core.Std
open Unix

external raw_fork_exec :
  stdin : File_descr.t
  -> stdout : File_descr.t
  -> stderr : File_descr.t
  -> ?working_dir : string
  -> ?setuid : int
  -> ?setgid : int
  -> ?env : (string) array
  -> string
  -> string array
  -> Pid.t
  =  "extended_ml_spawn_bc" "extended_ml_spawn"

module Env = struct
  open String.Map
  type t = string String.Map.t

  let empty : t = empty

  let get ()  =
    Array.fold  (Unix.environment ())
      ~init:empty
      ~f:(fun env str ->
        match String.lsplit2 ~on:'=' str with
        | Some (key,data) -> add ~key ~data env
        | None ->
          failwithf
            "extended_unix.Env.get %S is not in the form of key=value"
            str
            ())

  let add ~key ~data env =
    if String.mem key '=' then
      failwithf "extended_unix.Env.add:\
  variable to export in the environment %S contains an equal sign"
        key
        ()
    else if String.mem key '\000' then
      failwithf "extended_unix.Env.add:\
  variable to export in the environment %S contains an null character"
        key
        ()
    else if String.mem data '\000' then
      failwithf "extended_unix.Env.add:\
  value (%S) to export in the environment for %S contains an null character"
        data
        key
        ()
    else
      String.Map.add ~key ~data env

  let to_string_array env =
    String.Map.to_alist env
    |! List.map ~f:(fun (k,v) -> k^"="^v)
    |! List.to_array
end

let fork_exec
    ?(stdin=Unix.stdin)
    ?(stdout=Unix.stdout)
    ?(stderr=Unix.stderr)
    ?(path_lookup=true)
    ?env
    ?working_dir
    ?setuid
    ?setgid
    prog
    args
    =
  let env = Option.map env
    ~f:(fun e ->
      let init,l = match e with
        | `Extend  l ->
          Env.get (),l
        | `Replace l ->
          Env.empty,l
      in
      List.fold_left l
        ~init
        ~f:(fun env (key,data) -> Env.add ~key ~data env)
      |! Env.to_string_array)

  and full_prog =
    if path_lookup then
      match Shell__core.which prog with
      | Some s -> s
      | None -> failwithf "fork_exec: Process not found %s"
        prog
        ()
    else
      prog
  in
  raw_fork_exec
    ~stdin
    ~stdout
    ~stderr
    ?working_dir
    ?setuid
    ?setgid
    ?env
    full_prog
    (Array.of_list (prog::args))

external seteuid : int -> unit = "extended_ml_seteuid"
external setreuid : uid:int -> euid:int -> unit = "extended_ml_setreuid"
external gettid : unit -> int = "extended_ml_gettid"

external htonl : Int32.t -> Int32.t = "extended_ml_htonl"
external ntohl : Int32.t -> Int32.t = "extended_ml_ntohl"

TEST =
  htonl (ntohl 0xdeadbeefl) = 0xdeadbeefl

type statvfs = {
  bsize: int;                           (** file system block size *)
  frsize: int;                          (** fragment size *)
  blocks: int;                          (** size of fs in frsize units *)
  bfree: int;                           (** # free blocks *)
  bavail: int;                          (** # free blocks for non-root *)
  files: int;                           (** # inodes *)
  ffree: int;                           (** # free inodes *)
  favail: int;                          (** # free inodes for non-root *)
  fsid: int;                            (** file system ID *)
  flag: int;                            (** mount flags *)
  namemax: int;                         (** maximum filename length *)
} with sexp, bin_io

(** get file system statistics *)
external statvfs : string -> statvfs = "statvfs_stub"

(** get load averages *)
external getloadavg : unit -> float * float * float = "getloadavg_stub"

module Extended_passwd = struct
  open Passwd

  let of_passwd_line_exn s =
    match String.split s ~on:':' with
    | name::passwd::uid::gid::gecos::dir::shell::[] ->
        { name = name;
          passwd = passwd;
          uid = Int.of_string uid;
          gid = Int.of_string gid;
          gecos = gecos;
          dir = dir;
          shell = shell
        }
    | _ -> failwithf "of_passwd_line: failed to parse: %s" s ()
  ;;
  let of_passwd_line s = Option.try_with (fun () -> of_passwd_line_exn s) ;;

  let of_passwd_file_exn fn =
    Exn.protectx (In_channel.create fn)
      ~f:(fun chan ->
        List.map (In_channel.input_lines chan) ~f:of_passwd_line_exn)
      ~finally:In_channel.close
  ;;

  let of_passwd_file f = Option.try_with (fun () -> of_passwd_file_exn f) ;;
end

external strptime : fmt:string -> string -> Unix.tm = "unix_strptime"

module Inet_port = struct
  type t = int with sexp

  let of_int_exn x =
    if x > 0 && x < 65536 then
      x
    else
      failwith (sprintf "%d is not a valid port number." x)

  let of_int x =
    try
      Some (of_int_exn x )
    with _ ->
      None

  let of_string_exn x =
    Int.of_string x |! of_int_exn

  let of_string x =
    try
      Some (of_string_exn x)
    with _ ->
      None

  let to_string x =
    Int.to_string x

  let to_int x =
    x

  let t_of_sexp sexp = String.t_of_sexp sexp |! of_string_exn
  let sexp_of_t t = to_string t |! String.sexp_of_t

  let _flag = Command.Spec.Arg_type.create of_string_exn
end

TEST = Inet_port.of_string "88" = Some 88
TEST = Inet_port.of_string "2378472398572" = None
TEST = Inet_port.of_int 88 = Some 88
TEST = Inet_port.of_int 872342 = None

module Mac_address = struct
  (* An efficient internal representation would be something like a 6 byte array,
     but let's use a hex string to get this off the ground. *)
  type t = string with sexp, bin_io
  let ( = ) = String.( = )
  let equal = ( = )
  let rex = Re2.Std.Re2.create_exn "[^a-f0-9]"
  let of_string s =
    let addr =
      String.lowercase s |> Re2.Std.Re2.rewrite_exn rex ~template:""
    in
    let length = String.length addr in
    if length <> 12 then
      failwithf "MAC address '%s' has the wrong length: %d" s length ();
    addr

  let to_string t =
    let rec loop acc = function
      | a::b::rest ->
        let x = String.of_char_list [a; b] in
        loop (x :: acc) rest
      | [] -> List.rev acc |! String.concat ~sep:":"
      | _ -> assert false
    in
    loop [] (String.to_list t)

  let to_string_cisco t =
    let lst = String.to_list t in
    let a = List.take lst 4 |! String.of_char_list
    and b = List.take (List.drop lst 4) 4 |! String.of_char_list
    and c = List.drop lst 8 |! String.of_char_list in
    String.concat ~sep:"." [a; b; c]
  let t_of_sexp sexp = String.t_of_sexp sexp |! of_string
  let sexp_of_t t = to_string t |! String.sexp_of_t

  let _flag = Command.Spec.Arg_type.create of_string
end

TEST = Mac_address.to_string (Mac_address.of_string "00:1d:09:68:82:0f") = "00:1d:09:68:82:0f"
TEST = Mac_address.to_string (Mac_address.of_string "00-1d-09-68-82-0f") = "00:1d:09:68:82:0f"
TEST = Mac_address.to_string (Mac_address.of_string "001d.0968.820f") = "00:1d:09:68:82:0f"
TEST = Mac_address.to_string_cisco (Mac_address.of_string "00-1d-09-68-82-0f") = "001d.0968.820f"


module Quota = struct

  type bytes  = Int63.t with sexp
  type inodes = Int63.t with sexp

  let bytes  x = x
  let inodes x = x

  type 'units limit = {
    soft  : 'units sexp_option;
    hard  : 'units sexp_option;
    grace : Time.t sexp_option;
  } with sexp

  type 'units usage = private 'units

  (* None is encoded as zero *)
  type 'units c_limit = {
    c_soft  : 'units;
    c_hard  : 'units;
    c_grace : Time.t;
  }

  let zero_bytes  = bytes  Int63.zero
  let zero_inodes = inodes Int63.zero

  let ml_limit_of_c_limit ~zero { c_soft; c_hard; c_grace } =
    { soft  = (if c_soft = zero then None else Some c_soft);
      hard  = (if c_hard = zero then None else Some c_hard);
      grace = (if c_grace = Time.epoch then None else Some c_grace); }

  let c_limit_of_ml_limit ~zero { soft; hard; grace } =
    { c_soft  = (match soft  with None -> zero | Some x -> x);
      c_hard  = (match hard  with None -> zero | Some x -> x);
      c_grace = (match grace with None -> Time.epoch | Some x -> x); }

  external quota_query
    : [ `User | `Group ]
    -> id:int
    -> path:string
    -> ( bytes c_limit * bytes usage * inodes c_limit * inodes usage)
    = "quota_query"

  external quota_modify
    : [ `User | `Group ]
    -> id:int
    -> path:string
    -> bytes  c_limit
    -> inodes c_limit
    -> unit
    = "quota_modify"

  let query user_or_group ~id ~path =
    try
      let blimit, busage, ilimit, iusage = quota_query user_or_group ~id ~path in
      Ok (ml_limit_of_c_limit ~zero:zero_bytes blimit, busage,
          ml_limit_of_c_limit ~zero:zero_inodes ilimit, iusage)
    with Unix.Unix_error _ as exn ->
      Or_error.of_exn exn

  let set user_or_group ~id ~path byte_limit inode_limit =
    try
      Ok (quota_modify user_or_group ~id ~path
            (c_limit_of_ml_limit ~zero:zero_bytes byte_limit)
            (c_limit_of_ml_limit ~zero:zero_inodes inode_limit))
    with Unix.Unix_error _ as exn ->
      Or_error.of_exn exn
end

module Mount_entry = struct
  (* see: man 3 getmntent *)
  type t = {
    fsname     : string;
    directory  : string;
    fstype     : string;
    options    : string;
    dump_freq  : int sexp_option;
    fsck_pass  : int sexp_option;
  } with sexp, fields

  let escape_seqs = [ "040", " " ;
                      "011", "\t";
                      "012", "\n";
                      "134", "\\";
                      "\\",  "\\"; ]
  let rec unescape s =
    match String.lsplit2 s ~on:'\\' with
    | None -> s
    | Some (l, r) ->
      match
        List.find_map escape_seqs ~f:(fun (prefix, replacement) ->
          Option.map (String.chop_prefix ~prefix r)
            ~f:(fun r -> l ^ replacement ^ unescape r))
      with
      | None -> l ^ "\\" ^ unescape r
      | Some ret -> ret

  let parse_optional_int = function
    | "0" -> None
    |  s  -> Some (Int.of_string s)

  let parse_line line =
    if String.is_empty line then Ok None
    else if line.[0] = '#' then Ok None
    else
      match
        List.map ~f:unescape
          (String.split_on_chars ~on:[' ';'\t'] (String.strip line))
      with
      | [] | [""] -> Ok None
      | fsname :: directory :: fstype :: options
        :: ([] | [_] | [_;_] as dump_freq_and_fsck_pass) ->
        begin
          let dump_freq, fsck_pass =
            match dump_freq_and_fsck_pass with
            | [                    ] -> None,           None
            | [dump_freq           ] -> Some dump_freq, None
            | [dump_freq; fsck_pass] -> Some dump_freq, Some fsck_pass
            | _ -> assert false
          in
          try
            let dump_freq = Option.bind dump_freq parse_optional_int in
            let fsck_pass = Option.bind fsck_pass parse_optional_int in
            if String.equal fstype "ignore"
            then Ok (None)
            else Ok (Some { fsname; directory; fstype;
                            options; dump_freq; fsck_pass })
          with exn ->
            Or_error.of_exn exn
        end
      | _ -> Or_error.error "wrong number of fields" line String.sexp_of_t

  let visible_filesystem ts =
    let add_slash = function
      | "" -> "/"
      | p  -> if p.[String.length p - 1] = '/' then p else p ^ "/"
    in
    let overlay map t =
      let remove_prefix = add_slash (directory t) in
      let rec loop map =
        match String.Map.next_key map remove_prefix with
        | None ->
          map
        | Some (key, _) ->
          if not (String.is_prefix ~prefix:remove_prefix key) then
            map
          else
            loop (String.Map.remove map key)
      in
      String.Map.add (loop map) ~key:(directory t) ~data:t
    in
    List.fold ts ~init:String.Map.empty ~f:(fun map t ->
      if not (String.is_prefix ~prefix:"/" (directory t)) then
        map
      else
        overlay map t)
end
