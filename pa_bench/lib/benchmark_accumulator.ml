let unique_id =
  let r = ref 0 in
  fun () -> incr r; !r

(* Used to track the current libname in such a way that for functor applications, it is
   the calling libraries name that gets registered. *)
module Current_libname = struct
  let null = "<unknown>"
  let libname_ref = ref null

  let set str = libname_ref := str
  let unset () = libname_ref := null
  let get () = !libname_ref
end

module Current_bench_module_stack = struct
  let t = ref []

  let push s = t := s :: !t

  let pop_exn () = t := List.tl !t

  let to_name () =
    match !t with
    | [] -> None
    | ms -> Some (String.concat "." (List.rev ms))
end

(* This is the main data structure of this module. An [Entry.t] represents a benchmark
   along with some metadata about is position, arguments etc. *)
module Entry = struct

  type indexed_spec = {
    arg_name   : string;
    arg_values : int list;
    thunk      : int -> unit -> unit;
  }

  type test_spec =
    | Regular_thunk of (unit -> unit -> unit)
    | Indexed_thunk of indexed_spec

  type t = {
    unique_id         : int;
    code              : string;
    type_conv_path    : string;
    name              : string;
    filename          : string;
    line              : int;
    startpos          : int;
    endpos            : int;
    test_spec         : test_spec;
    bench_module_name : string option;
  }

  let compare t1 t2 = compare t1.unique_id t2.unique_id

  let get_indexed_arg_name t =
    match t.test_spec with
    | Regular_thunk _ -> None
    | Indexed_thunk {arg_name; _} -> Some arg_name

  (* Extracts module name from ["filename.ml.Module"], which is the format of [ext_name]
     as set by [typeconv]. *)
  let get_module_name_opt t =
    let str = t.type_conv_path in
    let len = String.length str in
    let rec loop i =
      if i + 4 <= len
      then
        if String.sub str i 4 = ".ml."
        then Some (String.sub str (i + 4) (len - i - 4))
        else loop (i + 1)
      else None
    in
    loop 0
end

(* Inspect command-line argument to decide if benchmarks are being run. This is called by
   the code generated by the [pa_bench] syntax to decide if the global hashtable should be
   populated. *)
let add_benchmarks_flag =
  match Array.to_list Sys.argv with
  | _name :: "-benchmarks-runner" :: _rest -> true
  | _ -> false

(* This hashtable contains all the benchmarks from all the of libraries that have been
   loaded. At the time the benchmarks are registering themselves with [pa_bench_lib] we
   don't yet know which libraries will need to be run.  *)
let libs_to_entries : (string, Entry.t list) Hashtbl.t = Hashtbl.create 10

let lookup_rev_lib ~libname =
  try Hashtbl.find libs_to_entries libname
  with Not_found -> []

let lookup_lib ~libname =
  List.rev (lookup_rev_lib ~libname)

let add_bench
      ~name
      ~code
      ~filename
      ~type_conv_path
      ~line
      ~startpos
      ~endpos
      test_spec
  =
  let libname = Current_libname.get () in
  let entry = { Entry.
    code; unique_id = unique_id ();
    type_conv_path; bench_module_name = Current_bench_module_stack.to_name ();
    name; filename; line; startpos; endpos; test_spec;
  } in
  Hashtbl.add libs_to_entries libname (entry :: lookup_rev_lib ~libname)

let add_bench_module
    ~name
    ~code:_
    ~type_conv_path:_
    ~filename:_
    ~line:_
    ~startpos:_
    ~endpos:_
    f =
  (* Running f registers the benchmarks using BENCH *)
  Current_bench_module_stack.push name;
  try
    f ();
    Current_bench_module_stack.pop_exn ();
  with ex ->
    Current_bench_module_stack.pop_exn ();
    raise ex