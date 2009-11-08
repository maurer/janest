open Core.Std
module S = String

(** The documentation for an embedded command. Used the generate help
    messages.*)
type descr = {
  name : string option;
  descr : string;
  arg_descr :string;
}

type 'a t = {
  run : string list -> 'a option;
  doc : descr option;
}

exception Arg_error of string

module Spec = struct
  type ok = (string list -> bool)
  let ok_empty = List.is_empty
  type ('a,'b) t = {
    f : 'res.next_ok:ok (*Recognized by the continuation (used for backtracking)*)
    -> ('a -> string list -> 'res) (* continuation *)
    -> 'b -> string list -> 'res;
    ok : ok -> ok;
    descr : string list;
  }

  let s_dscr ty =
    match ty.descr with
    | [a] -> a
    | l -> "( " ^ String.concat ~sep:" " l ^ " )"

  module Result = struct
    type ('src,'res) t = 'src -> 'res
    let string = ident
    let unit = fun () -> ""
    let ok = fun () -> "ok"
    let bool = string_of_bool
    let big_string b = Bigstring.to_string b
    let create v = v
    let int = Int.to_string_hum
    let list ?(sep="\n") f =  fun v -> String.concat ~sep (List.map ~f v)
  end

  module Infix = struct
    let ( ++ ) g f  =
      {
        f = (fun ~next_ok x -> g.f ~next_ok:(f.ok next_ok) (f.f ~next_ok x));
        ok = (fun x -> g.ok (f.ok x));
        descr = g.descr @ f.descr
      }

    let ( --> ) args res =
      { args with
          f = fun ~next_ok k -> args.f ~next_ok (fun v -> k (res v));
      }
  end

  include Infix

  let create apply = fun descr ->
    {
      f = (fun ~next_ok:_ k f l -> match l with
           | h::t -> k ((f (apply h |! Option.value_exn))) t
           | [] -> assert false);
      ok = (fun k l ->
              match l with
              | h::t -> Option.is_some (apply h) && k t
              | [] -> false);
      descr = [descr]
    }

  let int descr =
    let iof s =
      try
        Some (int_of_string s)
      with _ -> None
    in
    create iof descr

  let string descr = create Option.some descr

  let unit = {
    f = (fun ~next_ok:_ k f l -> k (f ()) l);
    ok = ident;
    descr = []
  }

  let option (e : ('k,'a -> 'k) t) =
    let ok k l = e.ok k l || k l
    in
    let f ~next_ok k f l =
        if (e.ok next_ok l) then
          e.f ~next_ok k (fun x -> f (Some x)) l
        else
          k (f None) l
    in
    { ok = ok;
      f = f;
      descr = [ "[" ^ String.concat ~sep:" "  e.descr  ^ "]" ]
    }

  let choice elts =
    let ok k l = List.exists ~f:(fun e -> e.ok k l) elts in
    let f ~next_ok k f l =
      let e = List.find_exn ~f:(fun e -> e.ok next_ok l) elts in
      e.f ~next_ok k f l
    in
    { ok = ok;
      f = f;
      descr = [ "( " ^ String.concat ~sep:" | "
                  (List.map ~f:(fun e -> String.concat ~sep:" " e.descr) elts) ^
                  " )" ]
    }

  let list (e : ('k,'a -> 'k) t) =
    let ok k =
      let rec aux l =  k l || e.ok aux l in
      aux
    in
    let f ~next_ok:end_list k f l =
      let res = ref [] in
      let okl = ok end_list in
      let acc x = res := x :: !res in
      let rec aux = fun l ->
        if end_list l then
          k (f (List.rev (!res))) l
        else
          e.f ~next_ok:okl (fun () -> aux) acc l
      in
      aux l
    in
    { ok = ok;
      f = f;
      descr =  [ s_dscr e ^ "..." ]
    }

  let map1 ~f ty  = {
    f = (fun ~next_ok k acc l ->
           ty.f ~next_ok k (fun v -> acc (f v)) l
        );
    ok = ty.ok;
    descr = ty.descr
  }

  let map2 ~f ty = {
    f = (fun ~next_ok k acc l ->
           ty.f ~next_ok k (fun v w -> acc (f v w)) l
        );
    ok = ty.ok;
    descr = ty.descr
  }

  let map3 ~f ty = {
    f = (fun ~next_ok k acc l ->
           ty.f ~next_ok k (fun v w x -> acc (f v w x)) l
        );
    ok = ty.ok;
    descr = ty.descr
  }

  let embed a f =
    a.f ~next_ok:ok_empty (fun i remaining -> assert (remaining = []); i) f

  let describe a = String.concat ~sep:" " a.descr

  let ok a = a.ok ok_empty
end

(** fill line*)
let split s pos =
  String.sub s ~pos:0 ~len:pos,
  String.sub s ~pos:(pos+1) ~len:(String.length s - pos - 1)

let tabulate ~len s =
  let rec loop acc s =
    if String.length s <= len then
      s::acc
    else
      let pos =
        try
          Some
            (try String.rindex_from s len ' '
              with Not_found -> String.index_from s len ' ')
        with Not_found -> None
      in
      match pos with
      | Some pos ->
          let line = String.sub s ~pos:0 ~len:pos in
          (* technicaly we could optimize this function by not reallocating a new string. *)
          let rest = String.sub s ~pos:(pos+1) ~len:(String.length s - pos - 1) in
          loop (line::acc) rest
      | None -> s::acc
  in
  List.concat_map (String.split s ~on:'\n')
    ~f:(fun s -> List.rev (loop [] s))

let get_sig d =
  match d.name with
  | Some v -> v ^ " " ^ S.uppercase d.arg_descr
  | None -> S.uppercase d.arg_descr

let pad s l =
  let src_len = S.length s
  and res = S.create l in
  S.blit ~src:s ~dst:res ~src_pos:0 ~dst_pos:0 ~len:src_len;
  S.fill ~pos:src_len ~len:(l-src_len) res ' ';
  res

let cmd_help ~padding cmds =
  let b = Buffer.create 17 in
  let descrs = List.filter_map ~f:(fun c -> c.doc) cmds in
  let nl = ref false in
  let pad_str = String.make (padding + 3) ' ' in
  List.iter descrs
    ~f:(fun d ->
      let descr,multiline =
        match tabulate ~len:(80 - 3 - padding) (S.capitalize d.descr) with
        | [s] -> s,false
        | l -> String.concat ~sep:("\n"^ pad_str) l,true
      in
      if !nl || multiline then
        bprintf b "\n";
      bprintf b " %s  %s\n" (pad (get_sig d) padding)
        descr;
      nl := multiline
    );
  Buffer.contents b

let embed ?cmd ~descr f ty =
  let recognizes =
      match cmd with
      | Some cmd ->
          (function
             | h::tl when h = cmd -> Spec.ok ty tl
             | _ -> false
          );
      | None -> Spec.ok ty
  and raw_run =
      match cmd with
      | Some _ ->
          (let f = Spec.embed ty f in
           fun l ->
             f (List.tl_exn l)
          );
      | None -> Spec.embed ty f
  in
  {
    run = (fun l ->
      if recognizes l then
        Some (raw_run l)
      else
        None
    );
    doc = Some {
      name = cmd;
      descr = descr;
      arg_descr = Spec.describe ty;
    }
  }

let declare ?doc cmd = {
  run = cmd;
  doc = doc
}

(**
   Global flags handling
*)

let ar1 doc =
  if doc = "" then
    None
  else
    if doc.[0] <> ' ' then
      let arg,doc =
        Option.value (String.lsplit2 doc ~on:' ') ~default:(doc,"")
      in
      Some (arg,doc)
    else
      None

let get_doc d =
  (match d,ar1 d with
   | _,None -> d
   | _,Some (_,d) -> d
  )
|! String.strip
|! String.capitalize

let sarg = function
  | key,Arg.Symbol ([],_),_ -> sprintf "%s <none>" key
  | key,Arg.Symbol (l,_),_ -> sprintf "%s {%s}" key (String.concat ~sep:"|" l)
  | key,_,doc ->
      match ar1 doc with
      | None -> key
      | Some (a,_) -> sprintf "%s %s" key (String.uppercase a)


(**CR till for till: Consolidate with spec printing*)
let arg_help ~padding args =
  let b = Buffer.create 17 in
  List.sort ~cmp:(Tuple3.compare ~cmp1:String.compare ~cmp2:compare ~cmp3:compare) args
  |! List.iter
    ~f:(fun ((_,_,doc) as a) ->
          bprintf b " %s  %s\n" (pad (sarg a) padding)
            (S.capitalize (get_doc doc))
       );
  Buffer.contents b

let run_arg global args =
  let res = ref [] in
  Arg.parse_argv ~current:(ref 0)
    (Array.of_list (""::args)) global (fun a -> res := a::!res) "";
  List.rev !res

type 'a spec = {
  cmds : 'a t list;
  global : Arg.t list;
  help : unit -> 'a;
  bad : string -> 'a
}

let spec_padding s =
  let max_len = ref 0 in
  List.iter s.global
    ~f:(fun a -> max_len := max (S.length (sarg a)) !max_len);
  List.iter s.cmds
    ~f:(fun c ->
          match c.doc with
          | None -> ()
          | Some d ->
              max_len := max (S.length (get_sig d)) !max_len
       );
  !max_len

let rec run_cmd args =
  function
    | [] -> assert false
    | h::t ->
        match h.run args with
        | Some v -> v
        | None -> run_cmd args t

let exe_name = Filename.basename (Sys.executable_name)

let run_spec specs args =
  try
    let args = run_arg specs.global args in
    run_cmd args specs.cmds
  with
  | Arg.Help _ -> specs.help ()
  | Arg.Bad s ->
      let errs = String.slice s 2 (String.index s '\n') in
      specs.bad errs

let help pg_desc specs =
  let padding =  spec_padding specs in
  let scmds = cmd_help ~padding specs.cmds in
  if specs.global = [] then
    sprintf "Usage: %s cmd [cmd_args]\nwhere cmd is one of:\n\n%s"
      pg_desc scmds
  else
    sprintf "Usage: %s [flags] cmd [cmd_args]\nwhere flags are in\n\n\
            %s\n\
            and cmd is one of:\n\n\
            %s"
      pg_desc (arg_help ~padding specs.global) scmds

let add_help ?(cmd="help") ~run specs =
  let help_cmd = ref (fun _ -> assert false) in
  let cmds =
    {
      run = (function
        | h::_ when h = cmd -> Some (!help_cmd ())
        | _ -> None);
      doc = Some {
        name = Some cmd;
        descr = "Display this help page.";
        arg_descr = "";
      };
    }::specs.cmds
  in
  help_cmd := run specs;
  { specs with
      cmds = cmds;
      help = run specs
  }

let run_gen
    ?(prog_name=exe_name)
    ?(args=List.tl_exn (Array.to_list Sys.argv))
    ?(global=[])
    cmds
    =
  let rec specs = {
    global = global;
    cmds = cmds;
    help = (fun () -> assert false);
    bad = (fun s -> eprintf "Bad argument: %s\n%!" s;
             prerr_endline (help prog_name specs);
             exit 1)
  }
  in
  let specs = add_help specs
    ~run:(fun specs () -> print_endline (help prog_name specs); exit 0);
  in
  let specs =
    { specs with
        cmds = specs.cmds @
        [{
           doc = None;
           run = (function
                    | [] ->
                        print_endline (help prog_name specs);
                        exit 0
                    | l ->
                        eprintf "unrecognised command %s\n" (
                          String.concat ~sep:" " l);
                        prerr_endline (help prog_name specs);
                        exit 1
                 )
         }]
    }
  in
  run_spec specs args

let run
    ?(prog_name=exe_name)
    ?(args=List.tl_exn (Array.to_list Sys.argv))
    ?(global=[])
    cmds
    =
  let rec specs = {
    global = global;
    cmds = cmds;
    help = (fun () -> assert false);
    bad = (fun s -> eprintf "Bad argument: %s\n%!" s;
             prerr_endline (help prog_name specs);
             exit 1)
  }
  in
  let specs = add_help specs
    ~run:(fun specs () -> print_endline (help prog_name specs); exit 0);
  in
  let specs =
    { specs with
        cmds = specs.cmds @
        [{
           doc = None;
           run = (function
                    | [] ->
                        print_endline (help prog_name specs);
                        exit 0
                    | l ->
                        eprintf "unrecognised command %s\n" (
                          String.concat ~sep:" " l);
                        prerr_endline (help prog_name specs);
                        exit 1
                 )
         }]
    }
  in
  Exn.handle_uncaught ~exit:true (fun () ->
    (match run_spec specs args with
    | "" -> ()
    | s ->  print_endline s
    )
  );
  exit 0

type shell = {
  prompt: unit -> string;
  quit: 'a.unit -> 'a;
  err: exn -> unit;
}

let default_shell = {
  prompt = (fun () -> "?");
  quit = (fun () -> exit 0);
  err = (fun e -> prerr_endline (Exn.to_string e))
}


let shell_help specs =
  let padding = spec_padding specs in
  if specs.global = [] then
    cmd_help ~padding specs.cmds
  else
    sprintf "Global flags:\n\n%s\n\nCommands\n\n%s"
      (arg_help ~padding specs.global)
      (cmd_help ~padding specs.cmds)

let shell sh ?(global = []) cmds =
  let rec specs = {
    global = global;
    cmds = cmds;
    help = (fun () -> assert false);
    bad = (fun _ -> assert false)
  }
  in
  let specs = add_help specs
    ~run:(fun specs () -> shell_help specs )
  in
  let specs = {
    specs with
      bad =
      (fun s -> eprintf "Arg error: %S\n\ usage:\n%s\n%!\n" s (shell_help specs)
        ;"");
      cmds=specs.cmds @
      [{
         run = (function
                  | [] -> prerr_endline "No command was specified\n"; Some ""
                  | t::_ -> eprintf "Unknown command: %S\n\
                       Usage:\n%s\n%!" t (shell_help specs);
                      Some ""
               );
         doc = None
       }]
  }
  in
  let get_cmdLine ~prompt () =
    try
      Readline.input_line ~prompt ()
    with End_of_file ->
      print_endline "\nexiting...";
      sh.quit ();
  in
  while true do
    flush stderr;
    flush stdout;
    let s = String.strip (get_cmdLine ~prompt:(sh.prompt ()) ()) in
    try
      let l = String.split s ~on:' ' in
      let r = run_spec specs l in
      if not (String.equal  r "") then
        print_endline r
    with e -> sh.err e
  done;
  assert false
