open Core.Std
open Printf

(* stupid module for formatting two columns *)
module Columns = struct
  type t = (string * string) list
  let align t : string list =
    let max_len =
      List.fold_left t ~init:0
        ~f:(fun acc pair -> Int.max acc (String.length (fst pair)))
    in
    let pad x =
      let n = max_len - String.length x in
      if Int.equal n 0 then x else (x ^ String.make n ' ')
    in
    List.map t ~f:(fun (x, y) -> pad x ^ "  " ^ y)
  ;;
end

module Help_page = struct

  (* A special string list sort which sorts the string "help" as the last
     element *)
  let help_last_sort list =
    let rec extract_help ?(acc=[]) list = match list with
      | hd :: tl when String.is_prefix ~prefix:"help" hd ->
          let acc = List.rev_append tl acc in
          let acc = hd :: acc in
          List.rev acc
      | hd :: tl ->
          extract_help tl ~acc:(hd :: acc)
      | [] -> List.rev acc
    in
    extract_help list
  ;;

  let render ~summary ~usage ~choice_type ~choices =
    let choices = help_last_sort choices in
    sprintf "\n%s\n\n  %s\n%s" summary usage
    (if choices = [] then "" else
      sprintf "\n  === %s ===\n\n%s\n" choice_type
        (List.fold_left choices ~init:""
          ~f:(fun acc x -> acc ^ "  " ^ x ^ "\n")))
  ;;

end

let partial_match tbl subcmd =
  let keys = String.Table.keys tbl in
  let ms = List.filter keys ~f:((=)subcmd) in
  match ms with
  | [key] -> String.Table.find tbl key
  | _ -> (* No full match, try for a partial match *)
      begin
        let ms = List.filter_map keys ~f:(fun key ->
          if String.is_prefix key ~prefix:subcmd
          then Some key
          else None) in
        match ms with
        | [key] -> String.Table.find tbl key
        | _ -> None (* No unique match *)
      end
;;

module Flag = struct

  module Type = struct (* type class for values parsable from a string *)
    type 'a t = string -> 'a
    let create f = f
    let bool = Bool.of_string
    let int = Int.of_string
    let float = Float.of_string
  end

  module Action = struct
    type 'a t =
      | Noarg of ('a -> unit)
      | Arg of ('a -> string -> unit)
      | Rest of ('a -> string list -> unit)
    let noarg f = Noarg f
    let arg f = Arg f
    let rest f = Rest f
    let of_type t f = Arg (fun x s -> f x (t s))
  end

  type 'a t = {
    name : string;
    spec : 'a Action.t;
    doc : string;
  }

  let lookup ts =
    let tbl = String.Table.create 0 in
    
    List.iter ts ~f:(fun t ->
      String.Table.replace tbl ~key:t.name ~data:t.spec);
    fun flag ->
      partial_match tbl flag
  ;;

  let help { name = name; doc = doc }  =
    if String.is_prefix doc ~prefix:" " then
      (name, String.lstrip doc)
    else
      let (arg, doc) =
        match String.lsplit2 doc ~on:' ' with
        | None -> (doc, "")
        | Some pair -> pair
      in
      (name ^ " " ^ arg, String.lstrip doc)
  ;;

end

module Explicit = struct

  type ('a, 'b) t = {
    summary : string;
    usage_arg : string;
    init : unit -> 'a;
    flags : 'a Flag.t list;
    anon : 'a -> string list -> unit;
    final : 'a -> (unit -> string) -> 'b;
    main : 'b -> int;
  }

  let summary t = t.summary

  let soft_tail = function
    | [] -> []
    | _ :: xs -> xs
  ;;

  let help ~cmd t =
    let helps = List.map ~f:Flag.help t.flags in
    Help_page.render
      ~summary:(summary t)
      ~usage:(cmd ^ " " ^ t.usage_arg)
      ~choice_type:"available flags"
      ~choices:(Columns.align helps)
  ;;

  let run t ~cmd ~args =
    let state = t.init () in
    let lookup = Flag.lookup t.flags in
    let show_help = ref false in
    let anon_args = ref [] in
    let rec process_flags = function
      | [] -> ()
      | arg :: args ->
          if not (String.is_prefix ~prefix:"-" arg) then begin
            anon_args := arg :: !anon_args;
            process_flags args
          end else begin
            let flag = arg in
            if String.equal flag "-help" then show_help := true else begin
              match lookup flag with
              | None ->
                  eprintf "%s" (help ~cmd t);
                  failwithf "unknown flag %s" flag ()
              | Some (Flag.Action.Noarg f) -> f state; process_flags args
              | Some (Flag.Action.Rest f) -> f state args
              | Some (Flag.Action.Arg f) ->
                  match args with
                  | arg :: args -> f state arg; process_flags args
                  | [] ->
                      eprintf "%s" (help ~cmd t);
                      failwithf "missing argument for flag %s" flag ()
            end
          end
    in
    process_flags args;
    if !show_help then begin
      print_string (help ~cmd t);
      0
    end
    else begin
      t.anon state (List.rev !anon_args);
      t.main (t.final state (fun () -> help ~cmd t));
    end
  ;;

  let get_flag_names t = List.map t.flags ~f:(fun f -> f.Flag.name)

end

module Implicit = struct
  (* type t = exists 'a 'b. ('a, 'b) Explicit.t *)
  type 'r cont = { k : 'a 'b. ('a, 'b) Explicit.t -> 'r }
  type t = { expose : 'r. 'r cont -> 'r }
  let create x = { expose = fun cont -> cont.k x }
  let run t ~cmd ~args = t.expose { k = fun x -> Explicit.run x ~cmd ~args }
  let help t = t.expose { k = fun x -> Explicit.help x }
  let summary t = t.expose { k = fun x -> Explicit.summary x }
  let get_flag_names t = t.expose { k = fun x -> Explicit.get_flag_names x }
end

type t =
  | Base of Implicit.t
  | Group of string * t String.Table.t (* summary, subcommands *)

let create ~summary ~usage_arg ~init ~flags ~anon ~final ~main =
  Base
    (Implicit.create
      { Explicit.
        summary = summary;
        usage_arg = usage_arg;
        init = init;
        flags = flags;
        anon = anon;
        final = final;
        main = main;
      })
;;

let group ~summary alist =
  match String.Table.of_alist alist with
  | `Ok tbl -> Group (summary, tbl)
  | `Duplicate_key name -> failwith ("multiple subcommands named " ^ name)
;;

let summary = function
  | Base implicit -> Implicit.summary implicit
  | Group (summary, _) -> summary
;;

let help ~cmd t =
  match t with
  | Base implicit ->
      Implicit.help ~cmd implicit
  | Group (mysummary, tbl) ->
      let alist =
        ("help", "display a help message for a given subcommand") ::
        List.map ~f:(fun (cmd, t) -> (cmd, summary t))
          (String.Table.to_alist tbl)
      in
      let alist =
        List.sort alist ~cmp:(fun (x,_) (y,_) -> String.compare x y)
      in
      Help_page.render
        ~summary:mysummary
        ~usage:(cmd ^ " SUBCOMMAND")
        ~choice_type:"available subcommands"
        ~choices:(Columns.align alist)
;;

let help_help ~cmd subcommands =
  printf "help_help\n%!";
  Help_page.render
    ~summary:"more detailed help on a subcommand"
    ~usage:(cmd ^ " help SUBCOMMAND")
    ~choice_type:"available subcommands"
    ~choices:(List.sort ~cmp:String.compare subcommands)
;;

let rec run_internal t ~cmd ~args =
  match t with
  | Base implicit -> Implicit.run implicit ~cmd ~args
  | Group (_, tbl) ->
      let (subcmd, rest) =
        match args with
        | [] ->
            eprintf "%s" (help ~cmd t);
            failwithf ("missing subcommand for command %s") cmd ();
        | ("-help" | "help" | "h" | "?" | "-?") :: rest ->
            begin match rest with
            | [] ->
                printf "%s" (help ~cmd t);
                exit 0
            | ("help" :: _) ->
                printf "%s" (help_help ~cmd (String.Table.keys tbl));
                exit 0
            | (sub_cmd :: rest) ->
                (sub_cmd, "-help" :: rest)
            end
        | subcmd :: rest -> (subcmd, rest)
      in
      begin match partial_match tbl subcmd with
      | None ->
          eprintf "%s" (help ~cmd t);
          failwith ("unknown subcommand " ^ subcmd ^ " for command " ^ cmd)
      | Some t ->
          run_internal t ~cmd:(cmd ^ " " ^ subcmd) ~args:rest
      end
;;

(*
    The #! protocol for stand-alone scripts groups together all embedded
    flags as one.  If the first line of a #! script reads

        #! /path/to/my/command -flag1 -flag2 -flag3

    and then we call the script as

        > script arg1 arg2

    then the argument vector passed to /path/to/my/command will be

        ["-flag1 -flag2 -flag3"; "arg1"; "arg2"]

    So we need to pull apart the first argument into three.  Likely, this
    will only happen when the first argument is a flag (starts with '-')
    and contains a space.
*)
let hash_bang_expand = function
  | (first :: rest) as same ->
      if String.is_prefix first ~prefix:"-" then
        String.split first ~on:' ' @ rest
      else
        same
  | other -> other
;;

let run ?argv t ~hash_bang_expand:expand =
  let argv = Option.value argv ~default:(Array.to_list Sys.argv) in
  match argv with
  | [] -> failwith "no command name passed in" (* I think this is impossible *)
  | cmd :: args ->
      let cmd = Filename.basename cmd in
      let args = if expand then hash_bang_expand args else args in
      run_internal t ~cmd ~args
;;

module Tab_completion = struct

  type t = {
    key: string;
    completions: string;
  }

  (* sta = subcommand t accumulator *)
  (* bta = base t accumulator *)
  let rec handle_command ~key ?(sta=[]) ?(bta=[]) command =
    match command with
    | Base command ->
        (* Extract the flag names from the base command *)
        let flag_names = Implicit.get_flag_names command in
        (* Function to concatenate strings with space delimiters *)
        let f = (fun x y -> x ^ y ^ " ") in
        (* Fold the list of flag names into a space separated string *)
        let completions = List.fold_left flag_names ~init:"" ~f in
        (* Store the key (subcommand) and flag names in a singleton list on the right,
           and since we have no group info, store [] on the left *)
        let t = {
          key = key;
          completions = completions;
        } in
        let bta = t :: bta in
        (sta, bta)
    | Group (_summary, tbl) ->
        (* Get this group's subcommands *)
        let keys = String.Table.keys tbl in
        (* Concatenates with spaces *)
        let f = (fun x y -> x ^ y ^ " ") in
        (* Folds keys into a space separated string *)
        let completions = List.fold_left keys ~init:"" ~f in
        let t = {
          key = key;
          completions = completions;
        } in
        let sta = t :: sta in
        (* Function that recursively calls handle_command with a key (subcommand name)
           and its associated command  *)
        let f = (fun ~key ~data (sta, bta) -> handle_command ~key ~sta ~bta data) in
        String.Table.fold ~f ~init:(sta, bta) tbl

  let top argv0 =
    "#!/bin/bash\n"
    ^ "_" ^ argv0 ^ "()\n"
    ^ "{\n\n"
    ^ "local cur prev\n\n"
    ^ "#The current word (probably unfinished, since we're tab-completing)\n"
    ^ "cur=\"${COMP_WORDS[COMP_CWORD]}\"\n\n"
    ^ "#The most recent, non-flag word (doesn't begin with \"-\")\n"
    ^ "prev=\"-\"; i=1\n"
    ^ "while [[ ${prev} == -* ]]; do\n"
    ^ "prev=\"${COMP_WORDS[COMP_CWORD-i]}\"\n"
    ^ "i=$i+1\n"
    ^ "done\n\n"

  let open_case =
    "case \"${prev}\" in\n"

  let close_case =
    "*)\n;;\n" ^ "esac\n\n"

  let flag_completion_header =
    "# Flag completion (cur begins with \"-\")\n"
    ^ "if [[ ${cur} == -* ]] ; then\n"
    ^ open_case

  let flag_completion_closer =
    close_case
    ^ "fi\n\n"

  let subcommand_completion_header =
    "# Subcommand completion\n"
    ^ open_case

  let subcommand_completion_closer =
    close_case

  let case_element subcommand completions =
    let tcelt completions =
      let s = sprintf "local subopts=\"%s\"\n" completions in
      let s = s ^ "COMPREPLY=( $(compgen -W \"${subopts}\" -- ${cur}) )\n" in
      s ^ "return 0\n"
    in
    subcommand ^ ")\n" ^ (tcelt completions) ^ ";;\n"

  let bottom argv0 =
    "}\n"
    ^ "complete -o default -F _" ^ argv0 ^ " " ^ argv0 ^ "\n"

  let write_script command ~argv0 ~dest =
    let script =
      let (s_tcelts, b_tcelts) = handle_command ~key:argv0 command in
      let flag_completion_elts =
        let f = (fun x tcelt -> x ^ (case_element tcelt.key tcelt.completions)) in
        List.fold_left b_tcelts ~init:"" ~f
      in
      let subcommand_completion_elts =
        let f = (fun x tcelt -> x ^ (case_element tcelt.key tcelt.completions)) in
        List.fold_left s_tcelts ~init:"" ~f
      in
      (top argv0)
      ^ flag_completion_header
      ^ flag_completion_elts
      ^ flag_completion_closer
      ^ subcommand_completion_header
      ^ subcommand_completion_elts
      ^ subcommand_completion_closer
      ^ (bottom argv0)
    in
    try
      (write_wrap ~f:(fun oc -> Out_channel.output_string oc script) dest)
    with
      e -> failwithf "Error: %s" (Exn.to_string e) ()


end


module Version = struct

  type action = Version | Build_info

  let command ~version ~build_info =
    let summary = "Print version information" in
    let usage_arg = "[-version | -build_info]" in
    let init () = ref None in
    let flags = [
      { Flag.name = "-version";
        spec = Flag.Action.noarg (fun a -> a := Some Version);
        doc = " Print the hg revision of this build and exit" };
      { Flag.name = "-build_info";
        spec = Flag.Action.noarg (fun a -> a := Some Build_info);
        doc = " Print build info as sexp and exit" };
    ] in
    let anon _a _anons = () in
    let final a _help =
      match !a with
      | None -> (* default is version *)
          eprintf "(no option given - printing version)\n%!";
          Version
      | Some action -> action in
    let main = function
      | Version -> print_endline version; 0
      | Build_info -> print_endline build_info; 0 in
    create
      ~summary
      ~usage_arg
      ~init
      ~flags
      ~anon
      ~final
      ~main

end
