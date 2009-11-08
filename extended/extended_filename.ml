open Core.Std
open Filename


(** Filename comparison *)



(*
  Extension comparison:
  We have a list of lists of extension that should appear consecutive to one
  another. Our comparison function works by mapping extensions to
  (extension*int) couples, for instance "c" is mapped to "h,1" meaning it
  should come right after h.
*)
let create_extension_map l =
  List.fold_left l
    ~f:(fun init l ->
      match l with
      | [] -> init
      | idx::_ ->
          List.fold_lefti l
            ~f:(fun pos map v ->
              if Map.mem map v then
                failwithf "Extension %s is defined twice" v ();
              Map.add map
                ~key:v
                ~data:(idx,pos)
            )
            ~init
    )
    ~init:Map.empty

let extension_cmp map h1 h2 =
  let lookup e =
    Option.value (Map.find map e) ~default:(e,0)
  in
  Tuple2.compare (lookup h1) (lookup h2)
    ~cmp1:(Extended_string.collate)
    ~cmp2:(Int.compare)


let basename_compare map f1 f2 =
  let ext_split s =
    Option.value (String.lsplit2 ~on:'.' s) ~default:(s,"")
  in
  Tuple2.compare (ext_split f1) (ext_split f2)
    ~cmp1:(Extended_string.collate)
    ~cmp2:(extension_cmp map)

let filename_compare map v1 v2 =
  let v1 = Filename.explode v1
  and v2 = Filename.explode v2 in
  List.compare ~cmp:(basename_compare map) v1 v2

let extension_map = create_extension_map [["h";"c"];["mli";"ml"]]

let compare = filename_compare extension_map


let rec is_parent_path p1 p2 =
  match p1, p2 with
  | ["/"], _ -> true
  | ((h1 :: p1) as l), (h2 :: p2) ->
      (h1 = h2 && is_parent_path p1 p2)
      || (h2 <> ".." && h2 <> "/" && List.for_all l ~f:((=) parent_dir_name))
  | l, [] -> List.for_all l ~f:((=) parent_dir_name)
  | [], (h :: _) -> h <> ".." && h <> "/"

let is_parent f1 f2 =
  is_parent_path (normalize_path (explode f1)) (normalize_path (explode f2))


(**
   [with_open_temp_file prefix suffix ~f]
   runs f on the output_channel pointing to the temporary file and returns the
   name of the file.
*)
let with_open_temp_file ?temp_dir prefix suffix ~(f:out_channel -> unit) =
  let fname,oc = open_temp_file ?temp_dir prefix suffix in
  protectx oc ~f ~finally:close_out;
  fname

let with_temp_dir ?in_dir prefix suffix ~f =
  protectx (temp_dir ?in_dir prefix suffix)
    ~f
    ~finally:(fun dirname -> ignore (Sys.command (sprintf "rm -rf '%s'" dirname)))

