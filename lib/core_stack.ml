(*pp camlp4o -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core_stack"

module List = Core_list

exception Empty

type 'a t = {
  mutable elts : 'a list;
  mutable length : int;
} with bin_io

type 'a binable = 'a t
type 'a container = 'a t
type 'a sexpable = 'a t

let invariant t =
  assert (t.length = List.length t.elts);
;;

let create () = { elts = []; length = 0; }

let push t x = t.elts <- x :: t.elts; t.length <- t.length + 1

let pop t =
  match t.elts with
  | [] -> None
  | x :: l -> t.length <- t.length - 1; t.elts <- l; Some x
;;

let pop_exn t = 
  match t.elts with
  | [] -> raise Empty
  | x :: l -> t.length <- t.length - 1; t.elts <- l; x
;;

let top t =
  match t.elts with
  | [] -> None
  | x :: _ -> Some x
;;

let top_exn t =
  match t.elts with
  | [] -> raise Empty
  | x :: _ -> x
;;

let clear t = t.elts <- []

let copy t = { elts = t.elts; length = t.length; }

let length t = t.length

let is_empty t = t.length = 0

let iter t ~f = List.iter t.elts ~f

let fold t ~init ~f = List.fold t.elts ~init ~f

let exists t ~f = List.exists t.elts ~f

let for_all t ~f = List.for_all t.elts ~f

let find t ~f = List.find t.elts ~f

let to_list t = t.elts

let to_array t = Array.of_list t.elts

let sexp_of_t sexp_of_a t = Sexplib.Conv.sexp_of_list sexp_of_a (to_list t)

let t_of_sexp a_of_sexp sexp =
  let elts = Sexplib.Conv.list_of_sexp a_of_sexp sexp in
  { elts = elts; length = List.length elts; }
;;

let until_empty t f =
  let rec loop () = if t.length > 0 then (f (pop_exn t); loop ()) in
  loop ()
;;
