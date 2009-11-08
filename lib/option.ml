(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` pa_type_conv.cmo pa_sexp_conv.cmo *)
TYPE_CONV_PATH "Option"

type 'a t = 'a option with sexp

type 'a container = 'a t
type 'a sexpable = 'a t

let is_none = function None -> true | _ -> false

let is_some = function Some _ -> true | _ -> false

let value_map o ~default ~f =
  match o with
  | Some x -> f x
  | None   -> default

let iter o ~f =
  match o with
  | None -> ()
  | Some a -> f a

let map2 o1 o2 ~f =
  match o1, o2 with
  | Some a1, Some a2 -> Some (f a1 a2)
  | _ -> None

let call x ~f =
  match f with
  | None -> ()
  | Some f -> f x

let apply x ~f =
  match f with
  | None -> None
  | Some f -> Some (f x)

let value t ~default =
  match t with
  | None -> default
  | Some x -> x
;;

let value_exn t =
  match t with
  | Some x -> x
  | None -> failwith "Option.value_exn None"
;;

let value_exn_message message t =
  match t with
  | Some x -> x
  | None -> failwith message
;;

let to_array t =
  match t with
  | None -> [||]
  | Some x -> [|x|]
;;

let to_list t =
  match t with
  | None -> []
  | Some x -> [x]
;;

let for_all t ~f =
  match t with
  | None -> true
  | Some x -> f x
;;

let exists t ~f =
  match t with
  | None -> false
  | Some x -> f x
;;

let length t =
  match t with
  | None -> 0
  | Some _ -> 1
;;

let is_empty = is_none

let fold t ~init ~f =
  match t with
  | None -> init
  | Some x -> f init x
;;

let find t ~f =
  match t with
  | None -> None
  | Some x -> if f x then Some x else None
;;

let equal f t t' =
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false

let some x = Some x

let both x y =
  match x,y with
  | Some a, Some b -> Some (a,b)
  | _ -> None

let wrap_exn f = (); fun x -> try Some (f x) with | _ -> None


include Monad.Make (struct
  type 'a t = 'a option
  let return x = Some x
  let bind o f =
    match o with
    | None -> None
    | Some x -> f x
  let failwith e = failwith e
end)

let container = {
  Container.
  length = length;
  is_empty = is_empty;
  iter = iter;
  fold = fold;
  exists = exists;
  for_all = for_all;
  find = find;
  to_list = to_list;
  to_array = to_array;
}
