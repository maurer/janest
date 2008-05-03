(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` pa_type_conv.cmo pa_sexp_conv.cmo *)
TYPE_CONV_PATH "Result"

type ('a, 'b) t =
  | Ok of 'a
  | Error of 'b
with sexp

type ('a, 'b) sexpable = ('a, 'b) t

let bind x f = match x with
  | Error _ as x -> x
  | Ok x -> f x

let return x = Ok x

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let iter ~f = function
  | Ok x -> f x
  | Error _ -> ()

let map ~f = function
  | Ok x -> Ok (f x)
  | Error _ as z -> z

let call ~f x =
  match f with
  | Ok g -> g x 
  | Error _ -> ()

let apply ~f x = 
  match f with
  | Ok g -> Ok (g x)
  | Error _ as z -> z
