(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Result"

type ('a, 'b) t =
  | Ok of 'a
  | Error of 'b
with sexp, bin_io

type ('a, 'b) _t = ('a, 'b) t

include Monad.Make2
(struct
   type ('a, 'b) t = ('a,'b) _t

   let bind x f = match x with
     | Error _ as x -> x
     | Ok x -> f x

   let return x = Ok x
 end)

type ('a, 'b) sexpable = ('a, 'b) t
type ('a, 'b) binable = ('a, 'b) t

let fail x = Error x;;

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let ok = function
  | Ok x -> Some x
  | Error _ -> None

let error = function
  | Ok _ -> None
  | Error x -> Some x

let iter v ~f = match v with
  | Ok x -> f x
  | Error _ -> ()

let call ~f x =
  match f with
  | Ok g -> g x
  | Error _ -> ()

let apply ~f x =
  match f with
  | Ok g -> Ok (g x)
  | Error _ as z -> z

let ok_fst = function
  | Ok x -> `Fst x
  | Error x -> `Snd x
;;

let trywith f =
  try Ok (f ())
  with exn -> Error exn
