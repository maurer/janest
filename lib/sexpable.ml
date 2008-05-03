open Sexplib

let failwithf = Core_printf.failwithf

module type S = sig
  type sexpable
  val sexp_of_t : sexpable -> Sexp.t
  val t_of_sexp : Sexp.t -> sexpable
end

module type S1 = sig
  type 'a sexpable
  val sexp_of_t : ('a -> Sexp.t) -> 'a sexpable -> Sexp.t
  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a sexpable
end

module type S2 = sig
  type ('a, 'b) sexpable
  val sexp_of_t : 
    ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) sexpable -> Sexp.t
  val t_of_sexp : 
    (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) sexpable
end

module Of_stringable (M : Stringable.S)
  : S with type sexpable = M.stringable = struct
  type sexpable = M.stringable
  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> M.of_string s
    | Sexp.List _ -> failwithf "t_of_sexp %s" (Sexp.to_string sexp) ()
  let sexp_of_t t = Sexp.Atom (M.to_string t)
end
  
module To_stringable (M : S) : Stringable.S with type stringable = M.sexpable =
struct
  type stringable = M.sexpable
  let of_string = Conv.of_string__of__of_sexp M.t_of_sexp
  let to_string = Conv.string_of__of__sexp_of M.sexp_of_t
end
