(* Interned strings.  That is, strings with constant-time (pointer-equality)
   equality function. *)

(* CRv2 | CF: I have the following non-release-critical complaints: *)
(* The "all" table is created upon functor instantiation, and never goes away, leaking
   memory.  The best solution would be to use weak pointers. *)
(* YM: I kind of disagree about this.  A 16-wide table created per instantiation of the
   functor seems really cheap to me. *)
(* CF: It's not the 16-wide table that I'm worried about; it's the gazillion strings that
   have been interned. *)
(* I've never heard this word "interned".  I think most people are more familiar with
   "hashcons". *)
(* I don't see why this module should be specialized to strings. *)
(* See http://www.lri.fr/~filliatr/ftp/ocaml/misc/ for a better hashconsing
   implementation. *)

(* CRv2 | YM: this isn't threadsafe, and so makes me rather nervous.  Are we sure this 
   is a good idea?  It seems like a lurking race condition which is really easy to miss, 
   since the use of these names doesn't _feel_ like something that one should be worried 
   about doing concurrently.... *)

open Interfaces

module type S = sig
  type t 
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t
  include Hashable with type hashable = t
  val compare : t -> t -> int
end

module Make (S : sig end): S = struct

  type t = {
    string : string;
    hash : int;
  }
  type sexpable = t
  type stringable = t

  module String_table = Core_string.Table

  let all : t String_table.t = String_table.create 16

  let equal (t : t) t' = t == t'

  let hash t = t.hash

  let of_string s = 
    match String_table.find all s with
    | None -> 
        let t = {
          string = s;
          (* CRv2 | OG: shouldn't we just use String.hash here?  It's equivalent, but
             clearer and makes it easier to modify our string-hashing algorithm later *)
          hash = Hashtbl.hash_param 1 1 s;
        } in
        String_table.add all ~key:s ~data:t;
        t
    | Some t -> t

  let t_of_sexp x = of_string (Core_string.t_of_sexp x)

  let to_string t = t.string

  let sexp_of_t t = Core_string.sexp_of_t (to_string t)

  let compare t1 t2 = 
    if equal t1 t2 then
      0
    else
      String.compare t1.string t2.string

  include Hashable.Make (struct
    type z = t
    type t = z
    let equal = equal
    let hash = hash
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)
end
