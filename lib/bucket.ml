(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Bucket"

open Std_internal

module type ContentsType =
sig
  type t
  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Comparable with type comparable = t
  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
end

(* Bucket-style datastucture

   [make ~size ~init_level]
   Create a new bucket. Fails if init_level is not within bounds [zero;size].

   [level t]
   Get the current bucket level.

   [take t x]
   Take some exact amount out of the bucket and return `Taken. If there is not
   enough left in the bucket, return `Unable.

   [take_at_most t x]
   Take some amount out of the bucket, possibly emptying it. The return value
   is the amount that was actually taken out.

   [fill t x]
   Put some amount into the bucket, possibly overflowing. The return value
   is the amount that was actually added to the bucket.
*)

module type S =
  sig
    type contents
    type t
    include Sexpable with type sexpable = t
    include Binable with type binable = t

    val make : size:contents -> init_level:contents -> t
    val level : t -> contents
    val take : t -> contents -> [ `Taken | `Unable ]
    val take_at_most : t -> contents -> contents
    val fill : t -> contents -> contents
  end

module Make(C: ContentsType): (S with type contents = C.t) =
  struct
    type contents = C.t
    with sexp, bin_io

    type t = { mutable level : contents; size : contents }
    with sexp, bin_io

    type sexpable = t
    type binable = t

    let make ~size ~init_level =
      if not (C.(<=) C.zero init_level)
      then failwith "Bucket.make: init_level not positive or zero";
      if not (C.(<=) init_level size)
      then failwith "Bucket.make: init_level above bucket size";
      { level = init_level; size = size }

    let level t = t.level

    let take t x =
      let new_level = C.sub t.level x in
      if C.(<=) C.zero new_level
      then begin
        t.level <- new_level;
        `Taken
      end
      else `Unable

    let take_at_most t x =
      let old_level = t.level in
      t.level <- C.max C.zero (C.sub old_level x);
      C.sub old_level t.level

    let fill t x =
      let old_level = t.level in
      t.level <- C.min t.size (C.add old_level x);
      C.sub t.level old_level
  end

module Int = Make (Int)
module Int64 = Make (Int64)
module Float = Make (Float)
