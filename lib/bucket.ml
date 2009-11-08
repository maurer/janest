(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Bucket"

open Std_internal

module type Contents =
sig
  type t
  include Sexpable with type sexpable = t
  include Binable with type binable = t
  include Comparable with type comparable = t
  val zero : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
end

(** Bucket-style datastucture

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

module Make (C: Contents): (S with type contents = C.t) =
  struct
    type contents = C.t
    with sexp, bin_io

    type t = { mutable level : contents; size : contents }
    with sexp, bin_io

    type sexpable = t
    type binable = t

    let make ~size ~init_level =
      let error msg =
        failwithf "Bucket.make ~size:%s ~init_level:%s: %s"
          (Sexp.to_string (C.sexp_of_t size))
          (Sexp.to_string (C.sexp_of_t init_level))
          msg ();
      in
      if C.(<) init_level C.zero then error "init_level negative";
      if C.(>) init_level size then error "init_level above bucket size";
      { level = init_level; size = size }
    ;;

    let level t = t.level

    let assert_positive name x =
      if C.(<) x C.zero
      then invalid_argf "Bucket.%s %s < 0" name (Sexp.to_string (C.sexp_of_t x)) ()

    let take t x =
      assert_positive "take" x;
      let new_level = C.(-) t.level x in
      if C.(<) new_level C.zero then
        `Unable
      else begin
        t.level <- new_level;
        `Taken
      end

    let take_at_most t x =
      assert_positive "take_at_most" x;
      let old_level = t.level in
      t.level <- C.max C.zero (C.(-) old_level x);
      C.(-) old_level t.level

    let fill t x =
      assert_positive "fill" x;
      let old_level = t.level in
      t.level <- C.min t.size (C.(+) old_level x);
      C.(-) t.level old_level
  end

module Int = Make (Int)
module Int64 = Make (Int64)
module Float = Make (Float)
