(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
(* Conversions between units of measure based on bytes. *)

TYPE_CONV_PATH "Byte_units"

module I = Core_int63

module T = struct
  type t = [
      | `Bytes of I.t
      | `Kilobytes of float
      | `Megabytes of float
      | `Gigabytes of float
      | `Words of I.t
  ] with bin_io, sexp

  type sexpable = t
  type binable = t

  let bytes_per_word =
    let module W = Word_size in
    match W.word_size with
    | W.W32 -> I.of_int 4
    | W.W64 -> I.of_int 8
  ;;

  let kbyte = 1024.
  let mbyte = kbyte *. kbyte
  let gbyte = kbyte *. mbyte
  
  let bytes_float = function
    | `Bytes n -> I.to_float n
    | `Kilobytes n -> n *. kbyte
    | `Megabytes n -> n *. mbyte
    | `Gigabytes n -> n *. gbyte
    | `Words n -> I.to_float (I.( * ) n bytes_per_word)


  let bytes t = I.of_float (bytes_float t)
  let kilobytes t = bytes_float t /. kbyte
  let megabytes t = bytes_float t /. mbyte
  let gigabytes t = bytes_float t /. gbyte
  let words t = I.(/) (bytes t) bytes_per_word

  let compare t1 t2 = Pervasives.compare (bytes_float t1) (bytes_float t2)
  let equal t1 t2 = bytes_float t1 = bytes_float t2
  let hash = Hashtbl.hash
end

include T
include Comparable.Make(T)
include Hashable.Make(T)
