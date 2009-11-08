(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core.Std_internal"

(* labeled versions *)
module Unix = Core_unix

(* original modules *)
module Std_unix = Unix

(* modified modules *)
module Set = Core_set
module Map = Core_map
module Array = Core_array
include Array.Infix
module Hashtbl = Core_hashtbl
module String = Core_string
module List = Core_list
include List.Infix

module Queue = Core_queue

module Stack = Core_stack

module Sys = Core_sys

module Char = Core_char

module Int = Core_int
include Int.Infix
module Int32 = Core_int32
module Int64 = Core_int64
module Nativeint = Core_nativeint

(* handy shortcuts *)
include Common

include (Float : Interfaces.Robustly_comparable with type robustly_comparable = float)
include String.Infix
let int_of_float = Float.to_int
let truncate = Float.truncate
let round = Float.round
include Interfaces
module Sexp = Core_sexp
include Core_sexp.Sexp_option
include Core_sexp.Sexp_list
include Sexplib.Conv
include Printf
include Scanf
