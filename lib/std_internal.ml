(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core.Std_internal"

(* labeled versions *)
module Unix = Core_unix

(* original modules *)
module FSet = MoreLabels.Set
module FMap = MoreLabels.Map
module Std_unix = Unix

(* modified modules *)
module Set = PSet
module Map = PMap
include PMap.Infix
module Array = Core_array 
include Array.Infix
module Hashtbl = Core_hashtbl
include Hashtbl.Infix
module String = Core_string 
module List = Core_list
include List.Infix

module Queue = Core_queue
  
module Stack = Core_stack

module Sys = struct 
  include Sys 
  include Core_sys 
end

module Char = Core_char

module Int = Core_int
module Int32 = Core_int32
module Int64 = Core_int64
module Nativeint = Core_nativeint

(* handy shortcuts *)
include Common

include (Float : Interfaces.Robustly_comparable with type robustly_comparable = float)
(* CRv2 achlipala: This [include] of [Float] leads to some confusing error messages about
   [Std_internal.robustly_comparable]. *)
include String.Infix
(* XXX overrides of certain Pervasives functions.  We should probably stop opening
 * Pervasives and then we wouldn't have to do this. *)
(* CRv2 sweeks: What does the above comment mean?  Where is Pervasive opened? *)
let int_of_float = Float.to_int
let truncate = Float.truncate
let round = Float.round
include Interfaces
module Sexp = Core_sexp
include Sexplib.Conv
include Printf
include Scanf
