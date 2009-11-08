(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Unique_id"



open Std_internal

module type Unit_ref = sig
  (* note, use equal (in this module) for comparison, poly compare
     will not work correctly. Use the hashtbl included with this
     module. *)

  type t with sexp_of

  include Hashable with type hashable = t

  val create : unit -> t
  val to_string : t -> string
end

module type Id = sig
  type t

  include Binable with type binable = t
  include Comparable with type comparable = t
  include Floatable with type floatable = t
  include Hashable with type hashable = t
  include Sexpable with type sexpable = t
  include Stringable with type stringable = t

  val create : unit -> t
end


module Unit_ref (Z : sig end) : Unit_ref = struct
    
  
  type t = {
    i : int
  } with sexp

  include Hashable.Make (struct
    type z = t with sexp (* to avoid cycle *)
    type t = z with sexp
    type sexpable = t
    let hash t = t.i
    let equal t1 t2 = phys_equal t1 t2
  end)

  

  let x = ref 0

  let create () = 
    let z = !x in
    incr x;
    {i = z}

  let to_string t = Int.to_string t.i
end

(* Only "make" can cause a context-switch that might lead to a race.
   Thus we have to check whether the contents of the cell remained
   unchanged across this call.  The subsequent comparison, dereferencing
   and assignment cannot cause context switches.  If the contents of the
   cell had changed, we will have to try again to obtain a unique id.
   This is essentially like a spin-lock and is virtually guaranteed to
   succeed quickly. *)
let rec race_free_create_loop cell make =
  let x = !cell in
  let new_x = make x in
  if phys_equal !cell x then begin cell := new_x; x end
  else race_free_create_loop cell make


module Int63 (Z : sig end) : Id = struct
  include Core_int63

  let current = ref zero
  let create () = race_free_create_loop current succ
end


module Int64 (Z : sig end) : Id = struct
  include Core_int64

  let current = ref zero
  let create () = race_free_create_loop current succ
end
