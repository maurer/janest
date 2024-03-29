(** OCaml record field. *)

(**/**)
module For_generated_code : sig
  (* don't use this by hand, it is only meant for pa_fields_conv *)
  type ('perm, 'record, 'field) t = {
    force_variance : 'perm -> unit;
    name : string;
    setter : ('record -> 'field -> unit) option;
    getter : ('record -> 'field);
    fset   : ('record -> 'field -> 'record);
  }
end
(**/**)

(* ['record] is the type of the record.  ['field] is the type of the
   values stored in the record field with name [name]. ['perm] is a way
   of restricting the operations that can be used. *)
type ('perm, 'record, 'field) t_with_perm =
| Field of ('perm, 'record, 'field) For_generated_code.t

(* a record field with no restriction *)
type ('record, 'field) t = ([ `Read | `Set_and_create], 'record, 'field) t_with_perm

(* a record that can only be read, because it belongs to a private type *)
type ('record, 'field) readonly_t = ([ `Read ], 'record, 'field) t_with_perm

val name : (_, _, _) t_with_perm -> string
val get  : (_, 'r, 'a) t_with_perm -> 'r -> 'a
val fset : ([> `Set_and_create], 'r, 'a) t_with_perm -> 'r -> 'a -> 'r
val setter : ([> `Set_and_create], 'r, 'a) t_with_perm -> ('r -> 'a -> unit) option

type ('perm, 'record, 'result) user =
    { f : 'field. ('perm, 'record, 'field) t_with_perm -> 'result }
