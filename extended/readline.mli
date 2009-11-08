(**
   Interactive line editing.

   This implements very basic [readline] abilities: backspace, left and right
   arrows work as expected.
   There's also a history that can be browsed through the [up] and [down] arrows.
*)
type completer = (left:string -> right:string -> string list)

(**
   A mutable variable representing the history.
*)
module History : sig
  type t
  val create : int -> t
  val flush : t -> unit
  val to_list : t -> string list
  val of_list : ?size:int -> string list -> t
end

val input_line :
  ?history:History.t ->
  ?prompt:string ->
  ?tab_completion:completer -> unit -> string
