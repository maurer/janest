(**
   Prompt user for input, passwords, yes/no
*)

(** [prompt display] prints display and returns one-line response from user *)
val prompt: string -> string

(** [password display] like prompt but turns off echo for user's response;
    None is returned on input error *)
val password: string -> string option

(** [confirm display true_answer] prints display and returns false unless
    user types true_answer *)
val confirm: string -> string -> bool
