open Core.Std
(**
   Command line argument handling by projecting ocaml functions.
*)

(**
   {3 Introduction }
   A trivial sample program could look like:
{[
open Core_extended.Std
open Dispatch_arg.Spec

let e = Dispatch_arg.embed

let () =
  Dispatch_arg.run
    \[
      e (fun x y -> x + y)
        (int "x" ++ int "y" --> Result.int)
        ~cmd:"add"
        ~descr:"add two integers";
      e (fun x y -> x - y)
        (int "x" ++ int "y" --> Result.int)
        ~cmd:"sub"
        ~descr:"substract two integers"

    \]
]}

   This embeds the [(-)] and the [(+)] for the command line. The embedding is
   done based on the functions signatures.

   Commands are usually embedded using an OCaml representation of their type.
*)

(** {3 Base types} *)

type descr = {
  name : string option;
  descr : string;
  arg_descr :string;
}

(** This is the low level representation of a command.*)
type 'res t = {
  run : string list -> 'res option;
  doc : descr option
}

(**
   This module defines functional unparsing style combinators used to embed our
   callback functions.

*)
module Spec : sig

  (** A type representation used to embed functions. A [('a,'b) t] is a spec
      which given a value of type ['b] projects to a value of type ['a].
  *)
  type ('a,'b) t

  (** Represent the return value of the embedded command*)
  module Result : sig
    type ('src,'res) t
    val unit : (unit,string) t
    val ok : (unit,string) t (* Prints "ok"*)
    val bool : (bool,string) t
    val big_string : (Bigstring.t,string) t
    val string : (string,string) t
    val int : (int,string) t
    val list : ?sep:string -> ('a,string) t -> ('a list,string) t
    val create : ('src -> 'res) -> ('src,'res) t
  end
  module Infix : sig
    val (++) : ('b,'c) t -> ('a,'b) t  -> ('a,'c) t
    val (-->) : ('a,'b) t -> ('a,'res) Result.t -> ('res,'b) t
  end
  val (++) : ('b,'c) t -> ('a,'b) t  -> ('a,'c) t
  val (-->) : ('a,'b) t -> ('a,'res) Result.t -> ('res,'b) t

  (** {3 Atomic types} *)

  (** Reads an argument from the argument list and passes it verbatim to the
      underlying function. The string passed at creation is used for
      documentation purposes.*)
  val string : string -> ('a,string -> 'a) t

  (** Same as [string] but attempts to perform an [int_to_string] conversion *)
  val int : string -> ('a,int -> 'a) t

  (** Produces a unit; reads nothing from the command line*)
  val unit : ('a,unit -> 'a) t

  (** {3 Parametrised types} *)

  val list : (unit,'a -> unit) t -> ('b,'a list -> 'b) t
  val option : ('b,'a -> 'b) t -> ('b,'a option -> 'b) t
  val choice :  ('a, 'b) t list -> ('a, 'b) t

  (** {3 Higher order functions}*)


  (** Create a new atomic type
      The option should have no side effect!!!
  *)
  val create : (string -> 'a option) -> ( string -> ('b,'a -> 'b) t)

  val map1 : f:('a -> 'b)
    -> ('c, 'a -> 'c) t
    -> ('c, 'b -> 'c) t

  val map2 : f:('a1 -> 'a2 -> 'b)
    -> ('c, 'a1 -> 'a2 -> 'c) t
    -> ('c, 'b -> 'c) t

  val map3 : f:('a1 -> 'a2 -> 'a3 -> 'b)
    -> ('c, 'a1 -> 'a2 -> 'a3 -> 'c) t
    -> ('c, 'b -> 'c) t

end

val embed : ?cmd:string -> descr:string -> 'a -> ('res,'a) Spec.t -> 'res t

(** Manual declaration; should be kept only for advanced uses *)
val declare :
  ?doc:descr ->
  (string list -> 'res option) ->
  'res t

(** Run a command
    If args is not specified then defaults to the command line arguments
    @param global is a list of flags that are run whatever is the command you use.
*)
val run :
  ?prog_name:string ->
  ?args:string list ->
  ?global:Arg.t list->
  string t list -> 'never_returns

val run_gen :
  ?prog_name:string ->
  ?args:string list ->
  ?global:Arg.t list->
  'a t list -> 'a

(** {3 Shells}
    The following functions are used to define interactive shells.
*)

(** The type used to represent shell configurations *)
type shell = {
  prompt: unit -> string; (** A command that returns the prompt*)

  (** The function used to exit the shell. It should be a function that never
      returns (e.g. raising an exception or exiting the program.
  *)
  quit: 'whatever.unit -> 'whatever
                         (*This is a function that never returns.
                           E.G. Pervasives.exit or raise*);

  (** The default error handler called when exceptions happen in
      commands. *)
  err: exn -> unit;
}

val default_shell : shell

(** Dispatch a new shell. This function never returns.*)
val shell : shell -> ?global:Arg.t list-> string t list -> _
