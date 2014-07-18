
(* Jenga API version 3 - Monadic Style; small improvements & cleanup over version 2*)

(* This signature provides the interface between the `user-code' which
   describes the build rules etc for a specific instatnce of jenga,
   and the core jenga build system. *)

open Core.Std
open Async.Std

module Path : sig (* repo-relative path *)
  type t with sexp
  val relative : dir:t -> string -> t
  val to_string : t -> string (* repo root-relative string *)
  val to_absolute_string : t -> string
  val dirname : t -> t
  val basename : t -> string
  val the_root : t
  val root_relative : string -> t
  (* [dotdot ~dir path]
     compute relative ".."-based-path-string to reach [path] from [dir] *)
  val dotdot : dir:t -> t -> string
end

module Kind : sig
  type t = [ `File | `Directory | `Char | `Block | `Link | `Fifo | `Socket ]
end

module Glob : sig
  type t with sexp
  val create : dir:Path.t -> ?kinds: Kind.t list -> string -> t
  val create_absolute : absolute_dir:string -> ?kinds: Kind.t list -> string -> t
end

module Alias : sig
  type t with sexp
  val create : dir: Path.t -> string -> t
end

module Action : sig
  type t
  val internal : tag:Sexp.t -> func:(unit -> unit Deferred.t) -> t
  val shell : dir:Path.t -> prog:string -> args:string list -> t
  val bash : dir:Path.t -> string -> t (* convenience *)
end

module Dep : sig (* The jenga monad *)

  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val cutoff : equal:('a -> 'a -> bool) -> 'a t -> 'a t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val all : 'a t list -> 'a list t
  val all_unit : unit t list -> unit t
  val deferred : (unit -> 'a Deferred.t) -> 'a t

  val alias : Alias.t -> unit t

  val path : Path.t -> unit t

  (* Depend on a path-string (either absolute or relative, taken w.r.t. [dir] *)
  val of_path_string : dir:Path.t -> string -> unit t

  (* [source_if_it_exists] Dont treat path as a goal (i.e. don't force it to be built)
     Just depend on its contents, if it exists. It's ok if it doesn't exist. *)
  val source_if_it_exists : Path.t -> unit t

  val contents : Path.t -> string t
  val contents_absolute : path:string -> string t
  val contents_cutoff : Path.t -> string t

  val glob_listing : Glob.t -> Path.t list t
  val glob_change : Glob.t -> unit t
  val subdirs : dir:Path.t -> Path.t list t
  val file_exists : Path.t -> bool t

  val action : Action.t t -> unit t
  val action_stdout : Action.t t -> string t

  module List : sig
    val concat_map : 'a list -> f:('a -> 'b list t) -> 'b list t
  end

end

module Rule : sig
  type t
  val create : targets:Path.t list -> Action.t Dep.t -> t
  val alias : Alias.t -> unit Dep.t list -> t
  val default : dir:Path.t -> unit Dep.t list -> t
end

module Scheme : sig
  type t
  val create : tag:string -> (dir:Path.t -> Rule.t list Dep.t) -> t
end

module Env : sig
  type t = Description.Env.t
  val create :
    ?putenv:(string * string) list ->
    ?command_lookup_path:[`Replace of string list | `Extend of string list] ->
    ?build_begin : (unit -> unit Deferred.t) ->
    ?build_end : (unit -> unit Deferred.t) ->
    (string * Scheme.t option) list ->
    t
end

val verbose : unit -> bool

val run_action_now : Action.t -> unit Deferred.t
val run_action_now_stdout : Action.t -> string Deferred.t
