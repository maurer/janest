(** command-line parsing with hierarchical sub-commands *)

module Flag : sig

  module Type : sig
    type 'a t (** type class of parsable types *)
    val create : (string -> 'a) -> 'a t
    val bool : bool t
    val int : int t
    val float : float t
  end

  module Action : sig
    type 'accum t
      (** ['accum]-mutating action to perform when processing a flag *)

    val noarg : ('accum -> unit) -> 'accum t
      (** an action for a flag that takes no additional argument *)

    val arg : ('accum -> string -> unit) -> 'accum t
      (** an action for a flag that takes an additional string argument *)

    val rest : ('accum -> string list -> unit) -> 'accum t
      (** [rest f]: an action for a flag that signals the end of flag
            processing.  all remaining arguments are passed to the [f] *)

    val of_type : 'a Type.t -> ('accum -> 'a -> unit) -> 'accum t
      (** an action taking an argument of some parsable type ['a] *)
  end

  (** type of flags to a command with mutable accumulator type ['accum] *)
  type 'accum t = {
    name : string; (** name of the flag *)
    spec : 'accum Action.t; (** how to process this flag *)
    doc : string; (** short description of what this flag means *)
  }

end

(** abstract type of commands *)
type t

(**
    [create] constructs a base command from the following data:
    {ul
        {li ['a] a mutable accumulator type for gathering arguments }
        {li ['b] a composite argument type for the command, build from ['a] }
        {li [summary] a short description of what the command does }
        {li [usage_arg] an abbreviation of the arguments it expects }
        {li [init] a function that creates an mutable
                accumulator of type ['accum] }
        {li [flags] a list of command line flags together with their
                associated accumulator-mutating actions }
        {li [anon] a function that runs on all anonymous arguments after
                we scan through all the flags }
        {li [final] a function that constructs the final argument
                structure of type ['args] from the accumulated arguments.
                The second argument to the function is a function which returns
                the help for the command, which you can use when failing.
                This function should raise an exception with some explanation
                if it doesn't find all the arguments it expects. }
        {li [main] the main function, parameterized by the argument structure }
    }
*)
val create :
     summary   : string
  -> usage_arg : string
  -> init      : (unit -> 'accum)
  -> flags     : ('accum Flag.t list)
  -> anon      : ('accum -> string list -> unit)
  -> final     : ('accum -> (unit -> string) -> 'args)
  -> main      : ('args -> int)
  -> t

(**
    [group ~summary [...; (name_i, t_i); ...]] is an aggregate command
    that dispatches to one of the named sub-commands.  A ["help"]
    sub-command will also be generated for the group.
*)
val group : summary:string -> (string * t) list -> t

(** Run the command against [Sys.argv].
    The labelled argument [hash_bang_expand] should be set to [true] when we
    expect the command to run as the result of calling a #! interpreter script.
*)
val run : ?argv:string list -> t -> hash_bang_expand:bool -> int

module Tab_completion : sig

  (** [write_script t argv0 dest] generates a tab-completion bash script for
      [t], using [argv0] as the command name to tab complete on, writing the
      script file to [dest] *)
  val write_script : t -> argv0:string -> dest:string -> unit

end

module Version : sig
  (** Provides a ["version"] subcommand. *)
  (* Requiring [version] and [build_info] as arguments allows you to munge the
     strings before passing them to [command]. Also, passing in the strings
     instead of using Version_util directly prevents this module from being
     rebuilt constantly, I think(?). *)
  val command : version:string -> build_info:string -> t
end
