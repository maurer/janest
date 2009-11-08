(** Interface to Unix utility functions *)

open Core.Std
open Unix

(** {2 Handling RAM limits}

    @author Sam Steingold <sds\@janestcapital.com>
*)

val physical_ram : unit -> int64
(** [physical_ram ()] @return the total amount of physical RAM in bytes. *)

val ram_limit_spec : Arg.t
(** [ram_limit_spec] command line arguments to set ram limits. *)


(** {2 Signal handling}

    @author Markus Mottl <mmottl\@janestcapital.com>
*)

val wrap_block_signals : (unit -> 'a) -> 'a
(** [wrap_block_signals f] blocks all signals before execution of [f], and
    restores them afterwards. *)

