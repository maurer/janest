(** [normalize length_fun thing_with_length i] is just [i], unless
    [i] is negative, in which case it's [length_fun thing_with_length
    + i].  This is used by various python-style slice functions. *)
val normalize :
  length_fun:('a -> int)
    -> 'a -> int -> int

val slice :
  length_fun:('a -> int) -> sub_fun:('a -> pos:int -> len:int -> 'a)
    -> 'a -> int -> int -> 'a

(** [get_pos_len ?pos ?len length] takes an optional position and a length
    and returns [(pos', len')] specifying a subrange of [0, length-1] such that:
      pos' = match pos with None -> 0 | Some i -> i
      len' = match pos with None -> length string - pos | Some i -> i
    [get_pos_len] also checks [pos'] and [len'] for sanity, and raises
    [Invalid_arg] if they do not specify a valid subrange of [0, length-1].
*)
val get_pos_len_exn : ?pos:int -> ?len:int -> length:int -> int * int

val get_pos_len : ?pos:int -> ?len:int -> length:int -> (int * int, string) Result.t
