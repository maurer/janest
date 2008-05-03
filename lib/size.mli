(* This is base on Jean-Christophe Filliatre's size.ml:
  http://www.lri.fr/~filliatr/ftp/ocaml/misc/size.ml *)
(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: size.ml,v 1.6 2007-09-13 13:50:55 filliatr Exp $ i*)


(** [traverse a init f] Will perform a bredth first traversal of all
    objects reachable from the closure of [a] (including [a]
    itself). [f] will be passed a pointer to each block encountered,
    and may use [init] as an accumulator value. [traverse] returns the
    number of words reachable from [a] and the accumulator value. *)
val traverse : 'a -> 'b -> (Obj.t -> 'b -> 'b) -> int * 'b

(** [words a] returns the number of words reachable from the closure of [a] *)
val words : 'a -> int

(** [bytes a] returns the number of bytes reachable from the closure of [a] *)
val bytes : 'a -> int

(** [kbytes a] returns the number of kbytes reachable from the closure of [a] *)
val kbytes : 'a -> int
