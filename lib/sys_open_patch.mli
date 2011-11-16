(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

(** This module is here to avoid holding the runtime lock when doing an
    open_in_gen lock. This has been upstreamed as a patch in 3.12.1. *)
(** This module exists only to workaround some undesirable behavior in
    the function [caml_sys_open] in the ocaml runtime.  Delete this
    module in favor of the same functions in [Pervasives] once the
    runtime is patched to our satisfaction *)

(** Reimplementation of [Pervasives.open_in_gen] *)
val open_in_gen : open_flag list -> int -> string -> in_channel

(** Reimplementation of [Pervasives.open_out_gen] *)
val open_out_gen : open_flag list -> int -> string -> out_channel

