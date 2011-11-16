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

(* core_sys_open is the same as caml_sys_open, except it does not
   re-acquire the runtime lock until after its call to fcntl.  this code
   should be removed when this fix makes it into the caml runtime
*)
external open_desc: string -> open_flag list -> int -> int = "core_sys_open"

external open_descriptor_in  : int ->  in_channel = "caml_ml_open_descriptor_in"
external open_descriptor_out : int -> out_channel = "caml_ml_open_descriptor_out"

let open_in_gen  mode perm name = open_descriptor_in  (open_desc name mode perm)
let open_out_gen mode perm name = open_descriptor_out (open_desc name mode perm)

