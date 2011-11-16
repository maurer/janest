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

open Std_internal

module T = struct
  type t = string * int with sexp, bin_io
  type binable = t
  type sexpable = t
  let compare = Pervasives.compare
end

include T
type identifiable = t

let create ~host ~port = (host, port)

let host = fst
let port = snd

type stringable = t
let to_string (host, port) = sprintf "%s:%d" host port
let of_string s =
  match String.split s ~on:':' with
  | [host; port] ->
    let port =
      try Int.of_string port
      with _exn -> failwithf "Host_and_port.of_string: bad port: %s" s ()
    in
    host, port
  | _ -> failwithf "Host_and_port.of_string: %s" s ()

let pp ppf t = Format.fprintf ppf "%s" (to_string t)
let () = Pretty_printer.register "Core.Host_and_port.t"

include Hashable.Make_binable (struct
  include T
  let hash = Hashtbl.hash
end)
include Comparable.Make (T)

