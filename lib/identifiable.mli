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

(** This module type is used to create various opaque identifier types. *)

module type S = sig
  type identifiable
  include Stringable.S       with type stringable = identifiable
  include Comparable.S       with type comparable = identifiable
  include Hashable.S_binable with type hashable   = identifiable
  include Sexpable.S         with type sexpable   = identifiable
  include Binable.S          with type binable    = identifiable
  val pp : Format.formatter -> identifiable -> unit  (* pretty print for top-level *)
end

(** [Of_stringable_sexpable], [Of_stringable] and [Of_sexpable] creates an identiable that
    uses string conversions for binable, sexpable, equality, hash, compare, and pp.
    Should only be used for modules where to_string is a cheap operation or where
    performance is not critical.
*)
module Of_stringable_sexpable (T : sig
  include Stringable.S
  include Sexpable.S with type sexpable = stringable
end)
  : S with type identifiable = T.stringable

module Of_stringable (T : Stringable.S)
  : S with type identifiable = T.stringable

module Of_sexpable (T : Sexpable.S)
  : S with type identifiable = T.sexpable
