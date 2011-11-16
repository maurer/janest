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

module type S = sig
  type identifiable
  include Stringable.S with type stringable = identifiable
  include Comparable.S with type comparable = identifiable
  include Hashable.S_binable with type hashable = identifiable
  include Sexpable.S with type sexpable = identifiable
  include Binable.S with type binable = identifiable
  val pp : Format.formatter -> identifiable -> unit
end

module Of_stringable_sexpable (T : sig
  include Stringable.S
  include Sexpable.S with type sexpable = stringable
end) = struct
  module T' = struct
    include T
    type t = stringable
    include Binable.Of_stringable (T)
    let equal t t' = Core_string.equal (T.to_string t) (T.to_string t')
    let hash t = Core_string.hash (T.to_string t)
    let compare t t' = Core_string.compare (T.to_string t) (T.to_string t')
  end
  include Binable.Of_stringable (T)
  include Comparable.Make (T')
  include Hashable.Make_binable (T')
  include T
  type identifiable = T.stringable
  let pp formatter t = Core_string.pp formatter (T.to_string t)
end

module Of_stringable (T : Stringable.S) = Of_stringable_sexpable (struct
  include T
  include Sexpable.Of_stringable(T)
end)

module Of_sexpable (T : Sexpable.S) = Of_stringable_sexpable (struct
  include T
  include Sexpable.To_stringable(T)
end)
