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

type 'a t = 'a ref = { mutable contents : 'a }

type 'a sexpable = 'a t

let sexp_of_t sexp_of_a t = sexp_of_a !t

let t_of_sexp a_of_sexp sexp = ref (a_of_sexp sexp)

include Bin_prot.Utils.Make_binable1 (struct
  module Binable = struct
    type 'a t = 'a with bin_io
    type 'a binable = 'a t
  end

  type 'a t = 'a ref
  let to_binable t = !t
  let of_binable a = ref a
end)

let create x = ref x

let (!) = Pervasives.(!)
let (:=) = Pervasives.(:=)

let equal (t1 : 'a t) t2 = phys_equal t1 t2

let swap t1 t2 =
  let tmp = !t1 in
  t1 := !t2;
  t2 := tmp

let replace t f = t := f !t

(* container functions below *)
type 'a container = 'a t

let length _ = 1

let is_empty _ = false

let iter t ~f = f !t

let fold t ~init ~f = f init !t

let exists t ~f = f !t

let for_all t ~f = f !t

let find t ~f = let a = !t in if f a then Some a else None

let to_list t = [ !t ]

let to_array t = [| !t |]

let container =
  { Container.
    length = length;
    is_empty = is_empty;
    iter = iter;
    fold = fold;
    exists = exists;
    for_all = for_all;
    find = find;
    to_list = to_list;
    to_array = to_array;
  }
;;
