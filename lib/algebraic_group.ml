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

(* An abelian group

   To avoid confusing the mathematicians, an implementation of this interface should have
   the following properties:
   i) associativity: (a+b)+c = a+(b+c) for all elt's a,b,c
   ii) identity: zero+a = a+zero = a for all elt's a
   iii) inverses: given any elt a there exists a (unique) elt b such that a+b=b+a=zero
   Note closure is enforced.  Properties (i)-(iii) aren't explicitly enforced.

   Additionally, Ron indicated we probably want
   iv) commutativity: a+b=b+a
*)



module type S = sig
  type elt with sexp  (* an element of the group *)

  val zero : elt
  val (+)  : elt -> elt -> elt
  val (-)  : elt -> elt -> elt
end
