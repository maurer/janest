(* This code is based on the MLton library set/disjoint.fun, which has the
   following copyright notice.
*)
(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)
open Core.Std

type 'a root = {
  mutable value: 'a;
  mutable rank: int;
}

let root_equal (r : _ root) r' = phys_equal r r'

type 'a t = {
  mutable parent : 'a parent;
}
and 'a parent =
| Parent of 'a t
| Root of 'a root

let create v = { parent = Root { value = v; rank = 0; }; }

let compress t =
  let rec loop t ac =
    match t.parent with
    | Root _ ->
        let p = Parent t in
        List.iter ac ~f:(fun t -> t.parent <- p);
    | Parent t' -> loop t' (t :: ac)
  in
  loop t []
;;

let root t =
  compress t;
  match t.parent with
  | Root r -> r
  | Parent t ->
      match t.parent with
      | Root r -> r
      | Parent _ -> assert false
;;
      
let get t = (root t).value

let set t v = (root t).value <- v

let same_class t1 t2 = root_equal (root t1) (root t2)

let union t1 t2 =
  let r1 = root t1 in
  let r2 = root t2 in
  if root_equal r1 r2 then
    ()
  else
    let n1 = r1.rank in
    let n2 = r2.rank in
    if n1 < n2 then
      t1.parent <- Parent t2
    else begin
      t2.parent <- Parent t1;
      if n1 = n2 then r1.rank <- r1.rank + 1;
    end
;;
