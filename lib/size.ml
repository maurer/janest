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

module H = Hashtbl.Make(
  struct
    type t = Obj.t
    let equal = (==) 
      (* CR estokes: could this just be "true", since the hash is perfect? 
         Would it matter for performance *)
    let hash o = (Obj.magic o : int)
  end)
;;

let size_of_double = Obj.size (Obj.repr 1.0);;

let traverse t a f =
  let to_check = Queue.create () in
  let tbl = H.create 1_000_000 in
  let total = ref 0 in
  Queue.push (Obj.repr t) to_check;
  let a = ref a in
  let count t =
    if Obj.is_block t then begin
      if not (H.mem tbl t) then begin
        a := f t !a;
        H.add tbl t ();
        let tag = Obj.tag t in
        if tag < Obj.no_scan_tag then begin
          let n = Obj.size t in
          total := !total + n + 1;
          for i = 0 to n - 1 do
            let f = Obj.field t i in
            if Obj.is_block f then Queue.push f to_check
          done
        end else if tag = Obj.string_tag then begin
          total := !total + 1 + (Obj.size t)
        end else if tag = Obj.double_tag then begin
          total := !total + size_of_double
        end else if tag = Obj.double_array_tag then begin
          total := !total + 1 + size_of_double * (Obj.size t)
        end else begin
          incr total
        end
      end
    end else
      incr total
  in
  while not (Queue.is_empty to_check) do
    let t = Queue.take to_check in
    count t
  done;
  (!total, !a)
;;

let words o = fst (traverse o () (fun _ _ -> ()));;
let bytes o = words o * (Sys.word_size / 8);;
let kbytes o = words o / (8192 / Sys.word_size);;
