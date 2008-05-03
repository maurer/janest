module Hashtbl = MoreLabels.Hashtbl

(** The type used to store function results for memoization. *)
type 'a memo_store  = Rval of 'a | Expt of exn

let ident f =
  let store = ref None in
  let doit arg =
    try
      let rval = f arg in
      store := Some (arg,Rval rval);
      rval
    with
      | Sys.Break as e -> raise e
      | e ->
	  store := Some (arg,Expt e);
	  raise e
  in
  (fun arg ->
     match !store with
       | Some (oldarg,oldrval) ->
	   if oldarg = arg
	   then
	     match oldrval with
		 Rval rval -> rval
	       | Expt e -> raise e
	   else doit arg
       | None -> doit arg
  )

let unit f =
  let l = Lazy.lazy_from_fun f in
  (fun () -> Lazy.force l)

let general f =
  let store = Hashtbl.create 0 in
  (fun x ->
     try
       (match Hashtbl.find store x with
	  | Rval rval -> rval
	  | Expt e -> raise e )
     with Not_found ->
       try
	 let rval = f x in
	 Hashtbl.add store ~key:x ~data:(Rval rval);
	 rval
       with
	 | Sys.Break as e -> raise e
	 | e ->
	     Hashtbl.add store ~key:x ~data:(Expt e);
	     raise e
  )
