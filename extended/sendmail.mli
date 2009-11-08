


open Core.Std

val send :
  body:string
  -> ?from:string
  -> ?subject:string
  -> ?cc:string
  -> ?bcc:string
  -> to_:string  (* "to" is an OCaml keyword, so we use "to_" *)
  -> unit
  -> unit
