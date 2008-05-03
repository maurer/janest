include Printf

(**
   failwith and invalid_arg accepting printf's format.
*)
let failwithf f = ksprintf (fun s () -> failwith s) f
let invalid_argf f = ksprintf (fun s () -> invalid_arg s) f
