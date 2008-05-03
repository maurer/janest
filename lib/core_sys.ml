(* CRv2 sweeks: add an mli and put in all the Caml.Sys functions that we want *)
open Printf

let getenv = Core_unix.getenv
let getenv_exn = Core_unix.getenv_exn
