external running_byte_code :
  unit -> unit -> unit -> unit -> unit -> unit -> bool
  = "caml_running_byte_code_bc" "caml_running_byte_code_nc" "noalloc"

let running_byte_code () = running_byte_code () () () () () ()
