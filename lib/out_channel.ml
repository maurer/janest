type t = out_channel

let stdout = Pervasives.stdout
let stderr = Pervasives.stderr

let create ?(binary = false) ?(append = false) ?(perm = 0o644) file =
  let flags = [Open_wronly; Open_creat] in
  let flags = if binary then Open_binary :: flags else flags in
  let flags = if append then Open_append :: flags else Open_trunc :: flags in
  Pervasives.open_out_gen flags perm file
;;

let close = Pervasives.close_out
let close_noerr = Pervasives.close_out_noerr

let set_binary_mode = Pervasives.set_binary_mode_out

let flush = Pervasives.flush

let output t ~buf ~pos ~len = Pervasives.output t buf pos len
let output_string = Pervasives.output_string
let output_char = Pervasives.output_char
let output_byte = Pervasives.output_byte
let output_binary_int = Pervasives.output_binary_int
let output_value = Pervasives.output_value

let seek = Pervasives.seek_out
let pos = Pervasives.pos_out
let length = Pervasives.out_channel_length
