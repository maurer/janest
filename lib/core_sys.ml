open Printf

let getenv = Core_unix.getenv
let getenv_exn = Core_unix.getenv_exn

include struct
  open Caml.Sys
  let argv = argv
  let executable_name = executable_name
  let file_exists = file_exists
  let is_directory = is_directory
  let remove = remove
  let rename = rename
  let command = command
  let chdir = chdir
  let getcwd = getcwd
  let readdir = readdir
  let interactive = interactive
  let os_type = os_type
  let word_size = word_size
  let max_string_length = max_string_length
  let max_array_length = max_array_length
  exception Break = Break
  let catch_break = catch_break
  let ocaml_version = ocaml_version
end
