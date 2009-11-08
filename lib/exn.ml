(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core.Exn"

module Sexp = Sexplib.Sexp
module Conv = Sexplib.Conv

let sexp_of_exn = Conv.sexp_of_exn

type t = exn with sexp_of

exception Finally of t * t with sexp
exception Reraised of string * t with sexp

let reraise exc str =
  raise (Reraised (str, exc))

let reraisef exc format =
  Printf.ksprintf (fun str () -> reraise exc str) format

let () =
  ignore (Conv.add_exn_converter (function
    | Bin_prot.Common.Read_exc (exc, pos) ->
        Some (Sexp.List [
          Sexp.Atom "Bin_prot.Common.Read_exc";
          sexp_of_exn exc;
          Conv.sexp_of_int pos;
        ])
    | Bin_prot.Common.Read_error (err, pos) ->
        let str_err = Bin_prot.Common.ReadError.to_string err in
        Some (Sexp.List [
          Sexp.Atom "Bin_prot.Common.Read_error";
          Sexp.Atom str_err;
          Conv.sexp_of_int pos;
        ])
    | Bin_prot.Unsafe_read_c.Error err ->
        let str_err = Bin_prot.Common.ReadError.to_string err in
        Some (Sexp.List [ Sexp.Atom "Bin_prot.Common.Read_error";
                          Sexp.Atom str_err ])
    | _ -> None))

let to_string exc = Sexp.to_string_hum ~indent:2 (sexp_of_exn exc)

let sexp_of_t = sexp_of_exn

let protectx ~f x ~(finally : 'a -> unit) =
  let res =
    try f x
    with exn ->
      (try finally x with final_exn -> raise (Finally (exn, final_exn)));
      raise exn
  in
  finally x;
  res

let protect ~f ~finally = protectx ~f () ~finally

let pp ppf t = Sexp.pp_hum ppf (sexp_of_exn t)

let handle_uncaught ~exit:must_exit f =
  try f ()
  with exc ->
    Format.eprintf "@[<2>Uncaught exception:@\n@\n@[%a@]@]@." pp exc;
    if Printexc.backtrace_status () then Printexc.print_backtrace stderr;
    if must_exit then exit 1

let reraise_uncaught str func =
  try func () with
  | exn -> raise (Reraised (str, exn))


let () = Pretty_printer.register "Core.Exn.pp"
