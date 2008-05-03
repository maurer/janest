open Sexplib.Sexp
open Printf

type t = exn

exception Finally of t * t

let converters = ref []
  
let rec to_string e =
  match e with
  | Finally (e1,e2) -> sprintf "Finally (%s,%s)" (to_string e1) (to_string e2)
  | _ ->
      let rec loop = function
        | [] -> Printexc.to_string e
        | cvt :: tl ->
            match cvt e with
            | None -> loop tl
            | Some s -> s
      in
      loop !converters
;;

let sexp_of_t e = Sexplib.Sexp.Atom (to_string e)

let register_converter f = converters := f :: !converters

let protectx ~f x ~(finally : 'a -> unit) =
  let res =
    try f x
    with exn ->
      (try finally x with final_exn -> raise (Finally (exn, final_exn)));
      raise exn
  in
  finally x;
  res
;;

let protect = protectx ()

let pp ppf t = Format.pp_print_string ppf (to_string t)

let () = Pretty_printer.register "Core.Exn.pp"

let () =
  register_converter
    (function
      | Failure s -> Some (sprintf "Failure (%s)" s)
      | Invalid_argument s -> Some (sprintf "Invalid_argument (%s)" s)
      | Unix.Unix_error (err,s,t) ->
          Some (sprintf "Unix_error(%s, %s, %s)" (Unix.error_message err) s t)
      | Sexplib.Sexp.ParseError pe ->
          let ppos = pe.parse_state.parse_pos in
          Some (
            sprintf "\
             ParseError { \
               location = %S; \
               err_msg = %S; \
               text_line = %d; \
               text_pos = %d; \
               buf_pos = %d }"
              pe.location
              pe.err_msg
              ppos.text_line
              ppos.text_char
              ppos.buf_pos)
      | Sexplib.Conv.Of_sexp_error (reason,sexp) ->
          Some (
            sprintf "Of_sexp_error (\"%s\", %s)"
              reason (Sexplib.Sexp.to_string_hum sexp))
      | Bin_prot.Common.Read_error (err, pos) ->
          let str_err = Bin_prot.Common.ReadError.to_string err in
          let str = sprintf "Bin_prot.Common.ReadError: (%s, %d)" str_err pos in
          Some str
      | Bin_prot.Unsafe_read_c.Error err ->
          let str_err = Bin_prot.Common.ReadError.to_string err in
          let str = sprintf "Bin_prot.Unsafe_read_c.Error: %s" str_err in
          Some str
      | _ -> None)
;;
