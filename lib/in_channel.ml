module String = Core_string

type t = in_channel

let stdin = Pervasives.stdin

let create ?(binary = false) file =
  let flags = [Open_rdonly] in
  let flags = if binary then Open_binary :: flags else flags in
  Pervasives.open_in_gen flags 0o000 file
;;

let close = Pervasives.close_in
let close_noerr = Pervasives.close_in_noerr

let may_eof f = try Some (f ()) with End_of_file -> None

let input t ~buf ~pos ~len = Pervasives.input t buf pos len
let really_input t ~buf ~pos ~len =
  may_eof (fun () -> Pervasives.really_input t buf pos len)
let input_byte t = may_eof (fun () -> Pervasives.input_byte t)
let input_char t = may_eof (fun () -> Pervasives.input_char t)
let input_binary_int t = may_eof (fun () -> Pervasives.input_binary_int t)
let input_value t = may_eof (fun () -> Pervasives.input_value t)

let seek = Pervasives.seek_in
let pos = Pervasives.pos_in
let length = Pervasives.in_channel_length

let set_binary_mode = Pervasives.set_binary_mode_in

let input_all t =
  let buf = String.create 4096 in
  let buffer = Buffer.create 16 in
  let rec loop () =
    let len = input t ~buf ~pos:0 ~len:(String.length buf) in
    if len > 0 then begin
      Buffer.add_substring buffer buf 0 len;
      loop ();
    end
  in
  loop ();
  Buffer.contents buffer;
;;

let input_line ?(fix_win_eol=true) t =
  may_eof (fun () ->
    let line = Pervasives.input_line t in
    if fix_win_eol && String.length line > 0
      && String.nget line (-1) = '\r' then
      String.slice line 0 (-1)
    else
      line)
;;

let fold_lines ?fix_win_eol t ~init ~f =
  let rec loop ac =
    match input_line ?fix_win_eol t with
    | None -> ac
    | Some line -> loop (f ac line)
  in
  loop init
;;

let input_lines ?fix_win_eol t =
  List.rev
    (fold_lines ?fix_win_eol t ~init:[] ~f:(fun lines line -> line :: lines))
;;

let iter_lines ?fix_win_eol t ~f =
  fold_lines ?fix_win_eol t ~init:() ~f:(fun () line -> f line)
;;
