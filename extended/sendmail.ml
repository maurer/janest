(** Simple (and likely incomplete) interface for sending mail *)
open Core.Std

let sendmail_cmd = "/usr/sbin/sendmail -t -oi"

module Message = struct
  type t = {
    headers : (string * string) list;
    body : string;
  }

  let get_body t = t.body
  let get_headers t = t.headers

  let add_headers t headers = { t with headers = headers @ t.headers }

  (** converts message to string ready for sending via you favorite MTA *)
  let to_string t =
    let header_lines =
      List.map t.headers ~f:(fun (field,entry) ->
        if field = "" then sprintf "\t%s\n" entry
        else sprintf "%s: %s\n" field entry) in
    (String.concat ~sep:"" header_lines) ^ "\n" ^ t.body
  ;;

  (** removes the continuation of the headers, where a continuation is defined
      to be an initial sequence of headers with empty field names *)
  let rec remove_continuation headers =  match headers with
    | [] -> []
    | ("", _) :: tl -> remove_continuation tl
    | headers -> headers
  ;;

  let rec filter_headers_from_headers headers fields = match headers with
    | [] -> []
    | (("", _) as hd) :: tl ->
        hd :: filter_headers_from_headers tl fields
    | ((field, _) as hd) :: tl ->
        if Set.mem fields (String.lowercase field) then
          hd::(filter_headers_from_headers tl fields)
        else
          filter_headers_from_headers (remove_continuation tl)
            fields
  ;;

  let filter_headers t fields =
    let fields = Set.of_list (List.map ~f:String.lowercase fields) in
    { t with headers = filter_headers_from_headers t.headers fields }
  ;;
end

(** Invokes sendmail and sends the argument to sendmail via stdin *)
let send_raw ~headers ~body =
  let message = { Message.headers = headers; body = body; } in
  let status = ref None in
  protectx (Unix.open_process_out sendmail_cmd)
    ~f:(fun cout -> output_string cout (Message.to_string message))
    ~finally:(fun cout -> status := Some (Unix.close_process_out cout));
  match !status with
  | Some (`Exited 0) -> ()
  | Some status -> failwith
      (sprintf "Sendmail.send_text failed: %s"
          (Unix.Process_status.to_string_hum status))
  | None -> failwith "Sendmail.send_text: no status"
;;

let send ~body ?from ?subject ?cc ?bcc ~to_ () =
  let headers = Stack.create () in
  let add key value = Stack.push headers (key, value) in
  Option.iter from ~f:(fun f -> add "From" f);
  Option.iter subject ~f:(fun s -> add "Subject" s);
  Option.iter cc ~f:(fun f -> add "Cc" f);
  Option.iter bcc ~f:(fun f -> add "Bcc" f);
  add "To" to_;
  let headers = Stack.to_list headers in
  send_raw ~headers ~body
;;
    
