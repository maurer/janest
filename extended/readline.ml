open Core.Std
exception Unknown_sequence of string

type completer = (left:string -> right:string -> string list)

let char () = input_char stdin

(**
   The strong form of protectx...
   Finally is always run, even if we press [ctrl + c]

   This is used because we HAVE to restore the terminal when we exit otherwise we
   will face the wrath of very angry users!!!
*)
let unwind_protect ~f ~finally=
 let run f ()=
  match !f with
   | Some f -> f ()
   | None -> ()
 in
 let closeFun=ref (Some finally) in
 at_exit (run closeFun);
 let res=
  try
   f ()
  with e ->
   finally ();
   raise e
 in
 closeFun := None;
 finally ();
 res

let with_readline f =
  let module T = Unix.Terminal_io in
  let attr_in = T.tcgetattr Unix.stdin
  and attr_out = T.tcgetattr Unix.stdout in
  unwind_protect
    ~f:(
      fun () ->
        let attr_in = {
          attr_in with T.
          c_echo = false;
          c_icanon = false;
          c_vmin = 1;
          c_ixon = false;
        }
        and attr_out = { attr_out with T.c_icanon = false; c_vmin = 1 }
        in
        T.tcsetattr ~mode:T.TCSAFLUSH Unix.stdout attr_out;
        T.tcsetattr ~mode:T.TCSADRAIN Unix.stdin attr_in;
        f ()
    )
    ~finally:(fun () ->
                T.tcsetattr ~mode:T.TCSAFLUSH Unix.stdout attr_out;
                T.tcsetattr ~mode:T.TCSADRAIN Unix.stdin attr_in
             )


(* a cursor on a list...
   The part on the left is the list until the cursor in reverse order. The
   part on the left is the list taken from the cursor.
*)

module List_zipper = struct
  type 'a t = {
    l : 'a list;
    r : 'a list
  }

  let create l r = {
    l = List.rev l;
    r = r
  }

  let drop_before = function
    | {l = []; r= _} ->  None
    | {l = h::t ; r = r } -> Some (h,{l=t;r=r})

  let drop_after = function
    | { l = _; r = [] } -> None
    | { l = l ; r = h::t } -> Some (h,{ l=l; r=t })

  let insert_before z v = {z with l = v::z.l}

  let insert_after z v = {z with r = v::z.r}

  let previous zip =
    match drop_before zip with
    | None -> None
    | Some (e,line) -> Some (insert_after line e)

  let next zip =
    match drop_after zip with
    | None -> None
    | Some (e,line) -> Some (insert_before line e)

end

module String_zipper = struct

  type t = char List_zipper.t

  open List_zipper

  let drop_before = drop_before
  let drop_after = drop_after
  let insert_before = insert_before
  let insert_after = insert_after
  let previous = previous
  let next = next

  let contents zip =
    let ll = List.length zip.l
    and lr = List.length zip.r in
    let res = String.create (ll+lr) in
    List.iteri zip.l
      ~f:(fun i c -> res.[ll-1-i] <- c);
    List.iteri zip.r
      ~f:(fun i c -> res.[ll+i] <- c);
    res

  let left_contents zip =
    let len = List.length zip.l in
    let res = String.create len in
    List.iteri zip.l
      ~f:(fun i c -> res.[len-1-i] <- c);
    res

  let right_contents zip =
    let len = List.length zip.r in
    let res = String.create len in
    List.iteri zip.r
      ~f:(fun i c -> res.[i] <- c);
    res

  let first zip =
    {
      l = [];
      r = List.rev zip.l @ zip.r;
    }

  let last zip =
    {
      l = List.rev zip.r @ zip.l;
      r = [];
    }

  let create left right =
    {
      l = String.to_list_rev left;
      r = String.to_list right
    }

end

module LZ = List_zipper
module SZ = String_zipper

(**
   An Ecma escape sequence is two characters separated by one or two optional
   numbers.

   This reads Ecma sequences from the stdin; it doesn't however read the escape
   character ["\027"]. It is based on specifications and reverse engineering...
*)
(*
  Does not handle all the bells and whistles of Ecma-48 because we only need
  to handle what the keyboard can reasonably output.
*)
let parse_esc ()=
  let b1 = Buffer.create 4
  and b2 = Buffer.create 4
  in
  let cmd = String.create 2 in
  cmd.[0] <- char ();
  let rec aux seen_semi =
    let c = char () in
    let b = if seen_semi then b2 else b1 in
    match c with
    | ';' when not seen_semi ->
        aux true
    | '0'..'9'  ->
       Buffer.add_char b c;
        aux seen_semi
    |'~' when Buffer.length b > 0 ->
       let c = Buffer.nth b 0 in
       let b_cnt = Buffer.sub b 1 (Buffer.length b -1) in
       Buffer.clear b;
       Buffer.add_string b b_cnt;
       c
    | _ -> c
  in
  let c = aux false in
  cmd.[1] <- c;
  let quant b = match Buffer.contents b with
    | "" -> None
    | s -> Some (int_of_string s)
  in
  cmd,quant b1,quant b2

(** Emitting escape codes *)
let kill_line () = print_string "\027[2K"

let bell () = print_string "\007"
let home_cursor () =   print_string "\027[0G"
let save_cursor () = print_string "\027[s"
let unsave_cursor () = print_string "\027[u"

let print_line prompt line =
  home_cursor ();
  kill_line ();
  print_string prompt;
  print_string (SZ.left_contents line);
  save_cursor();
  print_string (SZ.right_contents line);
  unsave_cursor();
  flush stdout

(**
   Handling the history.

   We do this in a very quick and dirty way:
   _We keep two lists: current and pending and append to both;
   when pending reaches h.size we rotate rotated pending to current and place
   a new list in pending.
*)
module History = struct
  type t = {
    size : int;
    mutable current:string list;
    mutable pending:string list
  }

  let create size = {
    size = size;
    current = [];
    pending = []
  }

  let flush h =
    h.current <- [];
    h.pending <- []

  let to_list h = List.take h.current h.size

  let of_list ?(size=50) l =
    let l = List.take l size in
    {
      size = size;
      current = l;
      pending = l;
    }

  let snapshot h = h.current

  let add h v =
    h.current <- v::h.current;
    h.pending <- v::h.pending;
    if List.length h.pending > h.size then begin
      h.current <- h.pending;
      h.pending <- []
    end
end


let default_history = History.create 50

let empty_completer ~left:_ ~right:_ = []

type key =
  | Backspace
  | Tab
  | Newline
  | Char of char
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Delete
  | Eof
  | Unknown_escape of (string*int option*int option)

let read_key () = match char () with
  | '\n' -> Newline
  | '\t' -> Tab
  | '\127' -> Backspace
  | '\004' -> Eof
  | '\027' -> (* Escape sequence *)
      (match (parse_esc ()) with
       | "[A",(None | Some 1),None -> Up
       | "[B",(None | Some 1),None -> Down
       | "[D",(None | Some 1),None -> Left
       | "[C",(None | Some 1),None -> Right
       | "[3",(None | Some 1),None -> Delete
       | "OH",None,None -> Home
       | "OF",None,None -> End
       | v -> Unknown_escape v)
  | c -> Char c

let input_line ?(history = default_history)
    ?(prompt="> ") ?(tab_completion=empty_completer) ()
    =
  let rec loop hist line =
    print_line prompt line;
    match read_key () with
    | Newline ->
        print_newline ();
        SZ.contents line
    | Tab ->
        begin
          let leftp = SZ.left_contents line in
          let rightp = SZ.right_contents line in
          match tab_completion ~left:leftp ~right:rightp with
          | [left] ->
              let line = SZ.create (left) "" in
              loop hist line
          | [] ->
              bell();
              loop hist line
          | matches ->
              (* TODO: Multiple entries on one line *)
              print_newline ();
              List.iter matches ~f:print_endline;
              loop hist line
        end
    | Backspace ->
        begin
          match SZ.drop_before line with
          | None -> loop hist line
          | Some (_,l) -> loop hist l
        end
    | Delete ->
        begin
          match SZ.drop_after line with
          | None -> loop hist line
          | Some (_,l) -> loop hist l
        end
    | Up ->
        begin
          match LZ.drop_after hist with
          | None -> loop hist line
          | Some (e,h) ->
              let hist = LZ.insert_before h (SZ.contents line) in
              let line = SZ.create e "" in
              loop hist line
        end
    | Down ->
        begin
          match LZ.drop_before hist with
          | None -> loop hist line
          | Some (e,h) ->
              let hist = LZ.insert_after h (SZ.contents line) in
              let line = SZ.create e "" in
              loop hist line
        end
    | Left ->
        begin
          match SZ.previous line with
          | None -> loop hist line
          | Some line -> loop hist line
        end
    | Right ->
        begin
          match SZ.next line with
          | None -> loop hist line
          | Some line -> loop hist line
        end
    | Home -> loop hist (SZ.first line)
    | End -> loop hist  (SZ.last line)
    | Unknown_escape _ -> loop hist line
    | Eof ->  raise End_of_file
    | Char c ->
        let line = SZ.insert_before line c in
        print_char c;
        loop hist line
  in
  let line = SZ.create "" "" in
  let hist = LZ.create [] (History.snapshot history) in
  let res = with_readline (fun () -> loop hist line) in
  if res <> "" then
    History.add history res;
  res
