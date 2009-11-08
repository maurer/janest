open Core.Std;;

(* Natural ordering like found in gnome nautilus, the mac finder etc...
   Refer to Mli for more documentation
*)
let collate s1 s2 =
  let pos1 = ref 0
  and pos2 = ref 0  in

  let next ~ok s pos =
    if (!pos) = String.length s then
      None
    else
      let c = s.[!pos] in
      if ok c then begin
        incr pos;
        Some c
      end else
        None
  in

  let compare_non_numerical () =
    let ok c = not (Char.is_digit c) in
    let rec loop () =
      match next ~ok s1 pos1,next ~ok s2 pos2 with
      | Some _, None -> 1
      | None , Some _ -> -1
      | None , None -> 0
      | Some c1,Some c2 when c1 = c2 -> loop ()
      | Some c1,Some c2 -> Char.compare c1 c2
    in
    loop ()
  in

  let compare_numerical () =
    let rec consume0 s pos  =
      match next ~ok:((=) '0') s pos with
      | Some _ -> consume0 s pos
      | None ->  ()
    in
     (* Our main loop works on string representation of ints where all the
        trailing zeros have been chopped of. Their magnitude is given by the
        length of their representation. If they have the same magnitude the
        lexical order is correct. Bias is used to save that information.
     *)
    let ok = Char.is_digit in
    let bias = ref 0 in
    let rec loop () =
      match next ~ok s1 pos1,next ~ok s2 pos2 with
      | Some _, None -> 1
      | None , Some _ -> -1
      | None , None when !bias <> 0-> !bias
      | None , None ->
          (* Both ints have the same value, The one with the shortest
             representation (i.e. the least trailing zeroes) is
             considered to be the smallest*)
          !pos1 - !pos2
      | Some c1,Some c2 when !bias = 0 ->
          bias := Char.compare c1 c2;
          loop ()
      | Some _ , Some _ -> loop ()
    in
    consume0 s1 pos1;
    consume0 s2 pos2;
    loop ()
  in

  let rec loop () =
    let r = compare_non_numerical () in
    let r' = compare_numerical () in
    match r,r' with
    | 0,0 when !pos1 = String.length s1 && !pos2 = String.length s2  -> 0
    | 0,0 -> loop ()
    | 0,i | i,_ -> i
  in
  loop ()

(**
   Inverse operation of [String.escaped]
*)
exception Unescape_error of bool*int*string

let unescaped' ?(strict=true) s =
  let len = String.length s in
  let pos = ref 0 in
  let error ?(fatal=false) message =
    raise (Unescape_error (fatal,!pos,message))
  in
  let consume () =
    let i = !pos in
    if i = len then error "unexpectedly reached end of string";
    let c = s.[i] in
    pos := i + 1;
    c
  in
  let res = Buffer.create len in
  let emit c = Buffer.add_char res c in
  let emit_code code =
    match Char.of_int code with
    | Some c -> emit c
    | None -> error ~fatal:true
        (Printf.sprintf "got invalid escape code %d" code)
  in
  let rec loop () =
    if !pos < len then begin
      let c = consume () in
      if c <> '\\' then
        emit c
      else begin
        let mark = !pos in
        try
          let c = consume () in
          match c with
          | '\\' | '\"' -> emit c
          | 'b' -> emit '\b'
          | 'n' -> emit '\n'
          | 'r' -> emit '\r'
          | 't' -> emit '\t'
          | '\n' ->
              let rec consume_blank () =
                if !pos < len then begin
                  match consume () with
                  | ' ' | '\t' -> consume_blank ()
                  | _ -> decr pos
                end
              in
              consume_blank ()
          | 'x' ->
              let c2hex c =
                if (c >= 'A') && (c <= 'F' ) then
                  (Char.to_int c) + 10 - Char.to_int 'A'
                else if (c >= 'a') && (c <= 'f' ) then
                  (Char.to_int c) + 10 - Char.to_int 'a'
                else if (c >= '0') && (c <= '9') then
                  (Char.to_int c) - Char.to_int '0'
                else
                  error (Printf.sprintf "expected hex digit, got: %c" c);
              in
              let c1 = consume () in
              let c2 = consume () in
              emit_code (16 * c2hex c1 + c2hex c2);
          | c when Char.is_digit c ->
              let char_to_num c =
                match Char.get_digit c with
                | None -> error (Printf.sprintf "expected digit,got: %c" c);
                | Some i -> i
              in
              let i1 = char_to_num c in
              let i2 = char_to_num (consume ()) in
              let i3 = char_to_num (consume ()) in
              emit_code (100 * i1 + 10 * i2 + i3);
          | c -> error (Printf.sprintf "got invalid escape character: %c" c);
        with Unescape_error (false,_,_) when not strict ->
          emit '\\';
          pos := mark
      end;
      loop ()
    end else
      Buffer.contents res;
  in
  loop ();
;;

let unescaped ?strict s =
  try
    unescaped' ?strict s
  with Unescape_error (_,pos,message) ->
    invalid_argf "String.unescaped error at position %d of %s: %s"
      pos s message ()

let unescaped_res ?strict s =
  try
    Core.Result.Ok (unescaped' ?strict s)
  with Unescape_error (_,pos,message) ->
    Core.Result.Error (pos,message)


let squeeze str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec skip_spaces i =
    if i >= len then
      Buffer.contents buf
    else
      let c = str.[i] in
      if (c = ' ') || (c = '\n') || (c = '\t') || (c = '\r') then
        skip_spaces (i+1)
      else
        begin
          Buffer.add_char buf c;
          copy_chars (i+1)
        end
  and copy_chars i =
    if i >= len then
      Buffer.contents buf
    else
      let c = str.[i] in
      if (c = ' ') || (c = '\n') || (c = '\t') || (c = '\r') then
        begin
          Buffer.add_char buf ' ';
          skip_spaces (i+1)
        end
      else
        begin
          Buffer.add_char buf c;
          copy_chars (i+1)
        end
  in
  copy_chars 0
;;


(* Knuth-Morris-Pratt string matching. *)


let is_substring  ~substring t =
  let kmp_prefix len ~substring =
    let prefix = Array.create len 0 in
    let rec f ~k ~q =
      if q > len then prefix
      else (
        let k =
          let rec g k =
            if k <= 0 || ((String.get substring k) = (String.get substring (q - 1))) then k
            else g (prefix.(k - 1))
          in
          g k
        in
        let k =
          if (String.get substring k) = (String.get substring (q - 1))
          then k + 1 else k
        in
        assert (q - 1 >= 0 && q - 1 < len);
        Array.set prefix (q - 1) k;
        f ~k ~q:(q + 1)
      )
    in
    f ~k:0 ~q:2
  in
  let n = String.length t in
  let m = String.length substring in
  let prefix = kmp_prefix m ~substring in
  let rec f ~q ~i =
    if i > n then false
    else
    let q = (
      let q =
        let rec g q =
          if q <= 0 || ((String.get substring q) = (String.get t (i - 1)))
          then q
          else g (prefix.(q - 1))
        in
        g q
      in
      if String.get substring q = String.get t (i - 1) then q + 1
      else q
    )
    in
    if q = m then true
    else f ~q ~i:(i + 1)
  in
  f ~q:0 ~i:1
