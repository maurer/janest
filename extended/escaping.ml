open Core.Std

let escape_gen ~escapeworthy_map ~escape_char s =
  let get_code_for c =
    if c = escape_char then c else
      try List.assoc c escapeworthy_map with Not_found -> failwith "escape_gen bug"
  in
  let escapeworthy c =
    c = escape_char || List.exists ~f:(fun (c',_) -> c = c') escapeworthy_map in
  let count_escapeworthy_chars s =
    let ctr = ref 0 in
    for i = 0 to String.length s - 1 do
      if escapeworthy s.[i] then ctr := !ctr + 1
    done;
    !ctr
  in
  let really_escape_string count s =
    let ns = String.create count in
    let ns_pos = ref 0 in
    for i = 0 to String.length s - 1 do
      if escapeworthy s.[i] then
        begin
          ns.[!ns_pos] <- escape_char;
          ns.[!ns_pos + 1] <- get_code_for s.[i];
          ns_pos := !ns_pos + 2
        end
      else
        begin
          ns.[!ns_pos] <- s.[i];
          ns_pos := !ns_pos + 1
        end
    done;
    ns
  in
  let ewc = count_escapeworthy_chars s in
  if ewc > 0 then really_escape_string (ewc + String.length s) s
  else s


let escape ~escapeworthy ~escape_char str =
  let escapeworthy = escape_char :: escapeworthy in
  let threshold = List.fold_left ~f:(fun m p -> if p < fst m then (p, snd m)
    else if p > snd m then (fst m, p) else m)
    ~init:(List.hd_exn escapeworthy, List.hd_exn escapeworthy) escapeworthy in
  let min = fst threshold in
  let max = snd threshold in
  let strlen = (String.length str) - 1 in
  let smark = ref 0
  and dmark = ref 0 in
  let newstr =  String.create ((strlen + 1) * 2) in
  let blit send =   (* copies smark to send into newstr *)
    let len = (send - !smark + 1) in
    String.blit ~src:str ~src_pos:!smark ~dst:newstr ~dst_pos:!dmark ~len:len;
    smark := send + 1;
    dmark := !dmark + len;
  in
  let rec exists lst c =
    match lst with
    | [] -> false
    | p :: rst -> if p = c then true else exists rst c
  in
  for i = 0 to strlen do
    if str.[i] >= min && str.[i] <= max &&
      List.exists ~f:(fun c -> c = str.[i]) escapeworthy then
      begin
        blit i;
        newstr.[!dmark - 1] <- escape_char;
        newstr.[!dmark] <- str.[i];
        dmark := !dmark + 1;
      end
  done;
  if !smark > 0 then
    begin
      if !smark <= strlen then blit strlen;
      String.sub newstr ~pos:0 ~len:!dmark
    end
  else
    str

let escape_one_orig ~escapeworthy ~escape_char str =
  let len = String.length str in
  let rec loop cnt i =
    if i < 0 then cnt
    else
      let new_i = i - 1 in
      let c = str.[i] in
      if c = escapeworthy || c = escape_char then loop (cnt + 2) new_i
      else loop (cnt + 1) new_i
  in
  let len_1 = len - 1 in
  let cnt = loop 0 len_1 in
  if cnt = len then str
  else
    let res = String.create cnt in
    let rec loop src_pos dst_pos =
      if src_pos < 0 then res
      else
        let new_src_pos = src_pos - 1 in
        let c = str.[src_pos] in
        res.[dst_pos] <- c;
        if c = escapeworthy || c = escape_char then (
          res.[dst_pos - 1] <- escape_char;
          loop new_src_pos (dst_pos - 2))
        else loop new_src_pos (dst_pos - 1)
    in
    loop len_1 (cnt - 1)

let escape_two_orig ~escapeworthy1 ~escapeworthy2 ~escape_char str =
  let len = String.length str in
  let rec loop cnt i =
    if i < 0 then cnt
    else
      let new_i = i - 1 in
      let c = str.[i] in
      if c = escapeworthy1 || c = escapeworthy2 || c = escape_char
      then loop (cnt + 2) new_i
      else loop (cnt + 1) new_i
  in
  let len_1 = len - 1 in
  let cnt = loop 0 len_1 in
  if cnt = len then str
  else
    let res = String.create cnt in
    let rec loop src_pos dst_pos =
      if src_pos < 0 then res
      else
        let new_src_pos = src_pos - 1 in
        let c = str.[src_pos] in
        res.[dst_pos] <- c;
        if c = escapeworthy1 || c = escapeworthy2 || c = escape_char then (
          res.[dst_pos - 1] <- escape_char;
          loop new_src_pos (dst_pos - 2))
        else loop new_src_pos (dst_pos - 1)
    in
    loop len_1 (cnt - 1)

let unescape_gen ~map ~escape_char =
  let get_c_for_code code =
    if code = escape_char then code else
      try List.assoc code map with Not_found -> code
  in
  let count_escape_chars s =
    let ctr = ref 0 in
    let i = ref 0 in
    while !i < String.length s - 1 do
      if s.[!i] = escape_char then
        begin
          incr ctr;
          i := !i + 2
        end
      else
        i := !i + 1
    done;
    !ctr
  in
  let really_unescape_string num_escape_char os =
    let ns_length = String.length os - num_escape_char in
    let ns = String.create ns_length in
    let os_pos = ref 0 in
    for i = 0 to ns_length - 1 do
      if os.[!os_pos] = escape_char then
        begin
          ns.[i] <- get_c_for_code (os.[!os_pos + 1]);
          os_pos := !os_pos + 2;
        end
      else
        begin
          ns.[i] <- os.[!os_pos];
          os_pos := !os_pos + 1;
        end
    done;
    ns
  in
  (fun str ->
     let num_escape_chars = count_escape_chars str in
     if num_escape_chars > 0 then really_unescape_string num_escape_chars str
     else str)

let unescape ~escape_char str = unescape_gen ~map:[] ~escape_char str
(* this implementation is faster, if need be: *)
(*let unescape ~escape_char str =
  let count_escape_chars s =
    let ctr = ref 0 in
    let i = ref 0 in
    while !i < String.length s - 1 do
      if s.[!i] = escape_char then
        begin
          incr ctr;
          i := !i + 2
        end
      else
          i := !i + 1
    done;
    !ctr
  in
  let really_unescape_string num_escape_char os =
    let ns_length = String.length os - num_escape_char in
    let ns = String.create ns_length in
    let os_pos = ref 0 in
    for i = 0 to ns_length - 1 do
      if os.[!os_pos] = escape_char then
        begin
          ns.[i] <- os.[!os_pos + 1];
          os_pos := !os_pos + 2;
        end
      else
        begin
          ns.[i] <- os.[!os_pos];
          os_pos := !os_pos + 1;
        end
    done;
    ns
  in
  let num_escape_chars = count_escape_chars str in
  if num_escape_chars > 0 then really_unescape_string num_escape_chars str
  else str
*)
