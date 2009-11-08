open Core.Std

module Int_set = Core_extended.Std.Int_set
  
let max_int = truncate (2.0 ** (Float.of_string Sys.argv.(1)))

module Range = struct
  type t = {lo: int; hi: int}

  let to_string t = sprintf "(%d, %d)" t.lo t.hi

  (* on the number line, r1 is either on the left, on the right, or
     intersected with r2 *)
  let compare r1 r2 =
    if r1.hi < r2.lo - 1 then
      `Lt
    else if r1.lo > r2.hi + 1 then
      `Gt
    else
      `Intr

  let contains r1 r2 = r1.lo >= r2.lo && r1.hi <= r2.hi

  let merge r1 r2 =
    match compare r1 r2 with
    | `Lt -> `Error `Lt
    | `Gt -> `Error `Gt
    | `Intr ->
        if contains r1 r2 then
          `Subinterval r2
        else
          `Ok {lo = Int.min r1.lo r2.lo; hi = Int.max r1.hi r2.hi}

  let contains i r = r.lo <= i && i <= r.hi

  let max_rand = Int.min 1_000_000_000 max_int
  let random () = 
    let lo = Random.int max_rand in
    let hi = lo + (Int.min (max_int - lo) (Random.int (max_rand / 1000))) in
    {lo = lo; hi = hi}
end

type t = {
  mutable min : int;
  mutable ranges : Range.t list;
}

let to_string t = 
  String.concat ~sep:", "
    (List.rev_map t.ranges ~f:Range.to_string)

let add_range t r = 
  let info_added = ref false in
  let rec loop ranges to_add = 
    match ranges with
    | r :: rest ->
        begin match Range.merge to_add r with
        | `Error `Lt -> r :: loop rest to_add
        | `Error `Gt -> 
            info_added := true;
            to_add :: r :: rest
        | `Ok merged -> 
            info_added := true;
            loop rest merged
        | `Subinterval merged -> merged :: rest
        end
    | [] ->
        t.min <- to_add.Range.lo; (* this is dirty *)
        [to_add]
  in
  t.ranges <- loop t.ranges r;
  !info_added

let add_range_ignore t r = ignore (add_range t r)

let mem t i = List.exists t.ranges ~f:(Range.contains i)

let full t = 
  match t.ranges with
  | [{Range.lo=x;hi=y}] -> x = 0 && y = max_int
  | _ -> false

let check t iset =
  let check_range ~expected r = 
    for i = r.Range.lo to r.Range.hi do
      if Int_set.mem iset i <> expected then
        failwith (sprintf "fail %d, expected %b" i expected)
    done
  in
  let invert_ranges rngs = 
    let module R = Range in
    let new_t = {min = 0; ranges = []} in
    match List.rev rngs with
    | r :: rest ->
        add_range_ignore new_t {R.lo = 0; hi = r.R.lo - 1};
        let rec loop rngs = 
          match rngs with
          | r1 :: r2 :: rest ->
              add_range_ignore new_t {R.lo = r1.R.hi + 1; hi = r2.R.lo - 1};
              loop (r2 :: rest)
          | [r] -> 
              add_range_ignore new_t {R.lo = r.R.hi + 1; hi = max_int}
          | [] -> assert false
        in
        loop (r :: rest);
        new_t
    | [] -> 
        add_range_ignore new_t {R.lo = 0; hi = max_int};
        new_t
  in
  List.iter t.ranges ~f:(check_range ~expected:true);
  List.iter (invert_ranges t.ranges).ranges ~f:(check_range ~expected:false)

let () =
  Random.full_init [|Unix.getpid ();truncate (Unix.gettimeofday ())|];
  let ranges = ref [] in
  let t = {min = 0; ranges = []} in
  let iset = Int_set.create () in
  let skipped_checks = ref 0 in
  Caml.Sys.set_signal Caml.Sys.sigusr1 
    (Caml.Sys.Signal_handle
        (fun _ -> 
          print_endline (to_string {min=0;ranges=(!ranges)});
          print_endline (to_string t);
          flush stdout));
  Caml.Sys.set_signal Caml.Sys.sigusr2
    (Caml.Sys.Signal_handle
        (fun _ -> 
          let rlen = List.length !ranges in
          let clen = List.length t.ranges in
          let cratio = (float clen) /. (float rlen) in
          printf "total_added: %d, in_model: %d, deflate factor: %f\n"
            rlen clen cratio;
          printf "skipped checks: %d, skip percentage: %f\n\n%!"
            !skipped_checks ((float !skipped_checks) /. (float rlen))));
  while not (full t) do
    let r = Range.random () in
    ranges := r :: !ranges;
    let information_added = add_range t r in
    Int_set.add_range iset ~lo:r.Range.lo ~hi:r.Range.hi;
    if information_added then
      try check t iset
      with exn ->
        printf "added : %s\n" (to_string {min=0;ranges=(!ranges)});
        printf "resulting in model: %s\n" (to_string t);
        printf "iset failed: %s\n%!" (Exn.to_string exn);
        exit 1
    else
      incr skipped_checks
  done
