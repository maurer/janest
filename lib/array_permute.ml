(* factored out due to a circular dependency between core_array and core_list *)

let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp


(** randomly permute an array. *)
let permute ?random_state t =
  let state =
    match random_state with
    | None -> Random.get_state ()
    | Some state -> state
  in
  let len = Array.length t in
  for i = 0 to len - 2 do
    let j = i + Random.State.int state (len - i) in
    swap t i j;
  done;
  if random_state = None then Random.set_state state
