open Core.Std

let create ~iter =
  let length c =
    let count = ref 0 in
    iter c ~f:(fun _ -> incr count);
    !count
  in
  let is_empty c = length c = 0 in
  let iter c = iter c in
  let fold c ~init ~f =
    let accum = ref init in
    iter c ~f:(fun x -> accum := f !accum x);
    !accum
  in
  let exists c ~f =
    let result = ref false in
    iter c ~f:(fun x -> if f x then result := true);
    !result
  in
  let for_all c ~f =
    let result = ref true in
    iter c ~f:(fun x -> if not (f x) then result := false);
    !result
  in
  let find c ~f =
    let result = ref None in
    iter c ~f:(fun x -> if f x then result := Some x);
    !result
  in
  let to_list c =
    let accum = ref [] in
    iter c ~f:(fun x -> accum := x :: !accum);
    List.rev !accum
  in
  let to_array c = Array.of_list (to_list c) in
  {
    Container.
    length = length;
    is_empty = is_empty;
    iter = iter;
    fold = fold;
    exists = exists;
    for_all = for_all;
    find = find;
    to_list = to_list;
    to_array = to_array;
  }
