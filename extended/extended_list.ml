open Core.Std




let diff l1 l2 =
  let set = Set.of_list l2 in
  List.filter l1 ~f:(fun x -> not (Set.mem set x))

let inter l1 l2 =
  let set = Set.of_list l2 in
  List.dedup (List.filter l1 ~f:(fun x -> Set.mem set x))


let classify ?(equal=( = )) ~f list =
  let classify_element class_members_assoc this_member =
    let this_class = f this_member in
    let rec add_class_member new_class_members_assoc old_class_members_assoc =
      match old_class_members_assoc with
      | [] ->
          (this_class,[this_member])::new_class_members_assoc
      | (classs,members)::rest when equal classs this_class ->
          (classs, this_member::members)::new_class_members_assoc@rest
      | l::ls ->
          add_class_member (l::new_class_members_assoc) ls
    in
    add_class_member [] class_members_assoc
  in
  List.fold_left list
    ~init:[] ~f:classify_element

let take_while xs f =
  let rec loop xs rev_prefix = match xs with
    | x::xs' -> if f x then loop xs' (x::rev_prefix) else List.rev rev_prefix
    | []     -> List.rev rev_prefix
  in
  loop xs []

let split_while xs p =
	let rec loop acc = function
		| [] -> (List.rev acc, [])
		| x::xs as x_xs -> if p x then loop (x::acc) xs else (List.rev acc, x_xs)
	in loop [] xs

let intersperse t sep =
  match t with
  | [] -> []
  | x :: xs ->
      let rec loop res xs =
        match xs with
        | [] -> List.rev res
        | x :: xs ->
            loop (x :: sep :: res) xs
      in
      x :: loop [] xs
;;

let enumerate_from =
	let rec loop acc n = function 
		| [] -> List.rev acc
		| x::xs -> loop ((x,n)::acc) (n+1) xs
	in
	fun xs -> loop [] xs

let fold_left_term lst ~f ~init =
  let rec loop lst ~f ~acc =
    match lst with
    | [] -> acc
    | hd :: tl ->
        match f acc hd with
        | `Final v -> v
        | `Continue acc -> loop tl ~f ~acc
  in
  loop lst ~f ~acc:init



let max ?(cmp=Pervasives.compare) l =
  List.reduce l
    ~f:(fun x y -> if cmp x y > 0 then x else y)

let min ?(cmp=Pervasives.compare) l =
  List.reduce l
    ~f:(fun x y -> if cmp x y < 0 then x else y)

let max_exn ?(cmp=Pervasives.compare) l =
  List.reduce_exn l
    ~f:(fun x y -> if cmp x y > 0 then x else y)

let min_exn ?(cmp=Pervasives.compare) l =
  List.reduce_exn l
    ~f:(fun x y -> if cmp x y < 0 then x else y)
