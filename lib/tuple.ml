module T2 = struct
  type ('a, 'b) t = 'a * 'b

  let create a b = (a, b)

  let curry f = fun a b -> f (a, b)

  let uncurry f = fun (a,b) -> f a b

  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"

  let map1 ~f (x,y) = (f x, y)

  let map2 ~f (x,y) = (x, f y)

  let compare cmp1 cmp2 =
    fun (x, y) (x', y') ->
      match cmp1 x x' with
      | 0 -> cmp2 y y'
      | i -> i
  ;;
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let create a b c = (a ,b, c)

  let curry f = fun a b c -> f (a,b,c)

  let uncurry f = fun (a,b,c) -> f a b c

  let map1 ~f (x,y,z) = (f x, y, z)

  let map2 ~f (x,y,z) = (x, f y, z)

  let map3 ~f (x,y,z) = (x, y, f z)

  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  let get3 (_, _, a) = a

  let compare cmp1 cmp2 cmp3 =
    fun (x, y, z) (x', y', z') ->
      match cmp1 x x' with
      | 0 -> (match cmp2 y y' with
        | 0 -> cmp3 z z'
        | i -> i
        )
      | i -> i
  ;;
end
