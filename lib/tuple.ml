(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Tuple"

module T2 = struct
  type ('a, 'b) t = 'a * 'b with sexp

  type ('a, 'b) sexpable = ('a, 'b) t

  let create a b = (a, b)

  
  let curry f = (); fun a b -> f (a, b)

  let uncurry f = (); fun (a,b) -> f a b

  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"

  let map1 ~f (x,y) = (f x, y)

  let map2 ~f (x,y) = (x, f y)

  let compare ~cmp1 ~cmp2 =
    fun (x, y) (x', y') ->
      match cmp1 x x' with
      | 0 -> cmp2 y y'
      | i -> i
  ;;

  let swap (a, b) = (b, a)
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c with sexp

  type ('a, 'b, 'c) sexpable = ('a, 'b, 'c) t

  let create a b c = (a, b, c)

  let curry f = (); fun a b c -> f (a,b,c)

  let uncurry f = (); fun (a,b,c) -> f a b c

  let map1 ~f (x,y,z) = (f x, y, z)

  let map2 ~f (x,y,z) = (x, f y, z)

  let map3 ~f (x,y,z) = (x, y, f z)

  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  (* There's no %field2....*)
  let get3 (_, _, a) = a

  (* lexicographic comparison  *)
  let compare ~cmp1 ~cmp2 ~cmp3 =
    fun (x, y, z) (x', y', z') ->
      let c1 = cmp1 x x' in
      if c1 <> 0 then c1 else
        let c2 = cmp2 y y' in
        if c2 <> 0 then c2 else
          cmp3 z z'
  ;;

end
