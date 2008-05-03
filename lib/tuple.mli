module T2 : sig
  type ('a, 'b) t = 'a * 'b
  val create : 'a -> 'b -> ('a, 'b) t
  val curry :  (('a, 'b) t -> 'c) -> 'a -> 'b -> 'c
  val uncurry : ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c
  val compare :
    ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"
  val map1 : f:('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val map2 : f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
end

module T3 : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c
  val create : 'a -> 'b -> 'c -> ('a, 'b, 'c) t
  val curry :  (('a, 'b, 'c) t -> 'd) -> 'a -> 'b -> 'c -> 'd
  val uncurry : ('a -> 'b -> 'c -> 'd) -> ('a, 'b, 'c) t -> 'd
  val compare :
    ('a -> 'a -> int)
    -> ('b -> 'b -> int)
    -> ('c -> 'c -> int)
    -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int
  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  val get3 : (_, _, 'a) t -> 'a
  val map1 : f:('a -> 'd) -> ('a, 'b, 'c) t -> ('d, 'b, 'c) t
  val map2 : f:('b -> 'd) -> ('a, 'b, 'c) t -> ('a, 'd, 'c) t
  val map3 : f:('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
end
