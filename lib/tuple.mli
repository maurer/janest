

module T2 : sig
  type ('a, 'b) t = 'a * 'b

  include Sexpable.S2 with type ('a, 'b) sexpable = ('a, 'b) t

  val create : 'a -> 'b -> ('a, 'b) t
  val curry :  (('a, 'b) t -> 'c) -> 'a -> 'b -> 'c
  val uncurry : ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c
  val compare : cmp1: ('a -> 'a -> int) -> cmp2:('b -> 'b -> int)
    -> ('a, 'b) t
    -> ('a, 'b) t
    -> int

  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"
  val map1 : f:('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
  val map2 : f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

  val swap : ('a, 'b) t -> ('b, 'a) t
end

module T3 : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  include Sexpable.S3 with type ('a, 'b, 'c) sexpable = ('a, 'b, 'c) t

  val create : 'a -> 'b -> 'c -> ('a, 'b, 'c) t
  val curry :  (('a, 'b, 'c) t -> 'd) -> 'a -> 'b -> 'c -> 'd
  val uncurry : ('a -> 'b -> 'c -> 'd) -> ('a, 'b, 'c) t -> 'd
  val compare :
    cmp1:('a -> 'a -> int)
    -> cmp2:('b -> 'b -> int)
    -> cmp3:('c -> 'c -> int)
    -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> int
  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  val get3 : (_, _, 'a) t -> 'a
  val map1 : f:('a -> 'd) -> ('a, 'b, 'c) t -> ('d, 'b, 'c) t
  val map2 : f:('b -> 'd) -> ('a, 'b, 'c) t -> ('a, 'd, 'c) t
  val map3 : f:('c -> 'd) -> ('a, 'b, 'c) t -> ('a, 'b, 'd) t
end
