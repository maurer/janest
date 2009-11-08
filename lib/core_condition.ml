include Condition

let phys_equal = Caml.(==)

let equal (t : t) t' = phys_equal t t'

let timedwait cnd mtx time =
  Unix_ext.condition_timedwait cnd mtx (Time.to_float time)
