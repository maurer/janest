(** Least Recently Used cache

   Simple LRU list for use in caching situations.  The LRU is designed to
  not blow up when the destructor throws an exception, but better not to
  try... *)


type 'a lru


(** [create size destructor] creates a new lru list that stores at most
    [size] elements, and calls [destructor] on any element before it's kicked out
    of the list *)
val create : int -> ('a -> unit) -> 'a lru

(** touch marks an element as being recently used, and updates the LRU
  accordingly.  An element does not have to be in the LRU to be touched. *)
val touch : 'a lru -> 'a -> unit

(** clears out the entire lru, calling the destructor in each element *)
val clear : 'a lru -> unit

(** returns current size of lru  *)
val size : 'a lru -> int

(** change the maximum size of the LRU list.  If the size goes down, this
  could cause a series of destructor calls. *)
val change_size : 'a lru -> int -> unit


(** tells you whether a given value is cached or not *)
val in_cache : 'a -> 'a lru -> bool


(** Returns lru list and memoized version of function. *)
val memoize : ?destruct : ('b -> unit)
  -> int
  -> ('a -> 'b)
  -> 'a lru * ('a -> 'b)
