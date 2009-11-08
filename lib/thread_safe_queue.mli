(** There are four variants of thread-safe queue, depending on whether there are one/many
    readers or one/many writers.  All the variants allow (at least) one reader and (at
    least) one writer simultaneously, unlike the default Queue.t.  A create function
    returns a pair (read, write), where read gets an element from the front of the queue
    and write adds an element to the back of the queue.  *)

type 'a create = unit -> (unit -> 'a option) * ('a -> unit)


val one_reader_one_writer : 'a create   (* lock-free implementation *)
val one_reader_many_writers : 'a create
val many_readers_one_writer : 'a create
val many_readers_many_writers : 'a create
