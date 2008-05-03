(** There are four variants of thread-safe queue, depending on whether there
    are one/many readers or one/many writers.  A create function returns a pair
    (read, write), where read gets an element from the front of the queue and
    write adds an element to the back of the queue.
  *)
type 'a create = unit -> (unit -> 'a option) * ('a -> unit)

(* CRv2 sweeks: For reading with many readers, it would be nice to provide a
   function that gets all the elements so that it can be done with a single
   lock acquisition rather than one acquisition per element gotten.
   Ditto for writing a batch.
*)
  
val one_reader_one_writer : 'a create
val one_reader_many_writers : 'a create
val many_readers_one_writer : 'a create
val many_readers_many_writers : 'a create
