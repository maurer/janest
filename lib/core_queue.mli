(** Core_queue is a wrapper around OCaml's standard Queue module that
    follows Core idioms and adds some functions.

    Differences from the standard module:
      [enqueue] replaces [push], [add], and takes the queue first.
      [dequeue] replaces [pop], [take], takes the queue first, and returns an
        option rather than raising [Empty].
      [dequeue_exn] is available if you want to raise [Empty].
      [iter] takes a labeled argument.
      [transfer]'s arguments are labeled.
*)



exception Empty

type 'a t

include Container.S1 with type 'a container = 'a t
include Sexpable.S1 with type 'a sexpable = 'a t

(** [create ()] returns an empty queue. *)
val create : unit -> 'a t

(** [enqueue t x] adds [x] to the end of [t].*)
val enqueue : 'a t -> 'a -> unit

(** [dequeue t] returns [None] if [t] is empty, otherwise it removes and returns
    the front of [t] *)
val dequeue : 'a t -> 'a option

(** [dequeue_exn t] removes and returns the front of [t], raising [Empty] if [t]
    is empty. *)
val dequeue_exn : 'a t -> 'a

(** [peek t] returns [None] if [t] is empty, otherwise it returns [Some x] where
    [x] is the front of [t].
*)
val peek : 'a t -> 'a option

(** [peek_exn t] raises [Empty] if [t] is empty, otherwise it returns the front
    of [t].
*)
val peek_exn : 'a t -> 'a

(** [clear t] discards all elements from [t]. *)
val clear : 'a t -> unit

(** [copy t] returns a copy of [t]. *)
val copy : 'a t -> 'a t

(** [filter_inplace t ~f] removes all elements of [t] that don't satisfy [f]. *)
val filter_inplace : 'a t -> f:('a -> bool) -> unit

(** [transfer ~src ~dst] adds all of the elements of [src] to the end of [dst],
    then clears [src]. It is equivalent to the sequence
      [iter ~src ~f:(enqueue dst); clear src]
    but runs in constant time. *)
val transfer : src:'a t -> dst:'a t -> unit

(** [of_list list] returns a queue [t] with the elements of [list] in the same
    order as the elements of [list] (i.e. the first element of [t] is the first
    element of the list). *)
val of_list : 'a list -> 'a t

(** [partial_iter t ~f] iterates through t until f returns `Stop *)
val partial_iter : 'a t -> f:('a -> [`Continue | `Stop]) -> unit
