(** Min-heap implementation, adapted from CLR.  *)

(** {6 Exceptions} *)

exception Empty
(** Raised when [top] or [pop] is called on an empty heap. *)


(** {6 Types} *)

(** Type of heaps *)
type 'el t

(** Type of heap elements (they can be efficiently removed) *)
type 'el heap_el


(** {6 Functions on heap elements} *)

val heap_el_is_valid : 'el heap_el -> bool
(** [heap_el_is_valid heap_el] @return [true] iff heap element is member
    of a heap. *)

val heap_el_get_el : 'el heap_el -> 'el
(** [heap_el_get_el heap_el] @return the element associated with the heap
    element. *)


(** {6 Information on heap values} *)

val length : 'el t -> int
(** [length heap] @return the length (number of elements) of [heap]. *)

val is_empty : 'el t -> bool
(** [is_empty heap] @return [true] iff [heap] does not contain any
    elements. *)

val get_cmp : 'el t -> ('el -> 'el -> int)
(** [get_cmp heap] @return the ordering function used by [heap]. *)


(** {6 Heap creation} *)

val create : ?min_size : int -> ('el -> 'el -> int) -> 'el t
(** [create ?min_size cmp] @return a fresh heap that can store [min_size]
    elements without reallocations, using ordering function [cmp].

    If [cmp x y < 0] (i.e. x < y), then [x] will be on "top" of [y] in the heap.
    That is, Heap.pop will remove [x] before [y].
*)

val of_array : ?min_size : int -> ('el -> 'el -> int) -> 'el array -> 'el t
(** [of_array ?min_size cmp ar] @return a fresh heap that can store
    [min_size] elements without reallocations, using ordering function
    [cmp], and initialize it with the elements of [ar]. *)

val copy : 'el t -> 'el t
(** [copy heap] @return a copy of [heap]. *)


(** {6 Search functions} *)

val mem : 'el t -> 'el -> bool
(** [mem heap el] @return [true] iff [el] is member of [heap].
    Requires linear time in worst case. *)

val heap_el_mem : 'el t -> 'el heap_el -> bool
(** [heap_el_mem heap heap_el] @return [true] iff [heap_el] is member of
    [heap].  Requires constant time only. *)

val find_heap_el : 'el t -> 'el -> 'el heap_el
(** [find_heap_el heap el] @return the heap element associated with
    element [el] in [heap].
    
    @raise Not_found if [el] could not be found. *)


(** {6 Non-destructive heap accessors} *)

val top : 'el t -> 'el
(** [top heap] @return the top element of [heap] without changing it.
    @raise Empty if [heap] is empty. *)

val maybe_top : 'el t -> 'el option
(** [maybe_top heap] @return [Some top_element] of [heap] without
    changing it, or [None] if [heap] is empty. *)

val iter : 'a t -> f:('a -> unit) -> unit
  
(** {6 Destructive heap accessors} *)

val pop : 'el t -> 'el
(** [pop heap] @return the top element of [heap], removing it.
    @raise Empty if [heap] is empty. *)

val maybe_pop : 'el t -> 'el option
(** [maybe_pop heap] @return [Some top_element] of [heap], removing it,
    or [None] if [heap] is empty. *)

val pop_heap_el : 'el t -> 'el heap_el
(** [pop_heap_el heap] @return the top heap element of [heap], removing
    it.  @raise Empty if [heap] is empty. *)

val maybe_pop_heap_el : 'el t -> 'el heap_el option
(** [maybe_pop_heap_el heap] @return [Some top_heap_element], removing
    it, or [None] if [heap] is empty. *)

val cond_pop : 'el t -> ('el -> bool) -> 'el option
(** [cond_pop heap cond] @return [Some top_element] of [heap] if it
    fills condition [cond], removing it, or [None] in any other case. *)

val cond_pop_heap_el : 'el t -> ('el -> bool) -> 'el heap_el option
(** [cond_pop_heap_el heap cond] @return [Some top_heap_element] of
    [heap] if the associated element fills condition [cond], removing it,
    or [None] in any other case. *)

val push : 'el t -> 'el -> 'el heap_el
(** [push heap el] pushes element [el] on [heap].  @return the heap
    element associated with the newly inserted element. *)

val push_heap_el : 'el t -> 'el heap_el -> unit
(** [push_heap_el heap heap_el] pushes [heap_el] on [heap].
    @raise Failure if [heap_el] is already member of some heap. *)

val remove : 'el heap_el -> unit
(** [remove heap_el] removes [heap_el] from its associated heap.
    @raise Failure if [heap_el] is not member of any heap. *)

val update : 'el heap_el -> 'el -> unit
(** [update heap_el el] updates [heap_el] with element [el] in its
    associated heap.
    @raise Failure if [heap_el] is not member of any heap. *)

(** {6 For testing only.} *)

(** [check_heap_property h] asserts that [h] has the heap property: that all
    nodes are less than their children by [h]'s comparison function. *)

val check_heap_property : 'el t -> bool
