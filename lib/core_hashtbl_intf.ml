open Sexplib

module type HashedType = sig
  include MoreLabels.Hashtbl.HashedType
  include Sexpable.S with type sexpable = t
end

module type Types = sig
  type 'a key
  type ('a, 'b) t
end

(* Gen is a signature functor that generalizes the monomorpic hash table and
   polymorphic hash table. *)
module Gen (T : Types) = struct
  open T
  module type S = sig
    (* These are standard Hashtbl functions. *)
    val shadow_add : ('a, 'b) t -> key:'a key -> data:'b -> unit
    val clear : ('a, 'b) t -> unit
    val copy : ('a, 'b) t -> ('a, 'b) t
    val create : int -> ('a, 'b) t
    val shadow_find : ('a, 'b) t -> 'a key -> 'b list
    val fold :
      ('a, 'b) t -> init:'c -> f:(key:'a key -> data:'b -> 'c -> 'c) -> 'c
    val iter : ('a, 'b) t -> f:(key:'a key -> data:'b -> unit) -> unit
    val length : ('a, 'b) t -> int
    val mem : ('a, 'b) t -> 'a key -> bool
    val remove : ('a, 'b) t -> 'a key -> unit
    
    
    
    val replace : ('a, 'b) t -> key:'a key -> data:'b -> unit

    (** [map t f] returns new table with bound values replaced by
        [f] applied to the bound values *)
    val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

    (** like [map], but function takes both key and data as arguments *)
    val mapi: ('k, 'v1) t -> f:(key:'k key -> data:'v1 -> 'v2) -> ('k, 'v2) t

    (** returns new map with bound values filtered by f applied to the bound
        values *)
    val filter_map: ('k, 'v1) t -> f:('v1 -> 'v2 option) -> ('k, 'v2) t

    (** like [filter_map], but function takes both key and data as arguments*)
    val filter_mapi:
      ('k, 'v1) t -> f:(key:'k key -> data:'v1 -> 'v2 option) -> ('k, 'v2) t

    
    (** [remove_all t k] removes all bindings associated wity key [k]
        from [t]. *)
    val remove_all : ('a, 'b) t -> 'a key -> unit

    
    (** [find_default t k ~default] returns the data associated with key k if it
       is in the table t, otherwise it lets d = default() and adds it to the
       table. *)
    val find_default : ('a, 'b) t -> 'a key -> default:(unit -> 'b) -> 'b

    (** [find t k] returns Some (the current binding) of k in t, or None if no
        such binding exists *)
    val find : ('a, 'b) t -> 'a key -> 'b option

    (** [find_exn t k] returns the current binding of k in t, or raises Not_found
        if no such binding exists.*)
    val find_exn : ('a, 'b) t -> 'a key -> 'b

    (** [iter_vals t ~f] is like iter, except it only supplies the value to f,
        not the key. *)
    val iter_vals : ('a, 'b) t -> f:('b -> unit) -> unit

    (** [of_alist l] returns a new hashtable populated with the supplied data *)
    val of_alist : ('a key * 'b) list -> [ `Ok of ('a, 'b) t | `Duplicate_key of 'a key ]

    val of_alist_exn : ('a key * 'b) list -> ('a, 'b) t

    
    val of_alist_shadow : ('a key * 'b) list -> ('a,'b list) t

    (** Returns the list of all (key,data) pairs for given hashtable. *)
    val to_alist : ('a, 'b) t -> ('a key * 'b) list

    val to_alist_shadow : ('a, 'b) t -> ('a key * 'b list) list

    (** Merge two hashtables.

        The result of [merge f h1 h2] has as keys the set of all [k] in the
        union of the sets of keys of [h1] and [h2] for which [d(k)] is not
        None, where:

        d(k) =
               - f ~key:k (Some d1) None
                   if the *most recent* binding of [k] in [h1] is to d1,
                   and [h2] does not map [k];

               - f ~key:k None (Some d2)
                   if the *most recent* binding of [k] in [h2] is to d2,
                   and [h1] does not map [k];

               - f ~key:k (Some d1) (Some d2)
                   otherwise, where the *most recent* binding of [k] in [h1]
                   is to [d1] and the *most recent* binding of [k] in [h2]
                   is to [d2].

        Each key [k] is mapped to a single piece of data x, where
        [d(k)] = Some x.
    *)
    
    val merge:
      f:(key:'k key -> 'v1 option -> 'v2 option -> 'v3 option)
      -> ('k, 'v1) t -> ('k, 'v2) t -> ('k, 'v3) t

    (** Returns the list of all keys for given hashtable. *)
    val keys : ('a, 'b) t -> 'a key list

    (** Returns the list of all data for given hashtable. *)
    val data : ('a, 'b) t -> 'b list

    (** [filter_inplace t ~f] removes all the elements from [t] that don't
     * satisfy [f].
     *)
    val filter_inplace : (_, 'b) t -> f:('b -> bool) -> unit
    val filteri_inplace : ('a, 'b) t -> f:('a key -> 'b -> bool) -> unit

    val equal : ('a, 'b) t -> ('a, 'b) t -> ('b -> 'b -> bool) -> bool

    
    
    val add_to_groups : ('a key, 'a) t
      -> get_key:('r -> 'a key)
      -> get_data:('r -> 'a)
      -> combine:('a -> 'a -> 'a)
      -> rows:'r list
      -> unit

    val group :
      ?size:int
      -> get_key:('r -> 'a key)
      -> get_data:('r -> 'a)
      -> combine:('a -> 'a -> 'a)
      -> 'r list
      -> ('a key, 'a) t

    
    val create_with_key : get_key:('a -> 'a key) -> 'a list -> ('a key, 'a) t
    val create_mapped : get_key:('r -> 'a key) -> get_data:('r -> 'a)
      -> 'r list -> ('a key, 'a) t

  end
end

module type S = sig
  type key
  type 'a t
  module T : Types with type 'a key = key with type ('a, 'b) t = 'b t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val shadow_add : 'a t -> key:key -> data:'a -> unit
  val remove : 'a t -> key -> unit
  val shadow_find : 'a t -> key -> 'a list
  val replace : 'a t -> key:key -> data:'a -> unit
  val mem : 'a t -> key -> bool
  val iter : 'a t -> f:(key:key -> data:'a -> unit) -> unit
  val fold : 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b
  val length : 'a t -> int

  val map : 'a t -> f:('a -> 'b) -> 'b t
  val mapi : 'a t -> f:(key:key -> data:'a -> 'b) -> 'b t
  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
  val filter_mapi : 'a t -> f:(key:key -> data:'a -> 'b option) -> 'b t
  val remove_all : 'a t -> key -> unit
  val find_default : 'a t -> key -> default:(unit -> 'a) -> 'a
  val find : 'a t -> key -> 'a option
  val find_exn : 'a t -> key -> 'a
  val iter_vals : 'a t -> f:('a -> unit) -> unit
  val of_alist : (key * 'a) list -> [ `Ok of 'a t | `Duplicate_key of key ]
  val of_alist_exn : (key * 'a) list -> 'a t
  val of_alist_shadow : (key * 'a) list -> 'a list t
  val to_alist : 'a t -> (key * 'a) list
  val to_alist_shadow : 'a t -> (key * 'a list) list
  val merge : f:(key:key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val keys : 'a t -> key list
  val data : 'a t -> 'a list
  val filter_inplace : 'a t -> f:('a -> bool) -> unit
  val filteri_inplace : 'a t -> f:(key -> 'a -> bool) -> unit

  val equal : 'a t -> 'a t -> ('a -> 'a -> bool) -> bool

  val add_to_groups : 'a t
    -> get_key:('r -> key)
    -> get_data:('r -> 'a)
    -> combine:('a -> 'a -> 'a)
    -> rows:'r list
    -> unit

  val group :
    ?size:int
    -> get_key:('r -> key)
    -> get_data:('r -> 'a)
    -> combine:('a -> 'a -> 'a)
    -> 'r list
    -> 'a t

  val create_with_key : get_key:('a -> key) -> 'a list -> 'a t
  val create_mapped : get_key:('r -> key) -> get_data:('r -> 'a) -> 'r list -> 'a t

  include Sexpable.S1 with type 'a sexpable = 'a t
end

module type S2 = sig
  type ('a, 'b) t = ('a, 'b) MoreLabels.Hashtbl.t
  module T : Types with type 'a key = 'a with type ('a, 'b) t = ('a, 'b) t

  val shadow_add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val clear : ('a, 'b) t -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t
  val create : int -> ('a, 'b) t
  val shadow_find : ('a, 'b) t -> 'a -> 'b list
  val fold : ('a, 'b) t -> init:'c -> f:(key:'a -> data:'b -> 'c -> 'c) -> 'c
  val iter : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit
  val length : ('a, 'b) t -> int
  val mem : ('a, 'b) t -> 'a -> bool
  val remove : ('a, 'b) t -> 'a -> unit
  val replace : ('a, 'b) t -> key:'a -> data:'b -> unit

  val map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
  val mapi : ('a, 'b) t -> f:(key:'a -> data:'b -> 'c) -> ('a, 'c) t
  val filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t
  val filter_mapi : ('a, 'b) t -> f:(key:'a -> data:'b -> 'c option) -> ('a, 'c) t
  val remove_all : ('a, 'b) t -> 'a -> unit
  val find_default : ('a, 'b) t -> 'a -> default:(unit -> 'b) -> 'b
  val find : ('a, 'b) t -> 'a -> 'b option
  val find_exn : ('a, 'b) t -> 'a -> 'b
  val iter_vals : ('a, 'b) t -> f:('b -> unit) -> unit
  val of_alist : ('a * 'b) list -> [ `Ok of ('a, 'b) t | `Duplicate_key of 'a ]
  val of_alist_exn : ('a * 'b) list -> ('a, 'b) t
  val of_alist_shadow : ('a * 'b) list -> ('a, 'b list) t
  val to_alist : ('a, 'b) t -> ('a * 'b) list
  val to_alist_shadow : ('a, 'b) t -> ('a * 'b list) list
  val merge :
    f:(key:'k -> 'a option -> 'b option -> 'c option)
    -> ('k, 'a) t
    -> ('k, 'b) t
    -> ('k, 'c) t
  val keys : ('a, 'b) t -> 'a list
  val data : ('a, 'b) t -> 'b list
  val filter_inplace : ('a, 'b) t -> f:('b -> bool) -> unit
  val filteri_inplace : ('a, 'b) t -> f:('a -> 'b -> bool) -> unit

  val equal : ('a, 'b) t -> ('a, 'b) t -> ('b -> 'b -> bool) -> bool

  val add_to_groups : ('k, 'a) t
    -> get_key:('r -> 'k)
    -> get_data:('r -> 'a)
    -> combine:('a -> 'a -> 'a)
    -> rows:'r list
    -> unit

  val group :
    ?size:int
    -> get_key:('r -> 'k)
    -> get_data:('r -> 'a)
    -> combine:('a -> 'a -> 'a)
    -> 'r list
    -> ('k, 'a) t

  val create_mapped : get_key:('r -> 'k) -> get_data:('r -> 'a) -> 'r list
    -> ('k, 'a) t

  val create_with_key : get_key:('a -> 'k) -> 'a list -> ('k, 'a) t


  include Sexpable.S2 with type ('a, 'b) sexpable = ('a, 'b) t

end

module type Gen = sig
  module T : Types
  include Gen(T).S
end


(* Check that S and S2 are instances of Gen *)
module Check_S (M : S) = (M : Gen(M.T).S)
module Check_S2 (M : S2) = (M : Gen(M.T).S)
