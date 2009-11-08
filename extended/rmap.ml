(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Avl_tree"

open Core.Std

type ('k, 'v) tree =
    | Empty
    | Node of ('k, 'v) node

and ('k, 'v) node = {
 key: 'k;
 mutable value: 'v;
 mutable left: ('k, 'v) tree;
 mutable right: ('k, 'v) tree;
 mutable height: int;
} with bin_io

module Raw_impl
  (Key : sig
    type 'a t
    val compare : 'a t -> 'a t -> int
  end) = struct

    module T = struct
      type 'k key = 'k Key.t

      type ('k, 'v) t = {
        mutable root: ('k Key.t, 'v) tree;
        mutable length: int;
      }
    end
    open T

    let invariant t =
      let rec binary_tree = function
        | Empty -> ()
        | Node {left = left; right = right; key = key} ->
            begin match left with
            | Empty -> ()
            | Node {key = left_key} -> assert (Key.compare left_key key < 0)
            end;
            begin match right with
            | Empty -> ()
            | Node {key = right_key} -> assert (Key.compare right_key key > 0)
            end;
            assert (Key.compare key key = 0);
            binary_tree left;
            binary_tree right
      in
      let rec height = function
        | Empty -> 0
        | Node {left = left; right = right} ->
            Int.max (height left) (height right) + 1
      in
      let rec balanced = function
        | Empty -> ()
        | Node {left = left; right = right} ->
            assert (abs (height left - height right) < 3);
            balanced left;
            balanced right
      in
      let rec count = function
        | Empty -> 0
        | Node {left = left; right = right} ->
            1 + count left + count right
      in
      let count = count t.root in
      if count <> t.length then
        failwith (sprintf "missing nodes length: %d, count: %d" t.length count);
      binary_tree t.root;
      balanced t.root

    let create () = { root = Empty; length = 0 }

    let height = function
      | Empty -> 0
      | Node n -> n.height

    let update_height n =
      let new_height = (Int.max (height n.left) (height n.right)) + 1 in
      if new_height <> n.height then (* avoid caml_modify when possible *)
        n.height <- new_height

    let balance tree =
      match tree with
      | Empty -> tree
      | Node ({left = left; right = right} as root_node) as root ->
          let hl = height left and hr = height right in      
          if hl > hr + 2 then begin
            match left with
            | Empty -> assert false
            | Node left_node ->
                if height left_node.left >= height left_node.right then begin
                  root_node.left <- left_node.right;
                  left_node.right <- root;
                  update_height root_node;
                  update_height left_node;
                  left
                end else begin
                  match left_node.right with
                  | Empty -> assert false
                  | Node lr_node as lr ->
                      left_node.right <- lr_node.left;
                      root_node.left <- lr_node.right;
                      lr_node.right <- root;
                      lr_node.left <- left;
                      update_height left_node;
                      update_height root_node;
                      update_height lr_node;
                      lr
                end
          end else if hr > hl + 2 then begin
            match right with
            | Empty -> assert false
            | Node right_node ->
                if height right_node.right >= height right_node.left then begin
                  root_node.right <- right_node.left;
                  right_node.left <- root;
                  update_height root_node;
                  update_height right_node;
                  right
                end else begin
                  match right_node.left with
                  | Empty -> assert false
                  | Node rl_node as rl ->
                      right_node.left <- rl_node.right;
                      root_node.right <- rl_node.left;
                      rl_node.left <- root;
                      rl_node.right <- right;
                      update_height right_node;
                      update_height root_node;
                      update_height rl_node;
                      rl
                end
          end else
              tree
    ;;

    let set_left node tree =
      let tree = balance tree in
      if phys_equal node.left tree then ()
      else 
        node.left <- tree;
      update_height node

    let set_right node tree =
      let tree = balance tree in  
      if phys_equal node.right tree then ()
      else
        node.right <- tree;
      update_height node

    let set_root t tree =
      let tree = balance tree in
      if phys_equal t.root tree then ()
      else
        t.root <- tree;
      match t.root with
      | Empty -> ()
      | Node node ->
          update_height node

    let new_node k v =
      Node { key = k; value = v;
             left = Empty; right = Empty;
             height = 1 }

    let add =
      let rec loop t tree k v =
        match tree with
        | Empty -> 
            t.length <- t.length + 1;       
            new_node k v
        | (Node node) as tree ->
            let c = Key.compare k node.key in
            if c = 0 then
              node.value <- v
            else if c < 0 then
              set_left node (loop t node.left k v)
            else
              set_right node (loop t node.right k v);
            tree
      in
      fun t ~key ~data -> set_root t (loop t t.root key data)

    let find =
      let rec loop tree k =
        match tree with
        | Empty -> None
        | Node node ->
            let c = Key.compare k node.key in
            if c = 0 then Some node.value
            else loop (if c < 0 then node.left else node.right) k
      in
      fun t k -> loop t.root k

    let mem t k = Option.is_some (find t k)
        
    let rec min_elt tree =
      match tree with
      | Empty -> Empty
      | Node {left = Empty} -> tree
      | Node node -> min_elt node.left

    let rec remove_min_elt tree =
      match tree with
      | Empty -> assert false
      | Node ({left = Empty} as node) -> node.right
      | Node node -> set_left node (remove_min_elt node.left); tree

    let merge t1 t2 =
      match (t1, t2) with
      | (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let tree = min_elt t2 in
          match tree with
          | Empty -> Empty
          | Node node ->
              set_right node (remove_min_elt t2);
              set_left node t1;
              tree

    let remove =
      let rec loop tree k =
        match tree with
        | Empty -> Empty
        | Node node ->
            let c = Key.compare k node.key in
            if c = 0 then
              merge node.left node.right
            else if c < 0 then begin
              set_left node (loop node.left k);
              tree
            end else begin
              set_right node (loop node.right k);
              tree
            end
      in
      fun t k ->
        t.length <- Int.max 0 (t.length - 1);
        set_root t (loop t.root k)

    let fold =
      let rec loop acc f = function
        | Empty -> acc
        | Node node ->
            let acc = f ~key:node.key ~data:node.value acc in
            loop (loop acc f node.left) f node.right
      in
      fun t ~init ~f -> loop init f t.root

    let iter t ~f = fold t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)

    let of_alist alist =
      let t = create () in
      let bad_key = ref None in
      try
        List.iter alist ~f:(fun (k, v) ->
          if mem t k then begin
            bad_key := Some k;
            raise Exit
          end else
            add t ~key:k ~data:v);
        `Ok t
      with Exit ->
        match !bad_key with
        | None -> assert false
        | Some k -> `Duplicate_key k

    let to_alist t =
      fold t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc)
      
    let t_of_sexp key_of_sexp value_of_sexp sexp =
      match
        of_alist (list_of_sexp (pair_of_sexp key_of_sexp value_of_sexp) sexp)
      with
      | `Ok t -> t
      | `Duplicate_key _ ->
          failwith "Rmap.t_of_sexp: duplicate key"

    let sexp_of_t sexp_of_key sexp_of_value t =
      sexp_of_list (sexp_of_pair sexp_of_key sexp_of_value) (to_alist t)
  end

module type Key = sig
  type t
  include Sexpable.S with type sexpable = t

  val compare : t -> t -> int
end

module type S = sig
  type key
  type 'a t

  include Sexpable.S1 with type 'a sexpable = 'a t

  val create : unit -> 'a t
  val invariant : 'a t -> unit

  val add : 'a t -> key:key -> data:'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a option
  val fold : 'a t -> init:'b -> f:(key:key -> data:'a -> 'b -> 'b) -> 'b
  val iter : 'a t -> f:(key:key -> data:'a -> unit) -> unit
end

module Make (Key : Key) = struct
  include Raw_impl (struct
    type 'a t = Key.t
    let compare = Key.compare
  end)

  type key = Key.t
  type 'v t = (Key.t, 'v) T.t
  type 'a sexpable = 'a t
  let t_of_sexp a_of_sexp sexp = t_of_sexp Key.t_of_sexp a_of_sexp sexp
  let sexp_of_t sexp_of_a t = sexp_of_t Key.sexp_of_t sexp_of_a t
end

module Key = struct
  type 'a t = 'a
  let compare = Pervasives.compare
end

include Raw_impl (Key)

type ('a, 'b) t = ('a, 'b) T.t = {
  mutable root: ('a, 'b) tree;
  mutable length: int;
} with bin_io
  
type ('a, 'b) binable = ('a, 'b) t
type ('a, 'b) sexpable = ('a, 'b) t
