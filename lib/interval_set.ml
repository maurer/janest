(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Interval_set"

open Std_internal

type 'a t = 'a Interval.t list with sexp

let create pair_list =
  let pair_list =
    List.sort pair_list
      ~cmp:(fun (lb,_ub) (lb',_ub') ->
        ascending lb lb'
      )
  in
  let intervals = List.map pair_list
    ~f:(fun (lbound, ubound) -> Interval.make lbound ubound)
  in
  let intervals = List.filter intervals
    ~f:(fun i -> not (Interval.is_empty i))
  in
  if not (Interval.are_disjoint intervals)
  then failwith "Interval_set.create: intervals were not disjoint"
  else intervals

let contains_set ~container ~contained =
  List.for_all contained
    ~f:(fun contained_interval ->
      List.exists container
        ~f:(fun container_interval ->
          Interval.contains_interval container_interval contained_interval
        )
    )

let contains t x =
  List.exists t ~f:(fun interval -> Interval.contains interval x)

let ubound t =
  if t = [] then failwith "Interval_set.ubound called on empty set"
  else Interval.ubound (List.last t)

let lbound t =
  if t = [] then failwith "Interval_set.lbound called on empty set"
  else Interval.lbound (List.hd_exn t)

let test () =
  let s1 = create [ 1,2; 3,4; 5,6 ] in
  let s2 = create [ 3,5; 10,11 ] in
  let s3 = create [ 3,4 ] in
  assert (contains s2 3);
  assert (contains s2 4);
  assert (not (contains s2 9));
  assert (not (contains s2 11));
  assert (not (contains_set ~container:s2 ~contained:s1));
  assert (not (contains_set ~container:s1 ~contained:s2));
  assert (contains_set ~container:s1 ~contained:s3);
  assert (contains_set ~container:s2 ~contained:s3);
  ()

(* let () = test () *)
    
