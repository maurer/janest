(*pp camlp4o -I `ocamlfind query sexplib` -I `ocamlfind query type-conv` -I `ocamlfind query bin_prot` pa_type_conv.cmo pa_sexp_conv.cmo pa_bin_prot.cmo *)
TYPE_CONV_PATH "Core_gc"

include Caml.Gc

module Int = Core_int
module Sexp = Sexplib.Sexp
let sprintf = Printf.sprintf


let read_finaliser_queue, write_finaliser_queue =
  Thread_safe_queue.one_reader_one_writer ()
;;

let start_finaliser_thread_promise = lazy (
  ignore (
    Thread.create (fun () -> Function.forever (fun () ->
      match read_finaliser_queue () with
      | None -> Thread.delay 1.0
      | Some f ->
          Exn.handle_uncaught ~exit:false f))
      () ))
;;

(* Ocaml permits finalisers to be run in any thread and at any time after the object
 * becomes unreachable -- they are essentially concurrent.  This changes forces all
 * finaliser code to run sequentially and in a fixed thread. *)
let finalise f x =
  Lazy.force start_finaliser_thread_promise;
  let finaliser v = write_finaliser_queue (fun () -> f v) in
  Caml.Gc.finalise finaliser x
;;

module Stat = struct
  type pretty_float = float with bin_io, sexp
  let sexp_of_pretty_float f = Sexp.Atom (sprintf "%.2e" f)

  type pretty_int = int with bin_io, sexp
  let sexp_of_pretty_int i = Sexp.Atom (Core_int.to_string_hum i)

  type t = Caml.Gc.stat = {
    minor_words : pretty_float;
    promoted_words : pretty_float;
    major_words : pretty_float;
    minor_collections : pretty_int;
    major_collections : pretty_int;
    heap_words : pretty_int;
    heap_chunks : pretty_int;
    live_words : pretty_int;
    live_blocks : pretty_int;
    free_words : pretty_int;
    free_blocks : pretty_int;
    largest_free : pretty_int;
    fragments : pretty_int;
    compactions : pretty_int;
    top_heap_words : pretty_int;
  } with bin_io, sexp
  type binable = t
  type sexpable = t
end

module Control = struct
  (* The GC parameters are given as a control record.
     Note that these parameters can also be initialised
     by setting the OCAMLRUNPARAM environment variable.
     See the documentation of ocamlrun. *)
  type t = Caml.Gc.control = {
    mutable minor_heap_size : int; (* The size (in words) of the minor heap. Changing this parameter will trigger a minor collection. Default: 32k. *)
    mutable major_heap_increment : int; (* The minimum number of words to add to the major heap when increasing it. Default: 62k. *)
    mutable space_overhead : int; (* The major GC speed is computed from this parameter. This is the memory that will be "wasted" because the GC does not immediatly collect unreachable blocks. It is expressed as a percentage of the memory used for live data. The GC will work more (use more CPU time and collect blocks more eagerly) if space_overhead is smaller. Default: 80. *)
    mutable verbose : int; (* This value controls the GC messages on standard error output. It is a sum of some of the following flags, to print messages on the corresponding events:
    * 0x001 Start of major GC cycle.
    * 0x002 Minor collection and major GC slice.
    * 0x004 Growing and shrinking of the heap.
    * 0x008 Resizing of stacks and memory manager tables.
    * 0x010 Heap compaction.
    * 0x020 Change of GC parameters.
    * 0x040 Computation of major GC slice size.
    * 0x080 Calling of finalisation functions.
    * 0x100 Bytecode executable search at start-up.
    * 0x200 Computation of compaction triggering condition. Default: 0. *)
    mutable max_overhead : int; (* Heap compaction is triggered when the estimated amount of "wasted" memory is more than max_overhead percent of the amount of live data. If max_overhead is set to 0, heap compaction is triggered at the end of each major GC cycle (this setting is intended for testing purposes only). If max_overhead >= 1000000, compaction is never triggered. Default: 500. *)
    mutable stack_limit : int; (* The maximum size of the stack (in words). This is only relevant to the byte-code runtime, as the native code runtime uses the operating system's stack. Default: 256k. *)
    mutable allocation_policy : int; (** The policy used for allocating in the heap.  Possible values are 0 and 1.  0 is the next-fit policy, which is quite fast but can result in fragmentation.  1 is the first-fit policy, which can be slower in some cases but can be better for programs with fragmentation problems.  Default: 0. *)
  } with bin_io, sexp
  type binable = t
  type sexpable = t
end


let tune ?logger ?minor_heap_size ?major_heap_increment ?space_overhead
    ?verbose ?max_overhead ?stack_limit ?allocation_policy () =
  let c = get () in
  set {
    minor_heap_size = (match minor_heap_size with
    | None -> c.minor_heap_size
    | Some n ->
        Option.iter logger ~f:(fun f -> f
          (Printf.sprintf "Gc.Control.minor_heap_size: %d -> %d"
              c.minor_heap_size n));
        n);
    major_heap_increment = (match major_heap_increment with
    | None -> c.major_heap_increment
    | Some n ->
        Option.iter logger ~f:(fun f -> f
          (Printf.sprintf "Gc.Control.major_heap_increment: %d -> %d"
              c.major_heap_increment n));
        n);
    space_overhead = (match space_overhead with
    | None -> c.space_overhead
    | Some n ->
        Option.iter logger ~f:(fun f -> f
          (Printf.sprintf "Gc.Control.space_overhead: %d -> %d"
              c.space_overhead n));
        n);
    verbose = (match verbose with
    | None -> c.verbose
    | Some n ->
        Option.iter logger ~f:(fun f -> f
          (Printf.sprintf "Gc.Control.verbose: 0x%x -> 0x%x"
              c.verbose n));
        n);
    max_overhead = (match max_overhead with
    | None -> c.max_overhead
    | Some n ->
        Option.iter logger ~f:(fun f -> f
          (Printf.sprintf "Gc.Control.max_overhead: %d -> %d"
              c.max_overhead n));
        n);
    stack_limit = (match stack_limit with
    | None -> c.stack_limit
    | Some n ->
        Option.iter logger ~f:(fun f -> f
          (Printf.sprintf "Gc.Control.stack_limit: %d -> %d"
              c.stack_limit n));
        n);
    allocation_policy = (match allocation_policy with
    | None -> c.allocation_policy
    | Some n ->
        Option.iter logger ~f:(fun f -> f
          (Printf.sprintf "Gc.Control.allocation_policy: %d -> %d"
              c.allocation_policy n));
        n);
  }
