(*pp $SEXPLIB_CPP $ARCH_FLAGS *)

include Std_internal

type date = Time.Date.t = private { y : int; m : Month.t; d : int; }

module Arg = Core_arg
module Backtrace = Backtrace
module Bag = Bag
module Bigbuffer = Bigbuffer
module Bigstring = Bigstring
module Binable = Binable
#ifdef __linux__
module Linux_ext = Linux_ext
#else
#warning "linux_ext not supported, not being included in Core.Std"
#endif
module Binary_packing = Binary_packing
module Bool = Bool
module Bucket = Bucket
module Caml = Caml
module Common = Common (* CRv2 sweeks: Is this needed? *)
module Comparable = Comparable
module Container = Container
module Crc = Crc
module Date = Time.Date
module Dequeue = Dequeue
module Doubly_linked = Doubly_linked
module Error_check = Error_check
module Exn = Exn
module Fast_hashtbl = Fast_hashtbl
module Float = Float
module Force_once = Force_once
module Fqueue = Fqueue
module Filename = Core_filename
module Gc = Core_gc
module Hash_queue = Hash_queue
module Hash_set = Hash_set
module Hashable = Hashable
module Heap = Heap
module In_channel = In_channel
module Interfaces = Interfaces
module Interned = Interned
module Interval = Interval
module Interval_set = Interval_set
module Int_intf = Int_intf
module Linebuf = Linebuf
module Memo = Memo
module Monad = Monad
module Month = Month
module Mutex = Core_mutex
module Ofday = Time.Ofday
module Option = Option
module Out_channel = Out_channel
module OUnit_utils = OUnit_utils
module Piecewise_linear = Piecewise_linear
module Pretty_printer = Pretty_printer
module Quickcheck = Quickcheck
module Result = Result
module Sexpable = Sexpable
module Size = Size
module Space_safe_tuple2 = Space_safe_tuple.T2
module Space_safe_tuple3 = Space_safe_tuple.T3
module Squeue = Squeue
module Thread_safe_queue = Thread_safe_queue
module Time = Time
module Timer = Timer
module Tuple2 = Tuple.T2
module Tuple3 = Tuple.T3
module Unique_id = Unique_id
module Unix_ext = Unix_ext

