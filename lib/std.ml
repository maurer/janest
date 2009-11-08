(*pp cpp *)

(* We list the modules we want to export here and follow the convention of opening
   Core.Std instead of Core. *)



include Std_internal

(* Included here instead of in common because time depends on common *)

type date = Time.Date.t = private { y : int; m : Month.t; d : int; }

let sec = Time.Span.of_sec

module Agnostic_mutex = Agnostic_mutex
module Arg = Core_arg
module Backtrace = Backtrace
module Bag = Bag
module Bigbuffer = Bigbuffer
module Bigstring = Bigstring
module Bigsubstring = Bigsubstring
module Binable = Binable
#ifdef __linux__
module Linux_ext = Linux_ext
#else
#warning "linux_ext not supported, not being included in Core.Std"
#endif
module Binary_packing = Binary_packing
module Bool = Bool
module Bucket = Bucket
module Byte_units = Byte_units
module Caml = Caml
(*module Common = Common *)
module Comparable = Comparable
module Condition = Core_condition
module Container = Container
module Crc = Crc
module Date = Time.Date
module Daemon = Daemon
module Dequeue = Dequeue
module Doubly_linked = Doubly_linked
module Error_check = Error_check
module Exn = Exn
module Fast_hashtbl = Fast_hashtbl
module Float = Float
module Force_once = Force_once
module Fqueue = Fqueue
module Filename = Core_filename
module Floatable = Floatable
module Function = Function
module Gc = Core_gc
module Hash_queue = Hash_queue
module Hash_heap = Hash_heap
module Hash_set = Hash_set
module Hashable = Hashable
module Heap = Heap
module In_channel = In_channel
module Int63 = Core_int63
module Int_intf = Int_intf
module Int_set = Int_set
module Interfaces = Interfaces
module Interval = Interval
module Field = Core_field
module Linebuf = Linebuf
module Lock_file = Lock_file
module Memo = Memo
module Monad = Monad
module Month = Month
module Mutex = Core_mutex
module Option = Option
module Out_channel = Out_channel
module Piecewise_linear = Piecewise_linear
module Pretty_printer = Pretty_printer
module Printexc = Core_printexc
module Quickcheck = Quickcheck
module Result = Result
module Set_once = Set_once
module Sexpable = Sexpable
module Signal = Signal
module Space_safe_tuple2 = Space_safe_tuple.T2
module Space_safe_tuple3 = Space_safe_tuple.T3
module Squeue = Squeue
module Stringable = Stringable
module Substring = Substring
module Substring_intf = Substring_intf
module Thread = Core_thread
module Thread_safe_queue = Thread_safe_queue
module Time = Time
module Timer = Timer
module Tuple2 = Tuple.T2
module Tuple3 = Tuple.T3
module TZ = TZ
module Unique_id = Unique_id
module Unix_ext = Unix_ext
module Weekday = Weekday
module Word_size = Word_size
