(*pp cpp *)
include Extended_common

module Array = struct
  include Core.Std.Array
  include Extended_array
end
module Cbuffer = Cbuffer
module Command = Command
module Dispatch_arg = Dispatch_arg
module Exception_check = Exception_check
module Escaping = Escaping
module Exn = struct
  include Core.Std.Exn
  include Extended_exn
end
module Filename = struct
  include Core.Std.Filename
  include Extended_filename
end
module Find = Find
module Fold_map = Fold_map
module Float = struct
  include Core.Float
  include Extended_float
end
module Gc = struct
  include Core.Std.Gc
  include Extended_gc
end
module Hashtree = Hashtree
module Int_set = Int_set
module Int_set_simple = Core.Std.Int_set
module Iter = Iter
#ifdef __linux__
module Linux_ext = struct
  include Core.Std.Linux_ext
  include Extended_linux
end
#endif
module List = struct
  include Core.Std.List
  include Extended_list
end
module Logger = Logger
module Loggers = Loggers
module Lru = Lru
module Net_utils = Net_utils
module Ocaml_utils = Ocaml_utils
module Pp = Pp
module Process = Process
#ifdef __linux__
module Procfs = Procfs
#endif
module Prompt = Prompt
module Readline = Readline
module Res_array = Res_array
module Result = struct
  include Core.Std.Result
  include Extended_result
end
module Rmap = Rmap
module Rw_mutex = Rw_mutex
module Semaphore = Semaphore
module Sendmail = Sendmail
module Sexp = struct
  include Core.Std.Sexp
  include Extended_sexp
end
module Shell = Shell
module String = struct
  include Core.Std.String
  include Extended_string
end
module Syslog = Syslog
module Union_find = Union_find
module Unix = struct
  include Core.Std.Unix
  include Extended_unix
end
