(*pp $(pwd)/pp.sh *)
(*
#include <unistd.h>
end-pp-include*)
include Mutex0

#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)
let timedlock mtx time = Unix_ext.mutex_timedlock mtx (Time.to_float time)
#else
#warning "POSIX TMO not present; Core_mutex.timedlock unavailable"
#endif
