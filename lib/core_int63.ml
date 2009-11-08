(*pp cpp *)

#ifdef ARCH_SIXTYFOUR
include Core_int
let to_int x = Some x
#else
include Core_int64
#endif

let () = assert (Core_int.(>=) num_bits 63);
