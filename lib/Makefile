OCAMLMAKEFILE = ../OCamlMakefile

ifeq ($(shell uname -s), Linux)
  LINUX_EXT_STUBS_C = linux_ext_stubs.c
  LINUX_EXT_ML = linux_ext.ml
endif

SOURCES = \
  core_printf.mli \
  core_printf.ml \
  pretty_printer.mli \
  pretty_printer.ml \
  caml.ml \
  monad.ml \
  stringable.ml \
  sexpable.ml \
  core_hashtbl.mli \
  core_hashtbl.ml \
  container.ml \
  option.mli \
  option.ml \
  doubly_linked.mli \
  doubly_linked.ml \
  hash_queue.ml \
  hash_set.ml \
  hashable.ml \
  binable.ml \
  floatable.ml \
  robustly_comparable.ml \
  setable.ml \
  comparable.ml \
  interfaces.ml \
  core_sexp.mli \
  core_sexp.ml \
  result.mli \
  result.ml \
  ordered_collection_common.ml \
  core_char.mli \
  core_char.ml \
  pSet.mli \
  pSet.ml \
  core_string.mli \
  core_string.ml \
  in_channel.mli \
  in_channel.ml \
  exn.mli \
  exn.ml \
  core_mutex.mli \
  core_mutex.ml \
  float.mli \
  float.ml \
  core_list.mli \
  core_list.ml \
  pMap.mli \
  pMap.ml \
  common.mli \
  common.ml \
  core_array.mli \
  core_array.ml \
  word_size.mli \
  word_size.ml \
  int_conversions.ml \
  int_intf.ml \
  core_int.mli \
  core_int.ml \
  core_int32.mli \
  core_int32.ml \
  core_int64.mli \
  core_int64.ml \
  unix_ext.mli \
  unix_ext.ml \
  core_unix.mli \
  core_unix.ml \
  core_queue.mli \
  core_queue.ml \
  core_stack.mli \
  core_stack.ml \
  core_sys.ml \
  core_nativeint.mli \
  core_nativeint.ml \
  std_internal.ml \
  unique_id.ml \
  month.mli \
  month.ml \
  time.mli \
  time.ml \
  OUnit_utils.ml \
  backtrace.ml \
  bag.mli \
  bag.ml \
  bigstring.mli \
  bigstring.ml \
  bigbuffer.mli \
  bigbuffer.ml \
  binary_packing.mli \
  binary_packing.ml \
  bool.mli \
  bool.ml \
  bucket.ml \
  core_arg.mli \
  core_arg.ml \
  core_filename.mli \
  core_filename.ml \
  crc.mli \
  crc.ml \
  dequeue.mli \
  dequeue.ml \
  error_check.ml \
  fast_hashtbl.mli \
  fast_hashtbl.ml \
  force_once.mli \
  force_once.ml \
  fqueue.mli \
  fqueue.ml \
  heap.mli \
  heap.ml \
  thread_safe_queue.mli \
  thread_safe_queue.ml \
  interned.ml \
  interval.mli \
  interval.ml \
  interval_set.mli \
  interval_set.ml \
  linebuf.ml $(LINUX_EXT_ML) \
  squeue.mli \
  squeue.ml \
  memo.mli \
  memo.ml \
  piecewise_linear.mli \
  piecewise_linear.ml \
  quickcheck.ml \
  timer.mli \
  timer.ml \
  tuple.mli \
  tuple.ml \
  core_gc.mli \
  core_gc.ml \
  out_channel.mli \
  out_channel.ml \
  space_safe_tuple.mli \
  space_safe_tuple.ml \
  size.mli \
  size.ml \
  ref.mli \
  ref.ml \
  std.ml bigstring_stubs.c $(LINUX_EXT_STUBS_C) \
  misc.c \
  ml_backtrace.c \
  ocaml_utils_stubs.c \
  unix_ext_stubs.c

LIB_PACK_NAME = core
INCDIRS = +camlp4 .
OCAMLFLAGS = -for-pack Core -dtypes
CFLAGS = \
  -pipe -g -fPIC -O2 -fomit-frame-pointer \
  -Wall -pedantic -Wextra -Wunused -Wno-long-long
PACKS = res sexplib bin_prot
RESULT = core
THREADS = yes
ANNOTATE = true

all: byte-code-library native-code-library
byte: byte-code-library

LIBINSTALL_FILES = \
  core.cmi core.cma core.cmxa core.a libcore_stubs.a dllcore_stubs.so

install:	libinstall
uninstall:	libuninstall

clean::	clean-doc

-include $(OCAMLMAKEFILE)
