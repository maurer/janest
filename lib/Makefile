OCAMLMAKEFILE = ../OCamlMakefile

ifeq ($(shell uname -s), Linux)
  LINUX_EXT_STUBS_C = linux_ext_stubs.c
  LINUX_EXT_ML = linux_ext.mli linux_ext.ml
	CLIBS += rt
endif

SOURCES = \
	float_robust_compare.ml \
  core_printf.mli \
  core_printf.ml \
  pretty_printer.mli \
  pretty_printer.ml \
  caml.ml \
  monad.ml \
  stringable.ml \
  sexpable.ml \
  container.ml \
  option.mli \
  option.ml \
  ordered_collection_common.mli \
	ordered_collection_common.ml \
	core_set_intf.ml \
	array_permute.ml \
  core_array.mli \
	core_array.ml \
	core_set.mli \
	core_set.ml \
  core_list.mli \
	core_list.ml \
	core_hashtbl_intf.ml \
	core_hashtbl.mli \
	core_hashtbl.ml \
  doubly_linked.mli \
  doubly_linked.ml \
  hash_queue.ml \
  hash_set_intf.ml \
	hash_set.mli \
  hash_set.ml \
  heap.mli \
	heap.ml \
	hash_heap.ml \
  hashable.ml \
  binable.ml \
  floatable.ml \
  robustly_comparable.ml \
	core_map_intf.ml \
	core_map.mli \
	core_map.ml \
  comparable.ml \
  interfaces.ml \
  core_sexp.mli \
  core_sexp.ml \
  result.mli \
  result.ml \
  core_char.mli \
  core_char.ml \
  core_string.mli \
  core_string.ml \
  in_channel.mli \
  in_channel.ml \
  exn.mli \
  exn.ml \
  float.mli \
  float.ml \
  out_channel.mli \
	out_channel.ml \
  tuple.mli \
	tuple.ml \
  space_safe_tuple.mli \
	space_safe_tuple.ml \
  word_size.mli \
	word_size.ml \
  int_conversions.ml \
  core_int.mli \
	core_int.ml \
	core_int32.mli \
	core_int32.ml \
  core_int64.mli \
	core_int64.ml \
  core_int63.mli \
	core_int63.ml \
	signal.mli \
	signal.ml \
	terminal_io_intf.ml \
  core_unix.mli \
	core_unix.ml \
  core_filename.mli \
	core_filename.ml \
	mutex0.ml \
	function.mli \
	function.ml \
  common.mli \
  common.ml \
  int_intf.ml \
  unix_ext.mli \
  unix_ext.ml \
  core_queue.mli \
  core_queue.ml \
  core_stack.mli \
  core_stack.ml \
	core_sys.mli \
  core_sys.ml \
  core_nativeint.mli \
  core_nativeint.ml \
  std_internal.ml \
	unique_id.mli \
  unique_id.ml \
	month.mli \
	month.ml \
	weekday.mli \
	weekday.ml \
  time.mli \
  time.ml \
  core_mutex.mli \
	core_mutex.ml \
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
  crc.mli \
  crc.ml \
  dequeue.mli \
  dequeue.ml \
	error_check.mli \
  error_check.ml \
  fast_hashtbl.mli \
  fast_hashtbl.ml \
  force_once.mli \
  force_once.ml \
  fqueue.mli \
  fqueue.ml \
  thread_safe_queue.mli \
  thread_safe_queue.ml \
	interval_intf.ml \
  interval.mli \
  interval.ml \
	linebuf.mli \
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
  core_gc.mli \
  core_gc.ml \
  ref.mli \
  ref.ml \
	agnostic_mutex.mli \
	agnostic_mutex.ml \
	substring_intf.ml \
	make_substring.ml \
	bigsubstring.mli \
	bigsubstring.ml \
	byte_units.mli \
	byte_units.ml \
	core_condition.mli \
	core_condition.ml \
	core_thread.mli \
	core_thread.ml \
	daemon.mli \
	daemon.ml \
	int_set.mli \
	int_set.ml \
	core_field.ml \
	lock_file.mli \
	lock_file.ml \
	core_printexc.mli \
	core_printexc.ml \
	set_once.mli \
	set_once.ml \
	substring.ml \
	TZ.mli \
	TZ.ml \
	low_level_debug.mli \
	low_level_debug.ml \
  std.ml bigstring_stubs.c $(LINUX_EXT_STUBS_C) \
  misc.c \
  ocaml_utils_stubs.c \
  unix_stubs.c \
  unix_ext_stubs.c \
	signal_stubs.c \
  low_level_debug_stubs.c \

LIB_PACK_NAME = core
INCDIRS = +camlp4 .
OCAMLFLAGS = -for-pack Core -dtypes
CFLAGS = \
  -pipe -g -fPIC -O2 -fomit-frame-pointer \
  -Wall -pedantic -Wextra -Wunused -Wno-long-long
PACKS = res sexplib bin_prot fieldslib
RESULT = core
THREADS = yes
ANNOTATE = true

all: byte-code-library native-code-library

LIBINSTALL_FILES = \
  core.cmi core.cma core.cmxa core.a libcore_stubs.a dllcore_stubs.so

install:	libinstall
uninstall:	libuninstall

clean::	clean-doc

-include $(OCAMLMAKEFILE)
