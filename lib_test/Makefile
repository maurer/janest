export OCAMLMAKEFILE = ../OCamlMakefile

export INCDIRS = ../lib
export LIBDIRS = ../lib
export LIBS = core
export PACKS = bigarray oUnit type-conv sexplib bin_prot res fieldslib
export THREADS = yes
ANNOTATE = true

define PROJ_sub1
  SOURCES = \
            bag_test.ml \
            binary_packing_test.ml \
            common_test.ml \
            comparable_test.ml \
            container_test.ml \
            core_array_test.ml \
            core_list_test.ml \
            core_queue_test.ml \
            core_string_speed_test.ml \
            core_string_test.ml \
            crc_test.ml \
            doubly_linked_test.ml \
            fqueue_test.ml \
						filename_test.ml \
						hash_heap_test.ml \
						core_int_test.ml \
            hash_queue_test.ml \
            hashtbl_test.ml \
            heap_test.ml \
            int_conversions_test.ml \
            interval_test.ml \
						core_set_test.ml \
            pMap_test.ml \
            pSet_test.ml \
            time_test.ml \
            timer_test.ml \
						TZ_test.ml \
						int_set_test.ml \
            core_float_test.ml \
						core_mutex_test.ml \
						core_map_test.ml \
            test.ml \
            test_runner.ml
  
  RESULT = test_runner.exe
  THREADS = yes
  ANNOTATE = true

endef
export PROJ_sub1

ifndef SUBPROJS
  export SUBPROJS = sub1
endif

all:	bc

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
