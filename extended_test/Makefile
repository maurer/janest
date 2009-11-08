export OCAMLMAKEFILE = ../OCamlMakefile

export INCDIRS = ../extended ../lib
export LIBDIRS = ../extended ../lib
export LIBS = core core_extended
export PACKS = bigarray oUnit type-conv sexplib bin_prot res fieldslib pcre
export THREADS = yes
ANNOTATE = true

ifeq ($(shell uname -s), Linux)
  EXTENDED_LINUX_TEST_ML = extended_linux_test.ml
endif

define PROJ_sub1
  SOURCES = \
    cbuffer_test.ml \
    condition_test.ml \
    $(EXTENDED_LINUX_TEST_ML) \
    escaping_test.ml \
    extended_array.ml \
    extended_float.ml \
    extended_list.ml \
    extended_string.ml \
    int_set_test.ml \
    iter_test.ml \
    lru_test.ml \
		rmap_test.ml \
    shell_test.ml \
    union_find_test.ml \
    test.ml \
		test_runner.ml \
  
  RESULT = test_runner.exe
  THREADS = yes
  ANNOTATE = true
  #OCAMLFLAGS = -g
  OCAMLLDFLAGS = -g

endef
export PROJ_sub1

ifndef SUBPROJS
  export SUBPROJS = sub1
endif

all:	bc

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
