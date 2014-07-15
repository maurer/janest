#!/usr/bin/make -f

CONF = --prefix /usr --destdir '$(DESTDIR)'

ifneq ($(DOCPKG),)
CONF += --docdir /usr/share/doc/$(DOCPKG)/html/api
endif

ifneq ($(TEST),n)
CONF += --enable-tests
TARG = build test install
else
TARG = build install
endif

all: $(TARG)

configure: clean
	ocaml setup.ml -configure $(CONF)

build: configure
	ocaml setup.ml -build
	ocaml setup.ml -doc

test:
	ocaml setup.ml -test

install:
	mkdir -p '$(OCAMLFIND_DESTDIR)'
	mkdir -p '$(DESTDIR)$(OCAML_DLL_DIR)'
	ocaml setup.ml -install
	rm -f '$(DESTDIR)$(OCAML_DLL_DIR)'/*.so.owner

clean:
	ocaml setup.ml -distclean

.PHONY: all configure clean install test build
