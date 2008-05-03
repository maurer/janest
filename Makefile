.PHONY: all
all:
	@cd lib && $(MAKE)
	@cd lib_test && make

.PHONY: byte
byte:
	@cd lib && $(MAKE) byte
	@cd lib_test && make

.PHONY:	install
install:
	@cd lib && $(MAKE) $@

.PHONY:	libinstall-byte-code
libinstall-byte-code:
	@cd lib && $(MAKE) $@

.PHONY:	uninstall
uninstall:
	@cd lib && $(MAKE) $@

.PHONY:	clean
clean:
	@cd lib && $(MAKE) clean
	@cd lib_test && $(MAKE) clean
