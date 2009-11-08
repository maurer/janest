.PHONY: all
all:
	@cd lib && $(MAKE)
	@cd extended && $(MAKE)

.PHONY:	install
install:
	@cd lib && $(MAKE) $@
	@cd extended && $(MAKE) $@

.PHONY:	uninstall
uninstall:
	@cd lib && $(MAKE) $@
	@cd extended && $(MAKE) $@

.PHONY:	clean
clean:
	@cd lib && $(MAKE) clean
	@cd extended && $(MAKE) clean
