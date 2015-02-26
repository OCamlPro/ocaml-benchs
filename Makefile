include Makefile.options

all:
	make -C chameneos-redux all
	make -C lexifi-g2pp all
	make -C sauvola all
	make -C thread all
	make -C valet all
	make -C async_smtp all
	make -C cohttp all
	make -C core-micro all
	make -C kb all
	make -C almabench all
	make -C bdd all
	make -C sequence all
ifeq ($(CORE), 1)
	make -C core_test all
endif

.PHONY: clean
clean:
	make -C chameneos-redux clean
	make -C lexifi-g2pp clean
	make -C sauvola clean
	make -C thread clean
	make -C valet clean
	make -C async_smtp clean
	make -C	cohttp clean
	make -C core-micro clean
	make -C kb clean
	make -C almabench clean
	make -C bdd clean
	make -C sequence clean
	make -C core_test clean

