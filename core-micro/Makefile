CORE_BENCH ?= $(shell if ocamlfind query core_bench >/dev/null 2>&1; then echo 1; fi)

ifeq ($(CORE_BENCH), 1)
TARGETS += runner.native
endif

all:
	ocamlbuild -use-ocamlfind $(TARGETS)

.PHONY: clean
clean:
	ocamlbuild -clean
