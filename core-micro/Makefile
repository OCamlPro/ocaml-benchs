include ../Makefile.options

ifeq ($(CORE), 1)
TARGETS += runner.native
endif

all:
	ocamlbuild -use-ocamlfind $(TARGETS)

.PHONY: clean
clean:
	ocamlbuild -clean
