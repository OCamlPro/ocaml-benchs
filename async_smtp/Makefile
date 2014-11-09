include ../Makefile.options

ifeq ($(ASYNC), 1)
TARGETS	+= test_smtp.native
endif

OCAMLBUILD_OPTS=-use-ocamlfind

all:
	ocamlbuild $(OCAMLBUILD_OPTS) $(TARGETS)

.PHONY: clean
clean:
	ocamlbuild -clean
