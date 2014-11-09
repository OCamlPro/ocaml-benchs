include ../Makefile.options

ifeq ($(LWT), 1)
TARGETS += test_lwt.native
endif

ifeq ($(ASYNC), 1)
TARGETS	+= test_async.native
endif

OCAMLBUILD_OPTS=-use-ocamlfind

all:
	ocamlbuild $(OCAMLBUILD_OPTS) $(TARGETS)

.PHONY: clean
clean:
	ocamlbuild -clean
