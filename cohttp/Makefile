include ../Makefile.options

LWT = $(shell if ocamlfind query cohttp.lwt >/dev/null 2>&1; then echo 1; fi)
ASYNC = $(shell if ocamlfind query cohttp.async >/dev/null 2>&1; then echo 1; fi)

ifeq ($(LWT), 1)
TARGETS += cohttp_lwt_bench.native
endif

ifeq ($(ASYNC), 1)
TARGETS	+= cohttp_async_bench.native
endif

OCAMLBUILD_OPTS=-use-ocamlfind

all:
	ocamlbuild $(OCAMLBUILD_OPTS) $(TARGETS)

.PHONY: clean
clean:
	ocamlbuild -clean
