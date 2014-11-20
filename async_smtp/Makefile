ASYNC_SMTP ?= $(shell if ocamlfind query async_smtp >/dev/null 2>&1; then echo 1; fi)

ifeq ($(ASYNC_SMTP), 1)
TARGETS	+= test_smtp.native
endif

OCAMLBUILD_OPTS=-use-ocamlfind

all:
	ocamlbuild $(OCAMLBUILD_OPTS) $(TARGETS)

.PHONY: clean
clean:
	ocamlbuild -clean
