TARGETS= test_lwt.native test_async.native

OCAMLBUILD_OPTS=-use-ocamlfind

all:
	ocamlbuild $(OCAMLBUILD_OPTS) $(TARGETS)

.PHONY: clean
clean:
	ocamlbuild -clean
