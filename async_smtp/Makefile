OCAMLBUILD_OPTS=-use-ocamlfind

all:
	ocamlbuild $(OCAMLBUILD_OPTS) test_smtp.native

.PHONY: clean
clean:
	ocamlbuild -clean
