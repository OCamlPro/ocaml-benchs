OCAMLBUILD_OPTS=-use-ocamlfind
SRC=test_lwt.native test_async.native

all:
	ocamlbuild $(OCAMLBUILD_OPTS) $(SRC)

.PHONY: clean
clean:
	ocamlbuild -clean
