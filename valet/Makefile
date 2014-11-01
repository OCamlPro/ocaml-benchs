OCAMLBUILD_OPTS=-use-ocamlfind
SRC=test_lwt.native test_async.native valet_graph.native valet_graph_lwt.native

all:
	ocamlbuild $(OCAMLBUILD_OPTS) $(SRC)

.PHONY: clean
clean:
	ocamlbuild -clean
