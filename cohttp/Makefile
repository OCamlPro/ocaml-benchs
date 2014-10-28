OCAMLBUILD_OPTS=-use-ocamlfind

all:
	ocamlbuild $(OCAMLBUILD_OPTS) cohttp_async_bench.native cohttp_lwt_bench.native

.PHONY: clean
clean:
	ocamlbuild -clean
