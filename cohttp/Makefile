TARGETS= cohttp_lwt_bench.native cohttp_async_bench.native

OCAMLBUILD_OPTS=-use-ocamlfind

all:
	ocamlbuild $(OCAMLBUILD_OPTS) $(TARGETS)

.PHONY: clean
clean:
	ocamlbuild -clean
