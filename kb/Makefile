all: kb.native kb_no_exc.native

kb.native: kb.ml
	ocamlopt -o $@ $^

kb_no_exc.native: kb_no_exc.ml
	ocamlopt -o $@ $^

clean:
	rm -rf *.cm* *.native *.o
