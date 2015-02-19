all: almabench.native

almabench.native: almabench.ml
	ocamlopt -o $@ $^

clean:
	rm -rf *.cm* *.native *.o
