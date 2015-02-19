all: bdd.native

bdd.native: bdd.ml
	ocamlopt -o $@ $^

clean:
	rm -rf *.cm* *.native *.o
