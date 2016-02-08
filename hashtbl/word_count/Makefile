.PHONY: all
all: data.txt word_count.native word_count.byte

data.txt: data/*.txt
	cat data/*.txt > $@

%.native: %.ml
	ocamlfind ocamlopt -linkpkg $< -o $@

%.byte: %.ml
	ocamlfind ocamlc   -linkpkg $< -o $@
