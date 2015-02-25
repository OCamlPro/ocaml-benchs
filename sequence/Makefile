all: test.native test_cps.native

sequence_cps.cmx: sequence_cps.ml
	ocamlopt -c -o $@ $<

sequence.cmx: sequence.ml
	ocamlopt -c -o $@ $<

test_cps.cmx: test_cps.ml sequence_cps.cmx
	ocamlopt -c -o $@ $<

test.cmx: test.ml sequence.cmx
	ocamlopt -c -o $@ $<

test_cps.native: sequence_cps.cmx test_cps.cmx
	ocamlopt -o $@ $^

test.native: sequence.cmx test.cmx
	ocamlopt -o $@ $^

clean:
	rm -rf *.cm* *.native *.o
