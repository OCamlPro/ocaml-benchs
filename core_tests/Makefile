all: core_kernel_test.log core_test.log

core_kernel_test.native: test.ml
	ocamlfind ocamlopt -linkall -linkpkg -package core_kernel -o $@ test.ml

core_test.native: test.ml
	ocamlfind ocamlopt -linkall -linkpkg -package core -thread -o $@ test.ml

core_kernel_test.log: core_kernel_test.native
	./core_kernel_test.native inline-test-runner core_kernel -log -strict
	mv inline_tests.log $@

core_test.log: core_test.native
	./core_test.native inline-test-runner core -log -strict
	mv inline_tests.log $@

clean:
	rm -f *.cm* *.native *.o *.log

.PHONY: all clean
