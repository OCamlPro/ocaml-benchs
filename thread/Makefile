TARGETS= thread_sleep_lwt thread_ring_lwt_mvar thread_ring_lwt_stream \
	 thread_sleep_async thread_ring_async_pipe

all: $(TARGETS)

thread_sleep_lwt: thread_sleep_lwt.ml
	ocamlfind ocamlopt -package lwt.unix -linkpkg -o $@ $^

thread_sleep_async: thread_sleep_async.ml
	ocamlfind ocamlopt -thread -package async -linkpkg -o $@ $^

thread_ring_lwt_mvar: thread_ring_lwt_mvar.ml
	ocamlfind ocamlopt -package lwt.unix -linkpkg -o $@ $^

thread_ring_lwt_stream: thread_ring_lwt_stream.ml
	ocamlfind ocamlopt -package lwt.unix -linkpkg -o $@ $^

thread_ring_async_pipe: thread_ring_async_pipe.ml
	ocamlfind ocamlopt -thread -package async -linkpkg -o $@ $^

.PHONY: clean
clean:
	rm -f *.cm* *.o $(TARGETS)
