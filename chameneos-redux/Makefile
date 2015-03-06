TARGETS=chameneos_redux_th chameneos_redux_evtchn chameneos_redux_lwt chameneos_redux_async

all: $(TARGETS)

chameneos_redux_lwt: chameneos_redux_lwt.ml
	ocamlfind ocamlopt -bin-annot -package lwt.unix -linkpkg $^ -o $@

chameneos_redux_async: chameneos_redux_async.ml
	ocamlfind ocamlopt -bin-annot  -thread -package async -linkpkg $^ -o $@

chameneos_redux_th: chameneos_redux_th.ml
	ocamlopt -bin-annot -thread unix.cmxa threads.cmxa $^ -o $@

chameneos_redux_evtchn: chameneos_redux_evtchn.ml
	ocamlopt -bin-annot -thread unix.cmxa threads.cmxa $^ -o $@

.PHONY: clean

clean:
	rm -f *.cm* *.o $(TARGETS)
