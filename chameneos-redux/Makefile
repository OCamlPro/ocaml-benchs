all: \
	chameneos_redux_lwt \
	chameneos_redux_async \
	chameneos_redux_th \
	chameneos_redux_evtchn

chameneos_redux_lwt: chameneos_redux_lwt.ml
	ocamlfind ocamlopt -bin-annot -noassert -unsafe -nodynlink -inline 100 -package lwt.unix -linkpkg $^ -o $@

chameneos_redux_async: chameneos_redux_async.ml
	ocamlfind ocamlopt -bin-annot -noassert -unsafe -nodynlink -inline 100 -thread -package async -linkpkg $^ -o $@

chameneos_redux_th: chameneos_redux_th.ml
	ocamlopt -bin-annot -noassert -unsafe -nodynlink -inline 100 -thread unix.cmxa threads.cmxa $^ -o $@

chameneos_redux_evtchn: chameneos_redux_evtchn.ml
	ocamlopt -bin-annot -noassert -unsafe -nodynlink -inline 100 -thread unix.cmxa threads.cmxa $^ -o $@

.PHONY: clean

clean:
	rm -f *.cm* *.o \
	chameneos_redux_th \
	chameneos_redux_evtchn \
	chameneos_redux_lwt \
	chameneos_redux_async
