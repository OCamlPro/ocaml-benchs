SOURCES = echo_common.ml echo_client.ml echo_server.ml
PACKAGES = -package async -package bin_prot.syntax -package core
OPTIONS = -thread -syntax camlp4o
OPTOBJS = $(SOURCES:.ml=.cmx)

.SUFFIXES: .ml .cmx

all: echo_client.native echo_server.native echo_merge.native

echo_client.native: $(OPTOBJS) echo_client_main.cmx
	ocamlfind ocamlopt -linkpkg $(OPTIONS) $(PACKAGES) $(OPTOBJS) echo_client_main.cmx -o echo_client.native

echo_server.native: $(OPTOBJS) echo_server_main.cmx
	ocamlfind ocamlopt -linkpkg $(OPTIONS) $(PACKAGES) $(OPTOBJS) echo_server_main.cmx -o echo_server.native

echo_merge.native: $(OPTOBJS) echo_merge.cmx
	ocamlfind ocamlopt -linkpkg $(OPTIONS) $(PACKAGES) $(OPTOBJS) echo_merge.cmx -o echo_merge.native

.ml.cmx:
	ocamlfind ocamlopt $(OPTIONS) $(PACKAGES) -c $<


.PHONY: clean
clean:
	rm -f *.cm[iox]
	rm -f *.o
	rm -f *.native