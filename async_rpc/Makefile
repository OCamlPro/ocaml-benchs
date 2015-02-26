SOURCES = rpc_common.ml rpc_protocol.ml rpc_client.ml rpc_server.ml
PACKAGES = -package async -package bin_prot.syntax -package core
OPTIONS = -thread -syntax camlp4o
OPTOBJS = $(SOURCES:.ml=.cmx)

.SUFFIXES: .ml .cmx

all: $(OPTOBJS)
	ocamlfind ocamlopt -linkpkg $(OPTIONS) $(PACKAGES) $(OPTOBJS) -o rpc_server.native

.ml.cmx:
	ocamlfind ocamlopt $(OPTIONS) $(PACKAGES) -c $<


.PHONY: clean
clean:
	rm -f *.cm[iox]
	rm -f *.o
	rm -f rpc_server.native