CMX=yojson.cmx yojson_biniou.cmx
JSON=`ocamlfind query yojson`
PACKS = easy-format,biniou

all:
	ocamlfind ocamlopt -I $(JSON) -o ydump_bench -package $(PACKS) -linkpkg $(CMX) ydump.ml

clean:
	rm -rf *.cm* ydump_bench *.o
