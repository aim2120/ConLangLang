.PHONY: all
all: cll.native

.PHONY: clean
clean:
	ocamlbuild -clean
	rm parser.mli parser.ml parser.output

cll.native:
	ocamlbuild -use-ocamlfind cll.native

parser.ml:
	ocamlyacc -v parser.mly

