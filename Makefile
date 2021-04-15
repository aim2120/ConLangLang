.PHONY: all
all: clean cll.native test

.PHONY: clean
clean:
	ocamlbuild -clean
	./bin/clean.zsh

.PHONY: test
test:
	./bin/testall.zsh

cll.native:
	ocamlbuild -use-ocamlfind cll.native

parser.ml:
	ocamlyacc -v parser.mly

