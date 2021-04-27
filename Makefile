.PHONY: all
all: clean cll.native lib

.PHONY: clean
clean:
	ocamlbuild -clean
	./bin/clean.zsh

.PHONY: lib
lib: find_prime.o hash_table.o linked_list.o

.PHONY: test
test:
	./bin/testall.zsh

cll.native:
	opam config exec -- \
	ocamlbuild -use-ocamlfind cll.native

find_prime.o:
	gcc -o cll_build/find_prime.o -c lib/find_prime.c

hash_table.o:
	gcc -o cll_build/hash_table.o -c lib/hash_table.c

linked_list.o:
	gcc -o cll_build/linked_list.o -c lib/linked_list.c

parser.ml:
	ocamlyacc -v parser.mly

