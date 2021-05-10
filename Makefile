.PHONY: all
all: clean cll.native lib

.PHONY: clean
clean:
	ocamlbuild -clean
	./bin/clean.zsh

.PHONY: lib
lib: find_prime.o hash_table.o linked_list.o regex.o malloc_manager.o

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

regex.o:
	gcc -o cll_build/regex.o -c lib/regex.c

malloc_manager.o:
	gcc -o cll_build/malloc_manager.o -c lib/malloc_manager.c

parser.ml:
	ocamlyacc -v parser.mly

