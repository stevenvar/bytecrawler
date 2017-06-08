SRCS     := $(wildcard *.ml *.mli)
TARGETS  := bytecrawler
FLAGS    := -I +../obytelib

all: $(TARGETS)

bytecrawler: bytecrawler.ml
	ocamlc $(FLAGS) obytelib.cma bytecrawler.ml -o bytecrawler

pervasives.cmo : tests/pervasive.ml
	@cd tests && ocamlc -c -nopervasives pervasive.ml

test : bytecrawler tests/test.ml tests/pervasive.ml tests/prims.c
	@cd tests && gcc -c -fPIC -I/usr/local/lib/ocaml prims.c -o prims.o
	@cd tests && ocamlc -nopervasives -custom prims.o pervasive.ml test.ml -o test.byte
	@cd tests && ocamlclean test.byte -o test.byte
	@cd tests && ../bytecrawler -i test.byte

clean:
	@rm -f *~ $(TARGETS) \
	tests/*.byte \
	tests/*.cmo \
	tests/*.cmi \
	tests/*.o \
	*.cmo \
	*.cmi \
	*.byte

.PHONY: bytecrawler clean
