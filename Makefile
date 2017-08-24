SRCS     := $(wildcard *.ml *.mli)
TARGETS  := bytecrawler
FLAGS    := -I +../obytelib
OCAMLLIB := $(shell ocamlfind printconf stdlib)

all: $(TARGETS)

bytecrawler: bytecrawler.ml
	ocamlfind ocamlc -package csv -linkpkg $(FLAGS) obytelib.cma bytecrawler.ml -o bytecrawler

pervasives.cmo : tests/pervasive.ml
	@cd tests && ocamlc -c -nopervasives pervasive.ml

tests/test.byte : tests/test.ml tests/pervasive.ml tests/prims.c
	cd tests && gcc -c -fPIC -I $(OCAMLLIB) prims.c -o prims.o \
	&& ocamlc -nopervasives -custom prims.o pervasive.ml test.ml -o test.byte \
	&& ocamlclean test.byte -o test.byte


interp : bytecrawler tests/test.byte
	./bytecrawler -i tests/test.byte


test : bytecrawler tests/test.byte
	./bytecrawler tests/test.byte

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
