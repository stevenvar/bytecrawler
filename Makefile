SRCS     := src/mlstack.ml src/state.ml src/intrp.ml src/cycles.ml src/bytecrawler.ml
OPAMDIR   := $(shell opam config var lib)
TESTS	 := $(wildcard test*.ml)
TARGETS  := bin/bytecrawler
FLAGS    := -I +../obytelib -I src
OCAMLLIB := $(shell ocamlfind printconf stdlib)
OCAMLC   := ocamlc
PACKAGE := csv

CMOFILES = $(SRCS:%.ml=%.cmo)

all: $(TARGETS)

bin/bytecrawler:  $(CMOFILES)
	ocamlfind ocamlc -linkpkg -package $(PACKAGE) $(FLAGS) obytelib.cma  $+ -o $@

%.cmo: %.ml %.cmi
	ocamlfind ocamlc -package $(PACKAGE) $(FLAGS) -c $*.ml

%.cmi: %.mli %.ml
	ocamlfind ocamlc -package $(PACKAGE) $(FLAGS)  -c $*.mli

%.cmo: %.ml
	ocamlfind ocamlc -package $(PACKAGE) $(FLAGS)  -c $*.ml



pervasives.cmo : tests/pervasive.ml
	@cd tests && ocamlc -c -nopervasives pervasive.ml

tests/test.byte : tests/test.ml tests/pervasive.ml tests/prims.c
	cd tests && gcc -c -fPIC -I $(OCAMLLIB) prims.c -o prims.o \
	&& ocamlc -nopervasives -custom prims.o pervasive.ml test.ml -o test.byte \
	&& ocamlclean test.byte -o test.byte

interp : bytecrawler tests/test.byte
	./bytecrawler -i tests/test.byte


test : ./bytecrawler
	cd tests && make


clean:
	rm -f .depend
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -r ./bin/


.PHONY: bytecrawler clean

.depend: $(MLSOURCES) Makefile
	ocamldep -I src $(SRCS) > .depend

include .depend
