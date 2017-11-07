SRCS     := $(wildcard *.ml *.mli)
TESTS	 := $(wildcard test*.ml)
TARGETS  := bin/bytecrawler
FLAGS    := -I +../obytelib -I +src
OCAMLLIB := $(shell ocamlfind printconf stdlib)
PACKAGE := csv

all: $(TARGETS)

bin/bytecrawler:  src/mlstack.ml src/state.ml src/intrp.ml src/cycles.ml src/bytecrawler.ml
	ocamlfind ocamlc -package $(PACKAGE) $(FLAGS) obytelib.cma  -o $@

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
	@rm -f *~ $(TARGETS) \
	tests/*.byte \
	tests/*.cmo \
	tests/*.cmi \
	tests/*.o \
	bin/* \
	*.cmo \
	*.cmi \
	*.byte

.PHONY: bytecrawler clean
