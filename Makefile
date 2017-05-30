SRCS     := $(wildcard *.ml *.mli)
TARGETS  := bytecrawler
FLAGS    := -I +../obytelib

all: $(TARGETS)

bytecrawler: bytecrawler.ml
	ocamlc $(FLAGS) obytelib.cma bytecrawler.ml -o bytecrawler

pervasives.cmo : tests/pervasive.ml
	cd tests && ocamlc -c -nopervasives pervasive.ml

test : bytecrawler tests/test.ml pervasives.cmo
	cd tests && ocamlc -nopervasives test.ml -o test.byte
	cd tests && ocamlclean test.byte -o test2.byte
	cd tests && ../bytecrawler test2.byte

clean:
	@rm -f *~ $(TARGETS)
	@rm tests/test2.byte
	@rm tests/test.byte
	@rm tests/*.cmo
	@rm tests/*.cmi

.PHONY: bytecrawler clean
