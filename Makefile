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
	cd tests && ocamlclean test.byte -o test.byte
	cd tests && ../bytecrawler test.byte

clean:
	@rm -f *~ $(TARGETS) \
	tests/*.byte \
	tests/*.cmo \
	tests/*.cmi \
	*.cmo \
	*.cmi \
	*.byte

.PHONY: bytecrawler clean
