SRCS     := $(wildcard *.ml *.mli)
TARGETS  := bytecrawler
FLAGS    := -I +../obytelib

all: $(TARGETS)

bytecrawler: bytecrawler.ml
	ocamlc $(FLAGS) obytelib.cma bytecrawler.ml -o bytecrawler

test : bytecrawler test.ml
	ocamlc test.ml -o test.byte
	ocamlclean test.byte -o test.byte
	./bytecrawler test.byte

clean:
	@rm -f *~ $(TARGETS)
	@rm test.byte
	@rm *.cmo
	@rm *.cmi

.PHONY: bytecrawler clean
