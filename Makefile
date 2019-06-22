SRCS     := mlstack.ml state.ml intrp.ml cycles.ml bytecrawler.ml
OPAMDIR   := $(shell opam config var lib)
TARGET  := bin/bytecrawler
FLAGS    := -I +../obytelib -I src
OCAMLLIB := $(shell ocamlfind printconf stdlib)
OCAMLC   := ocamlc
PACKAGE := csv

CMOFILES = $(SRCS:%.ml=%.cmo)

all: $(TARGET)

$(TARGET) : $(addprefix src/,$(SRCS))
	ocamlfind ocamlc -linkpkg -package $(PACKAGE) $(FLAGS) obytelib.cma  $+ -o $@

%.cmo: %.ml %.cmi
	ocamlfind ocamlc -package $(PACKAGE) $(FLAGS) -c $*.ml

%.cmi: %.mli %.ml
	ocamlfind ocamlc -package $(PACKAGE) $(FLAGS)  -c $*.mli

%.cmo: %.ml
	ocamlfind ocamlc -package $(PACKAGE) $(FLAGS)  -c $*.ml

clean:
	rm -f .depend
	find . -name "*.cm*" -delete
	rm -f bin/*


.PHONY: bytecrawler clean

.depend: $(MLSOURCES) Makefile
	ocamldep -I src $(SRCS) > .depend

include .depend
