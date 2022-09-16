# The main Makefile, fragments shared between Makefile and Makefile.nt
CAMLYACC=menhir
CAMLLEX=ocamllex

world: lexer.ml parser.mli parser.ml

parser.mli parser.ml: parser.mly
	$(CAMLYACC) -v --strict parser.mly

clean::
	rm -f lexer.ml parser.mli parser.ml parser.automaton parser.conflicts

# The lexer

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

default: world
