# Copyright (C) 2006 Entain, Inc.
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.
#

NAME = mjs
MLTON = mlton -mlb-path-map mlb-path-map
FLAGS = \
	-const 'Exn.keepHistory true' \
	-verbose 1
LEXER = lex-internals.fun
EXE=$(NAME)

.PHONY: all
all: $(EXE)

$(EXE): $(shell $(MLTON) -stop f $(NAME).mlb)
	time $(MLTON) $(FLAGS) -output $(EXE) $(NAME).mlb

$(LEXER): javascript.lex
	rm -f $(LEXER)
	mllex javascript.lex
	mv javascript.lex.sml $(LEXER)
	chmod -w $(LEXER)

javascript.grm.sig javascript.grm.sml: javascript.grm
	rm -f javascript.grm.*
	mlyacc javascript.grm
	chmod -w javascript.grm.*
