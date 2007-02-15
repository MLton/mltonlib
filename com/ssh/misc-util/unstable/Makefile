# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

##########################################################################

.PHONY : check clean help

help :
	@echo "Targets:"
	@echo "    check    Compiles (when necessary) and runs (always) the tests"
	@echo "    clean    Removes generated files"
	@echo "    help     You are reading it"

clean :
	rm -rf mlb-path-map test $(wildcard *.dep)

check : test
	./test

##########################################################################

mlb-path-map : Makefile
	echo 'MLTON_LIB $(shell cd ../../../.. && pwd)' > $@
	echo 'SML_COMPILER mlton' >> $@

test : test.mlb mlb-path-map
	mlton -stop f -mlb-path-map mlb-path-map $<        \
	  | sed $$'s#\r##g'                                \
	  | awk 'BEGIN    { line = "$@ :" }                \
	                  { print line " \\" ; line=$$1 }  \
	         END      { print line }'                  \
	  > $@.dep
	mlton -mlb-path-map mlb-path-map     \
	      -prefer-abs-paths true         \
	      -show-def-use $@.def-use       \
	      -const 'Exn.keepHistory true'  \
	      -link-opt -ldl                 \
	      -output $@                     \
	      $<

##########################################################################

include $(wildcard *.dep)
