# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

##########################################################################

target-arch := $(shell mlton -show path-map | awk '/^TARGET_ARCH/ {print $$2}')
target-os   := $(shell mlton -show path-map | awk '/^TARGET_OS/ {print $$2}')
target-id   := $(target-arch)-$(target-os)

gen-dir := generated/$(target-id)

ffi-h-files := $(wildcard *.h)

nlffi-mlb := $(gen-dir)/nlffi/lib.mlb

##########################################################################

.PHONY : all check clean help

help :
	@echo "Targets:"
	@echo "    all      Builds the NLFFI files"
	@echo "    check    Compiles (when necessary) and runs (always) the tests"
	@echo "    clean    Removes generated files"
	@echo "    help     You are reading it"

all : $(nlffi-mlb)

clean :
	rm -rf mlb-path-map test $(wildcard *.dep) $(gen-dir)

check : test
	./test

##########################################################################

$(nlffi-mlb) : $(ffi-h-files)
	mkdir -p $(@D)
	mlnlffigen -dir $(@D)       \
	           -mlbfile $(@F)   \
	           -linkage static  \
	           $^

mlb-path-map : Makefile
	echo 'MLTON_LIB $(shell cd ../../../.. && pwd)' > $@
	echo 'SML_COMPILER mlton' >> $@

test : test.mlb mlb-path-map $(nlffi-mlb)
	mlton -stop f -mlb-path-map mlb-path-map $<          \
	  | sed $$'s#\r##g'                                  \
	  | awk 'BEGIN { printf "$@ :" } { printf " " $$1 }' \
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
