# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

##########################################################################

target-arch := $(shell mlton -show path-map | awk '/^TARGET_ARCH/ {print $$2}')
target-os   := $(shell mlton -show path-map | awk '/^TARGET_OS/ {print $$2}')
target-id   := $(target-arch)-$(target-os)

bin-dir := .bin/$(target-id)
gen-dir := generated/$(target-id)

mlb-path-map := $(gen-dir)/mlb-path-map

ffi-h-files := $(wildcard detail/ffi/*.h)

nlffi-mlb := $(gen-dir)/nlffi/lib.mlb

lib-dir := detail/lib
lib-c-files := $(wildcard detail/lib/*.c)
lib-o-files := $(patsubst $(lib-dir)/%.c,$(bin-dir)/%.o,$(lib-c-files))
lib-h-files := $(wildcard $(lib-dir)/*.h)

lib-file := libwin-$(target-id).lib

def-use-file := $(gen-dir)/lib.du

##########################################################################

.PHONY : all clean help check

help :
	@echo "Targets:"
	@echo "    all      Builds the static link library and NLFFI files"
	@echo "    check    Type check the SML code (does not check C code)"
	@echo "    clean    Removes generated files"
	@echo "    help     Prints this message"

all : $(lib-file) $(nlffi-mlb)

clean :
	rm -rf $(bin-dir) $(gen-dir) $(lib-file) $(def-use-file)

check : $(nlffi-mlb) $(mlb-path-map)
	mlton -stop tc                      \
	      -mlb-path-map $(mlb-path-map) \
	      -prefer-abs-paths true        \
	      -show-def-use $(def-use-file) \
	      lib.mlb

##########################################################################

$(mlb-path-map) : Makefile
	echo 'MLTON_LIB $(shell cd ../../../.. && pwd)' > $@
	echo 'SML_COMPILER mlton' >> $@

$(lib-file) : $(lib-o-files)
	ar cr $@ $^

$(nlffi-mlb) : $(ffi-h-files)
	mkdir -p $(@D)
	mlnlffigen -dir $(@D)       \
	           -mlbfile $(@F)   \
	           -linkage static  \
	           $^

$(bin-dir)/%.o : $(lib-dir)/%.c $(lib-h-files) $(ffi-h-files)
	mkdir -p $(@D)
	gcc -Wall     \
	    -Werror   \
	    -pedantic \
	    -std=c99  \
	    -c        \
	    -o $@     \
	    $<

##########################################################################
