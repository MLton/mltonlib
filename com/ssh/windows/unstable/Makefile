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

constants-sml := $(gen-dir)/constants.sml
constants-cpp := detail/constants.sml.cpp

##########################################################################

.PHONY : all clean help check

help :
	@echo "Targets:"
	@echo "    all      Builds the static link library and NLFFI files"
	@echo "    check    Type check the SML code (does not check C code)"
	@echo "    clean    Removes generated files"
	@echo "    help     Prints this message"

all : $(lib-file) $(nlffi-mlb) $(constants-sml)

clean :
	rm -rf $(bin-dir) $(gen-dir) $(lib-file)

check : $(nlffi-mlb) $(mlb-path-map) $(constants-sml)
	mlton -stop tc                      \
	      -mlb-path-map $(mlb-path-map) \
	      -prefer-abs-paths true        \
	      -show-def-use $(def-use-file) \
	      lib.mlb

##########################################################################

$(constants-sml) : $(constants-cpp) $(ffi-h-files) Makefile
	mkdir -p $(@D)
	cat $< | cpp -E -P                                 \
	       | awk '/(* begin *)/, /(* end *)/'          \
	       | sed -e 's#< *(\?#<#g' -e 's#)\? *>#>#g'   \
	             -e 's#<(.*)#<#g'                      \
	             -e 's#\([0-9]\)[lL]>#\1>#g'           \
	             -e 's#<0[xX]#<x#g'                    \
	             -e 's#<#<0w#g'                        \
	             -e 's#<\(.*\)>#\1#g'                  \
	       > $@

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
