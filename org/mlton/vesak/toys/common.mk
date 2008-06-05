# Copyright (C) 2007 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

##########################################################################

target-arch := $(shell mlton -show path-map | awk '/^TARGET_ARCH/ {print $$2}')
target-os   := $(shell mlton -show path-map | awk '/^TARGET_OS/ {print $$2}')
target-id   := $(target-arch)-$(target-os)

gen-dir := generated/$(target-id)

mlb-path-map := $(gen-dir)/mlb-path-map

exe := $(gen-dir)/$(name)

##########################################################################

.PHONY : all clean help run

help :
	@echo "Targets:"
	@echo "    all      Builds the toy benchmark"
	@echo "    run      Runs the toy benchmark"
	@echo "    clean    Removes generated files"
	@echo "    help     You are reading it"

all : $(exe)

clean :
	rm -rf $(gen-dir)

run : $(exe)
	bash -c 'time $(exe) @MLton gc-summary -- $(args)'
	bash -c 'if test mlmon.out -nt $(exe) ; then mlprof $(exe) mlmon.out ; fi'

##########################################################################

$(mlb-path-map) : Makefile
	mkdir -p $(@D)
	echo 'MLTON_LIB $(shell cd $(root) && pwd)' > $@
	echo 'SML_COMPILER mlton' >> $@

$(exe) : $(name).mlb $(mlb-path-map)
	mlton -stop f -mlb-path-map $(mlb-path-map) $<            \
	  | sed $$'s#\r##g'                                       \
	  | awk 'BEGIN { srcs = "" ; printf "$@ :" }              \
	               { srcs = srcs $$1 ":\n" ; printf " " $$1 } \
	           END { printf "\n" srcs }'                      \
	  > $@.dep
	mlton -mlb-path-map $(mlb-path-map)                  \
	      -prefer-abs-paths true                         \
	      -show-def-use $@.du                            \
	      -link-opt '$(link-opts)'                       \
	      -output $@                                     \
	      $(mlton-opts)                                  \
	      $<
	strip $@

##########################################################################

include $(wildcard $(gen-dir)/*.dep)
