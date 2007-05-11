# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

##########################################################################

target-arch := $(shell mlton -show path-map | awk '/^TARGET_ARCH/ {print $$2}')
target-os   := $(shell mlton -show path-map | awk '/^TARGET_OS/ {print $$2}')
target-id   := $(target-arch)-$(target-os)

gen-dir := generated/$(target-id)

mlb-path-map := $(gen-dir)/mlb-path-map

smlbot-exe := $(gen-dir)/smlbot

ifeq ($(target-os),mingw)
link-opt :=
else
link-opt := -link-opt -ldl
endif

##########################################################################

.PHONY : all clean help

help :
	@echo "Targets:"
	@echo "    all      Builds the SML bot"
	@echo "    clean    Removes generated files"
	@echo "    help     You are reading it"

all : $(smlbot-exe)

clean :
	rm -rf $(gen-dir)

##########################################################################

$(mlb-path-map) : Makefile
	mkdir -p $(@D)
	echo 'MLTON_LIB $(shell cd ../../../../../.. && pwd)' > $@
	echo 'SML_COMPILER mlton' >> $@

$(smlbot-exe) : smlbot.mlb $(mlb-path-map)
	mlton -stop f -mlb-path-map $(mlb-path-map) $<            \
	  | sed $$'s#\r##g'                                       \
	  | awk 'BEGIN { srcs = "" ; printf "$@ :" }              \
	               { srcs = srcs $$1 ":\n" ; printf " " $$1 } \
	           END { printf "\n" srcs }'                      \
	  > $@.dep
	mlton -mlb-path-map $(mlb-path-map)                  \
	      -prefer-abs-paths true                         \
	      -show-def-use $@.du                            \
	      -const 'Exn.keepHistory true'                  \
	      $(link-opt)                                    \
	      -output $@                                     \
	      $<

##########################################################################

include $(wildcard $(gen-dir)/*.dep)
