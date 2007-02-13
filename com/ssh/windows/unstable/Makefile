# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

target-arch := $(shell mlton -show path-map | awk '/^TARGET_ARCH/ {print $$2}')
target-os   := $(shell mlton -show path-map | awk '/^TARGET_OS/ {print $$2}')
target-id   := $(target-arch)-$(target-os)

bin-dir := .bin/$(target-id)
gen-dir := generated/$(target-id)

ffi-h-files := $(wildcard detail/ffi/*.h)

lib-dir := detail/lib
lib-c-files := $(wildcard detail/lib/*.c)
lib-o-files := $(patsubst $(lib-dir)/%.c,$(bin-dir)/%.o,$(lib-c-files))
lib-h-files := $(wildcard $(lib-dir)/*.h)

lib-file := libwin-$(target-id).lib

at :=

.PHONY : all clean help

help :
	@echo "Targets:"
	@echo "    all      Builds the static link library and NLFFI files"
	@echo "    clean    Removes generated files"
	@echo "    help     Prints this message"

all : $(lib-file) $(gen-dir)/nlffi/lib.mlb

clean :
	$(at)rm -rf $(bin-dir) $(gen-dir)/nlffi $(lib-file)

$(lib-file) : $(lib-o-files)
	$(at)ar cr $@ $^

$(gen-dir)/nlffi/lib.mlb : $(ffi-h-files)
	$(at)mkdir -p $(@D)
	$(at)mlnlffigen -dir $(@D)       \
	                -mlbfile $(@F)   \
	                -linkage static  \
	                $^

$(bin-dir)/%.o : $(lib-dir)/%.c $(lib-h-files) $(ffi-h-files)
	$(at)mkdir -p $(@D)
	$(at)gcc -Wall     \
	         -Werror   \
	         -pedantic \
	         -std=c99  \
	         -c        \
	         -o $@     \
	         $<
