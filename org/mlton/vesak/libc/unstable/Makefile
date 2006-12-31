# Copyright (C) 2006 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

target_arch := $(shell mlton -show path-map | awk '/^TARGET_ARCH/ {print $$2}')
target_os   := $(shell mlton -show path-map | awk '/^TARGET_OS/ {print $$2}')
target_id   := $(target_arch)-$(target_os)

lib_file    := libc-nlffi-$(target_id).a

config_h    := public/config/$(target_id).h
cc_opts     := -Wall -Werror -pedantic -std=c99

bin_dir     := .bin/$(target_id)
gen_dir     := generated/$(target_id)

dummy_c     := generated/dummy.c

std_h_files := $(wildcard public/*.h)
all_h_files := $(wildcard detail/*.h) $(config_h) $(std_h_files)

.PHONY : all clean help

help :
	@echo "Targets:"
	@echo "    all      Builds the library ($(lib_file)) and NLFFI files"
	@echo "    clean    Removes generated files"
	@echo "    help     Prints this message"

all : $(gen_dir)/config/lib.mlb $(gen_dir)/std/lib.mlb $(lib_file)

clean :
	rm -rf           \
	   $(bin_dir)    \
	   $(config_h)   \
	   $(dummy_c)    \
	   $(gen_dir)    \
	   $(lib_file)

$(config_h) : detail/config-gen.c
	@mkdir -p $(bin_dir) $(@D)
	gcc $(cc_opts) -o $(bin_dir)/config-gen $<
	$(bin_dir)/config-gen > $@

$(gen_dir)/config/lib.mlb : $(config_h) $(gen_dir)
	mlnlffigen -dir $(@D)                          \
	           -mlbfile $(@F)                      \
	           -linkage static $(config_h)

$(gen_dir)/std/lib.mlb : $(all_h_files) $(gen_dir)
	mlnlffigen -dir $(@D)                          \
	           -mlbfile $(@F)                      \
	           -cppopt '-include detail/declare.h' \
	           -cppopt '-include $(config_h)'      \
	           -linkage static $(std_h_files)

$(lib_file) : $(all_h_files) $(dummy_c)
	gcc $(cc_opts)                                         \
	    -c                                                 \
	    -o $(bin_dir)/libc-nlffi.o                         \
	    $(patsubst public/%.h,-include %.h,$(std_h_files)) \
	    -include detail/define.h                           \
	    $(patsubst %.h,-include %.h,$(std_h_files))        \
	    $(dummy_c)
	ar cr $@ $(bin_dir)/libc-nlffi.o

$(dummy_c) :
	@touch $@

$(gen_dir) :
	@mkdir -p $@
