# Copyright (C) 2006 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

target_arch := $(shell mlton -show path-map | awk '/^TARGET_ARCH/ {print $$2}')
target_os   := $(shell mlton -show path-map | awk '/^TARGET_OS/ {print $$2}')
target_id   := $(target_arch)-$(target_os)

lib_file    := libc-nlffi-$(target_id).a
mlb_file    := libc.mlb

config_h    := public/config-$(target_id).h
cc_opts     := -Wall -std=c99

bin_dir     := .bin/$(target_id)
gen_dir     := generated/$(target_id)

c_dir       := detail/c
c_files     := $(wildcard $(c_dir)/*.c)
o_files     := $(patsubst $(c_dir)/%.c,$(bin_dir)/%.o,$(c_files))

.PHONY : all clean help

help :
	@echo "Targets:"
	@echo "    all      Builds the library ($(lib_file)) and NLFFI files"
	@echo "    clean    Removes generated files"
	@echo "    help     Prints this message"

all : $(gen_dir)/$(mlb_file) $(lib_file)

clean :
	rm -rf $(gen_dir) $(bin_dir) $(config_h) $(lib_file)

$(config_h) : detail/config-gen.c
	mkdir -p $(bin_dir)
	gcc $(cc_opts) -o $(bin_dir)/config-gen $<
	$(bin_dir)/config-gen > $@

$(gen_dir)/$(mlb_file) : $(config_h) $(wildcard public/*.h)
	mkdir -p $(gen_dir)
	mlnlffigen -dir $(gen_dir)                                         \
	           -mlbfile $(mlb_file)                                    \
	           -cppopt '-U$(target_arch) -DTARGET_ARCH=$(target_arch)' \
	           -cppopt '-U$(target_os) -DTARGET_OS=$(target_os)'       \
	           -linkage static $^

$(lib_file) : $(o_files)
	ar cr $@ $^

$(bin_dir)/%.o : $(c_dir)/%.c
	mkdir -p $(bin_dir)
	gcc $(cc_opts) -c -o $@ $<
