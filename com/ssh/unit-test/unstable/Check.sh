#!/bin/bash

# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

name=lib-with-default

set -e
set -x

mkdir -p generated

echo "SML_COMPILER mlton
MLTON_LIB $(cd ../../../.. && pwd)" > generated/mlb-path-map

mlton -mlb-path-map generated/mlb-path-map         \
      -prefer-abs-paths true                       \
      -stop tc                                     \
      -show-def-use generated/$name.du             \
      -show-basis generated/$name.basis            \
      $name.mlb
