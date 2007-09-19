#!/bin/bash

# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

mkdir -p generated

echo "SML_COMPILER mlton
MLTON_LIB $(cd ../../../.. && pwd)" > generated/mlb-path-map

time \
mlton -mlb-path-map generated/mlb-path-map         \
      -prefer-abs-paths true                       \
      -show-def-use generated/test.du              \
      -output generated/test                       \
      -const 'Exn.keepHistory true'                \
      -type-check true                             \
      -verbose 2                                   \
      test.mlb

time \
generated/test
