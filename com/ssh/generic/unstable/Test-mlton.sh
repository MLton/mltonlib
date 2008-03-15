#!/bin/bash

# Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

if ! which mlton ; then
    echo 'Skipping test with MLton as it does not seem to be installed.'
    exit 0
fi

mkdir -p generated

echo "SML_COMPILER mlton
MLTON_LIB $(cd ../../../.. && pwd)
APPLICATION $(pwd)/test/app" > generated/mlb-path-map

time \
mlton -mlb-path-map generated/mlb-path-map         \
      -prefer-abs-paths true                       \
      -show-def-use generated/test.du              \
      -output generated/test                       \
      test.mlb

time \
generated/test
