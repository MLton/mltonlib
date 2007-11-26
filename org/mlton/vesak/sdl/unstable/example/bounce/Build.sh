# Copyright (C) 2007 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

export MLTON_LIB="$(cd ../../../../../../../ && pwd)"

mkdir -p generated

time \
mlton -mlb-path-var "SML_COMPILER mlton"    \
      -mlb-path-var "MLTON_LIB $MLTON_LIB"  \
      -prefer-abs-paths true                \
      -show-def-use generated/bounce.du     \
      -output generated/bounce              \
      -link-opt '-ldl'                      \
      -link-opt '-lSDL'                     \
      bounce.mlb
