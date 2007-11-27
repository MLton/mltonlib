# Copyright (C) 2007 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

##########################################################################
# MLton Platform

arch="$(mlton -show path-map | awk '/^TARGET_ARCH/ {print $2}')"
os="$(mlton -show path-map | awk '/^TARGET_OS/ {print $2}')"
target="$arch-$os"

##########################################################################
# Build Program

export MLTON_LIB="$(cd ../../../../../../../ && pwd)"

mkdir -p generated

time \
mlton -mlb-path-var "SML_COMPILER mlton"    \
      -mlb-path-var "MLTON_LIB $MLTON_LIB"  \
      -prefer-abs-paths true                \
      -show-def-use generated/bounce.du     \
      -output generated/bounce              \
      -link-opt "-ldl"                      \
      -link-opt "-lSDL"                     \
      -link-opt "-L../.."                   \
      -link-opt "-lsdl-$target"             \
      bounce.mlb
