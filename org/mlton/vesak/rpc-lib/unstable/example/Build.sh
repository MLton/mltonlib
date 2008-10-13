#!/bin/bash

# Copyright (C) 2008 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

mkdir -p generated

function Compile {
    mlton -mlb-path-var "MLTON_LIB $(cd ../../../../../.. && pwd)" \
          -mlb-path-var "SML_COMPILER mlton"                       \
          -mlb-path-var "APPLICATION $(pwd)/app"                   \
          -prefer-abs-paths true                                   \
          -show-def-use "generated/$1.du"                          \
          -output "generated/$1"                                   \
          "$1.mlb"
    strip "generated/$1"
    ls -l "generated/$1"
}

Compile server
Compile client
