#!/bin/bash

# Copyright (C) 2008 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e

export APPLICATION="$(pwd)/test/app"
export MLTON_LIB="$(cd ../../../.. && pwd)"

if which poly > /dev/null ; then
    pushd $MLTON_LIB/org/mlton/vesak/use-lib/unstable
    ./Make.sh
    popd
    echo "use \"$MLTON_LIB/org/mlton/vesak/use-lib/unstable/polyml.use\" ;
          use \"test.use\" ;" | poly
fi

if which mlton > /dev/null ; then
    mkdir -p generated

    echo "APPLICATION $APPLICATION
MLTON_LIB $MLTON_LIB
SML_COMPILER mlton" > generated/mlb-path-map

    mlton -mlb-path-map generated/mlb-path-map \
          -output generated/test               \
          test.mlb

    generated/test
fi
