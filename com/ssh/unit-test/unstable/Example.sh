#!/bin/bash

# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e

echo "Run example tests with SML/NJ..."
if sml -h > /dev/null ; then 
    eb=../../extended-basis/unstable

    if echo ''                                                   \
        | sml -m example.cm                                      \
              $eb/public/export/{open-top-level.sml,infixes.sml} \
              example/*.sml ; then echo "Unexpected!" ; fi
fi

echo "Compile example tests with MLton and run them..."
if mlton  > /dev/null ; then
    mkdir -p generated

    echo "SML_COMPILER mlton
MLTON_LIB $(cd ../../../.. && pwd)" > generated/mlb-path-map

    mlton -mlb-path-map generated/mlb-path-map \
          -output generated/example            \
          example.mlb

    if generated/example ; then echo "Unexpected!" ; fi
fi
