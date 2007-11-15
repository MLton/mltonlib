#!/bin/bash

# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e

if which poly > /dev/null ; then
    echo "Run example tests with Poly/ML..."

    pushd ../../../../org/mlton/vesak/use-lib/unstable
    ./Make.sh
    popd
    if echo 'use "../../../../org/mlton/vesak/use-lib/unstable/polyml.use" ;
             use "example.use" ;' | poly ; then echo "Unexpected!" ; fi
fi

if which sml > /dev/null ; then 
    echo "Run example tests with SML/NJ..."

    eb=../../extended-basis/unstable

    if echo ''                                                   \
        | sml -m example.cm                                      \
              $eb/public/export/{open-top-level.sml,infixes.sml} \
              example/*.sml ; then echo "Unexpected!" ; fi
fi

if which mlton > /dev/null ; then
    echo "Compile example tests with MLton and run them..."

    mkdir -p generated

    echo "SML_COMPILER mlton
MLTON_LIB $(cd ../../../.. && pwd)" > generated/mlb-path-map

    mlton -mlb-path-map generated/mlb-path-map \
          -output generated/example            \
          -ieee-fp true                        \
          example.mlb

    if generated/example ; then echo "Unexpected!" ; fi
fi
