#!/bin/bash

# Copyright (C) 2007-2008 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

if ! which poly ; then
    echo 'Skipping test with Poly/ML as it does not seem to be installed.'
    exit 0
fi

export APPLICATION="$(pwd)/test/app"

time \
echo 'use "../../../../org/mlton/vesak/use-lib/unstable/polyml.use" ;
      use "test.use" ;' | poly
