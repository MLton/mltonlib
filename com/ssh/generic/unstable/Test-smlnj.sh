#!/bin/bash

# Copyright (C) 2007-2008 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

if ! which sml ; then
    echo 'Skipping test with SML/NJ as it does not seem to be installed.'
    exit 0
fi

export CM_LOCAL_PATHCONFIG=generated/smlnj-pathconfig
echo "MLTON_LIB $(cd ../../../.. && pwd)
APPLICATION $(pwd)/test/app" > $CM_LOCAL_PATHCONFIG

eb=../../extended-basis/unstable

time \
echo '' | \
sml -m test.cm \
    $eb/public/export/{open-top-level.sml,infixes.sml}  \
    test/*.sml
