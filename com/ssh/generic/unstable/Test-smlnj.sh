#!/bin/bash

# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

if ! which sml ; then
    echo 'Skipping test with SML/NJ as it does not seem to be installed.'
    exit 0
fi

eb=../../extended-basis/unstable

time \
echo '' | \
sml -m test.cm \
    $eb/public/export/{open-top-level.sml,infixes.sml}  \
    $(find test/ -name '*.sml' -a -not -name 'generic.sml' | sort)
