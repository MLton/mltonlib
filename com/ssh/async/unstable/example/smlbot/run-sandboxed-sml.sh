#!/bin/bash

# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e

# Limit memory usage
maxMem=10000000
ulimit -v $maxMem

#maxTime=30
#ulimit -t $maxTime

# Run the code from stdin

if test -d .hamlet-succ ; then
    # Using HaMLet-S with modified Basis
    exec nice -n 19 .hamlet-succ/hamlet 2>&1
else
    # Make sandbox-prefix if necessary
    if ! test -e .sandbox-prefix.sml ; then
        ./make-sandbox-prefix.sh
    fi

    # Using sml/nj with the sandbox prefix
    exec nice -n 19 sml .sandbox-prefix.sml 2>&1
fi
