#!/bin/bash

# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e

# Limit resource usage
maxMem=1000000
maxTime=30
ulimit -v $maxMem -t $maxTime

# Make sandbox-prefix if necessary
if ! test -e .sandbox-prefix.sml ; then
    ./make-sandbox-prefix.sh
fi

# Run the code from stdin
exec nice -n 19 sml .sandbox-prefix.sml 2>&1
