#!/bin/bash

# Copyright (C) 2007 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

time \
echo 'use "../../../../org/mlton/vesak/use-lib/unstable/polyml.use" ;
      use "test.use" ;' | poly
