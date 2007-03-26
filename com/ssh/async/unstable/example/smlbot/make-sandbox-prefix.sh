#!/bin/bash

# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e

# Create sandbox
echo 'val () = Control.Print.linewidth := 70
fun print _ = raise Fail "IO not allowed"
val use = print
structure Poison = struct val IO_not_allowed = () end' \
 > .sandbox-prefix.sml

echo ''                                                           | \
nice -n 19 sml show-bindings.sml                                  | \
grep    -e 'structure' -e 'functor'                               | \
grep -v -e 'structure _Core'                                      | \
eval grep -v `sed -e 's#^#-e "^#g' -e 's#$#$"#g' allowed-modules` | \
sed -e 's#structure\(.*\)$#structure\1 = Poison#g'                  \
    -e 's#functor\(.*\)$#functor\1 () = Poison#g'                   \
 >> .sandbox-prefix.sml
