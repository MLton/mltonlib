#!/bin/bash

# Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

eb=../../extended-basis/unstable

time \
echo '' | \
sml -m test.cm \
    $eb/public/export/{open-top-level.sml,infixes.sml}   \
    test/utils.fun                                       \
    with/reg-basis-exns.sml                              \
    with/data-rec-info.sml                               \
    with/some.sml                                        \
    with/pickle.sml                                      \
    with/seq.sml                                         \
    with/reduce.sml                                      \
    with/transform.sml                                   \
    with/close-pretty-with-extra.sml                     \
    $(find test/ -name '*.sml')
