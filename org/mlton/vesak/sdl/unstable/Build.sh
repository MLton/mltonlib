# Copyright (C) 2007 Vesa Karvonen
#
# This code is released under the MLton license, a BSD-style license.
# See the LICENSE file or http://mlton.org/License for details.

set -e
set -x

##########################################################################
# MLton Platform

arch="$(mlton -show path-map | awk '/^TARGET_ARCH/ {print $2}')"
os="$(mlton -show path-map | awk '/^TARGET_OS/ {print $2}')"
target="$arch-$os"

##########################################################################
# Build Library

cd detail/lib

mkdir -p .$target

for src in *.c ; do
    gcc -O3 -Wall -c -o .$target/$src.o $src
done

cd ../..

ar cr libsdl-$target.a detail/lib/.$target/*.o
