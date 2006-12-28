/* Copyright (C) 2006 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#include <stdio.h>
#include "util.h"

CONSTANT(int, EOF)
CONSTANT(int, FILENAME_MAX)
CONSTANT(int, FOPEN_MAX)
CONSTANT(int, L_tmpnam)
CONSTANT(int, SEEK_CUR)
CONSTANT(int, SEEK_END)
CONSTANT(int, SEEK_SET)
CONSTANT(int, TMP_MAX)
CONSTANT(int, _IOFBF)
CONSTANT(int, _IOLBF)
CONSTANT(int, _IONBF)
CONSTANT(size_t, BUFSIZ)

CONST_FN(FILE *, stderr)
CONST_FN(FILE *, stdin)
CONST_FN(FILE *, stdout)
