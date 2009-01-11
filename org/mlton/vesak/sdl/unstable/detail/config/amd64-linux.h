/* Copyright (C) 2007-2008 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

/* SDL */

#define _BITS_PTHREADTYPES_H 1
#define _SDL_endian_h

#include <stddef.h>
#include <stdint.h>
#include <sys/types.h>

#include <SDL/SDL_config.h>

#undef STDC_HEADERS

#undef HAVE_CTYPE_H
#undef HAVE_ICONV_H
#undef HAVE_INTTYPES_H
#undef HAVE_MALLOC_H
#undef HAVE_STDARG_H
#undef HAVE_STDDEF_H
#undef HAVE_STDINT_H
#undef HAVE_STDIO_H
#undef HAVE_STDLIB_H
#undef HAVE_STRINGS_H
#undef HAVE_STRING_H
#undef HAVE_SYS_TYPES_H

#undef HAVE_ICONV

/* GL */

#define GL_GLEXT_PROTOTYPES
