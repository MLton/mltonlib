#ifndef SML_SDL_LIB_UTIL_H
#define SML_SDL_LIB_UTIL_H

/* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#include <SDL/SDL_video.h>

int
SML_SDL_FillRect(SDL_Surface *d, int x, int y, unsigned w, unsigned h,
                 Uint32 c);

int
SML_SDL_BlitRect(SDL_Surface *s, int sx, int sy, unsigned sw, unsigned sh,
                 SDL_Surface *d, int dx, int dy, unsigned dw, unsigned dh);

#endif
