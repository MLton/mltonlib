#ifndef SML_SDL_LIB_UTIL_H
#define SML_SDL_LIB_UTIL_H

/* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#include <SDL/SDL_video.h>

int SML_SDL_FillRect(SDL_Surface *dst, int x, int y, unsigned w, unsigned h,
                     Uint32 color);

#endif
