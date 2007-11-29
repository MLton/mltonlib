/* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#include "util.h"

int
SML_SDL_FillRect(SDL_Surface *d, int x, int y, unsigned w, unsigned h,
                 Uint32 c) {
  SDL_Rect dr = {x, y, w, h};
  return SDL_FillRect(d, &dr, c);
}

int
SML_SDL_BlitRect(SDL_Surface *s, int sx, int sy, unsigned sw, unsigned sh,
                 SDL_Surface *d, int dx, int dy, unsigned dw, unsigned dh) {
  SDL_Rect sr = {sx, sy, sw, sh}, dr = {dx, dy, dw, dh};
  return SDL_BlitSurface(s, &sr, d, &dr);
}
