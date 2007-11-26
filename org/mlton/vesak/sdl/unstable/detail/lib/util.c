/* Copyright (C) 2007 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#include "util.h"

int SML_SDL_FillRect(SDL_Surface *dst,
                     int x,
                     int y,
                     unsigned w,
                     unsigned h,
                     Uint32 color) {
  SDL_Rect rect;
  rect.x = x;
  rect.y = y;
  rect.w = w;
  rect.h = h;
  return SDL_FillRect(dst, &rect, color);
}
