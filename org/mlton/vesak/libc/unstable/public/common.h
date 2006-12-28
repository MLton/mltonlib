#ifndef COMMON_H
#define COMMON_H

/* Copyright (C) 2006 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#if !defined(TARGET_ARCH) || !defined(TARGET_OS)
#error TARGET_ARCH and TARGET_OS must be defined
#endif

#define STRINGIFY(x) STRINGIFY_DELAY(x)
#define STRINGIFY_DELAY(x) #x

#include STRINGIFY(config-TARGET_ARCH-TARGET_OS.h)

#define restrict /* mlnlffigen can't parse restrict ATM */

#endif
