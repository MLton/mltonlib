/* Copyright (C) 2006 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#define CONSTANT(type, name) const type name##_ = name;
#define CONST_FN(type, name) type name##_(void) {return name;}
