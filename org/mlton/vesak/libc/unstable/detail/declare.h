/* Copyright (C) 2006 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#define CONSTANT(name, type)                    \
extern const type name##_;

#define FUNCTION(name, result, args)            \
result name args;

#define PSEUDO_CONSTANT(name, type)             \
type name##_get(void);

#define PSEUDO_VARIABLE(name, type)             \
type name##_get(void);                          \
void name##_set(type name##_);

#define ABSTRACT_STRUCT(name)                   \
struct name;

#define ABSTRACT_TYPE(name)                     \
typedef struct name name;

/* mlnlffigen can't parse restrict */
#define restrict
