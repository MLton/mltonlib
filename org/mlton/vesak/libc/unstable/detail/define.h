/* Copyright (C) 2006 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#define STATIC_ASSERT(c)                        \
extern int static_assert[(c) ? 1 : -1]

#define ASSERT_EXISTS(name, type)                                       \
extern type* assert_exists_##name;                                      \
extern int assert_exists[1+0*sizeof(assert_exists_##name = &name)];

#define CONSTANT(name, type)                    \
STATIC_ASSERT(sizeof(type) == sizeof(name));    \
const type name##_ = name;

#define FUNCTION(name, result, args)            \
typedef result name##_type args;                \
ASSERT_EXISTS(name, name##_type)

#define PSEUDO_CONSTANT(name, type)             \
STATIC_ASSERT(sizeof(type) == sizeof(name));    \
type name##_get(void) {return name;}

#define PSEUDO_VARIABLE(name, type)             \
STATIC_ASSERT(sizeof(type) == sizeof(name));    \
type name##_get(void) {return name;}            \
void name##_set(type name##_) {name = name##_;}

#define ABSTRACT_STRUCT(name)

#define ABSTRACT_TYPE(name)
