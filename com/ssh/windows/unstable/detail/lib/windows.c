/* Copyright (C) 2007 SSH Communications Security, Helsinki, Finland
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

/*
 * Implementation of Windows utilities according to specification.
 */

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

/************************************************************************/

#define WIN_TYPEDEF(name, type) CHECK_COMPATIBLE_TYPES(type, name)

#define CHECK_COMPATIBLE_TYPES(type, name)                                      \
extern int typedef_fn_of_actual_type_##name(name);                              \
extern int typedef_fn_of_assumed_type_##name(type);                             \
extern name typedef_val_of_actual_type_##name;                                  \
extern type typedef_val_of_assumed_type_##name;                                 \
extern int typedef_chk_a_##name[sizeof(typedef_fn_of_assumed_type_##name        \
                                       (typedef_val_of_actual_type_##name))];   \
extern int typedef_chk_b_##name[sizeof(typedef_fn_of_actual_type_##name         \
                                       (typedef_val_of_assumed_type_##name))]

/************************************************************************/

#define WIN_CONST(name, type)                   \
typedef type type_of_##name;                    \
const type_of_##name win_##name = name

/************************************************************************/

#define WIN_FUNCTION(name, result, arity, args)                 \
result win_##name FORMALS##arity args                           \
{ UNLESS(IS_VOID(result))(return) name ACTUALS##arity args; }   \
typedef result (type_of_##name) args

#define UNLESS(c) CONCAT(UNLESS, c)
#define UNLESS0(x) x
#define UNLESS1(_)

#define CONCAT(a, b) CONCAT_1(a, b)
#define CONCAT_1(a, b) CONCAT_2(a, b)
#define CONCAT_2(a, b) a##b

#define APPLY(m, a) APPLY_1(m, a)
#define APPLY_1(m, a) APPLY_2(m, a)
#define APPLY_2(m, a) m a

#define FIRST(f, _) f

#define IS_VOID(s)                              \
APPLY(FIRST,                                    \
      (APPLY(CONCAT,                            \
             (IS_VOID_AUX,                      \
              APPLY(IS_VOID_AUX,                \
                    IS_VOID_##s)))))
#define IS_VOID_void ()
#define IS_VOID_AUX() 1
#define IS_VOID_AUXIS_VOID_AUX 0,!
#define IS_VOID_AUX1 1,!

#define FORMALS0(_) (void)
#define FORMALS1(A) (A a)
#define FORMALS2(A,B) (A a,B b)
#define FORMALS3(A,B,C) (A a,B b,C c)
#define FORMALS4(A,B,C,D) (A a,B b,C c,D d)
#define FORMALS5(A,B,C,D,E) (A a,B b,C c,D d,E e)
#define FORMALS6(A,B,C,D,E,F) (A a,B b,C c,D d,E e,F f)
#define FORMALS7(A,B,C,D,E,F,G) (A a,B b,C c,D d,E e,F f,G g)
#define FORMALS8(A,B,C,D,E,F,G,H) (A a,B b,C c,D d,E e,F f,G g,H h)
#define FORMALS9(A,B,C,D,E,F,G,H,I) (A a,B b,C c,D d,E e,F f,G g,H h,I i)

#define ACTUALS0(_) ()
#define ACTUALS1(A) (a)
#define ACTUALS2(A,B) (a,b)
#define ACTUALS3(A,B,C) (a,b,c)
#define ACTUALS4(A,B,C,D) (a,b,c,d)
#define ACTUALS5(A,B,C,D,E) (a,b,c,d,e)
#define ACTUALS6(A,B,C,D,E,F) (a,b,c,d,e,f)
#define ACTUALS7(A,B,C,D,E,F,G) (a,b,c,d,e,f,g)
#define ACTUALS8(A,B,C,D,E,F,G,H) (a,b,c,d,e,f,g,h)
#define ACTUALS9(A,B,C,D,E,F,G,H,I) (a,b,c,d,e,f,g,h,i)

/************************************************************************/

#include "../ffi/windows.h"

/************************************************************************/

LPTSTR win_FormatErrorLocalAlloc(DWORD error)
{
  LPTSTR msg = NULL;
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                FORMAT_MESSAGE_FROM_SYSTEM |
                FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, error, 0, (LPTSTR)&msg, 0, NULL);
  return msg;
}
