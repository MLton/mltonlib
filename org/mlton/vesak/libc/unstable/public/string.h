/* Copyright (C) 2006 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

FUNCTION(memchr     , void *    , (const void *s, int c, size_t n))
FUNCTION(memcmp     , int       , (const void *s1, const void *s2, size_t n))
FUNCTION(memcpy     , void *    , (void * restrict s1, const void * restrict s2, size_t n))
FUNCTION(memmove    , void *    , (void *s1, const void *s2, size_t n))
FUNCTION(memset     , void *    , (void *s, int c, size_t n))
FUNCTION(strcat     , char *    , (char * restrict s1, const char * restrict s2))
FUNCTION(strchr     , char *    , (const char *s, int c))
FUNCTION(strcmp     , int       , (const char *s1, const char *s2))
FUNCTION(strcoll    , int       , (const char *s1, const char *s2))
FUNCTION(strcpy     , char *    , (char * restrict s1, const char * restrict s2))
FUNCTION(strcspn    , size_t    , (const char *s1, const char *s2))
FUNCTION(strerror   , char *    , (int errnum))
FUNCTION(strlen     , size_t    , (const char *s))
FUNCTION(strncat    , char *    , (char * restrict s1, const char * restrict s2, size_t n))
FUNCTION(strncmp    , int       , (const char *s1, const char *s2, size_t n))
FUNCTION(strncpy    , char *    , (char * restrict s1, const char * restrict s2, size_t n))
FUNCTION(strpbrk    , char *    , (const char *s1, const char *s2))
FUNCTION(strrchr    , char *    , (const char *s, int c))
FUNCTION(strspn     , size_t    , (const char *s1, const char *s2))
FUNCTION(strstr     , char *    , (const char *s1, const char *s2))
FUNCTION(strtok     , char *    , (char * restrict s1, const char * restrict s2))
FUNCTION(strxfrm    , size_t    , (char * restrict s1, const char * restrict s2, size_t n))
