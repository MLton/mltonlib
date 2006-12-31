/* Copyright (C) 2006 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

ABSTRACT_STRUCT(tm)

CONSTANT(CLOCKS_PER_SEC     , clock_t)

FUNCTION(asctime    , char *        , (const struct tm *timeptr))
FUNCTION(clock      , clock_t       , (void))
FUNCTION(ctime      , char *        , (const time_t *timer))
FUNCTION(difftime   , double        , (time_t time1, time_t time0))
FUNCTION(gmtime     , struct tm *   , (const time_t *timer))
FUNCTION(localtime  , struct tm *   , (const time_t *timer))
FUNCTION(mktime     , time_t        , (struct tm *timeptr))
FUNCTION(strftime   , size_t        , (char * restrict s, size_t maxsize, const char * restrict format, const struct tm * restrict timeptr))
FUNCTION(time       , time_t        , (time_t *timer))

#if 0
ABSTRACT_STRUCT(tmx)

CONSTANT(_LOCALTIME         , int)
CONSTANT(_NO_LEAP_SECONDS   , int)

FUNCTION(mkxtime    , time_t        , (struct tmx *timeptr))
FUNCTION(strfxtime  , size_t        , (char * restrict s, size_t maxsize, const char * restrict format, const struct tmx * restrict timeptr))
FUNCTION(zonetime   , struct tmx *  , (const time_t *timer))
#endif
