/* Copyright (C) 2006 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

/* This simple C program is used to generate a configuration header that
 * defines typedefs for standard types and typedefs.  For each type[def],
 * this program simply chooses a type that has the correct size and kind
 * (signed or unsigned integer).  The reason for doing this is that system
 * and/or compiler headers may (and often do) contain non-standard C and
 * choke mlnlffigen.  Also, mlnlffigen can't export macros, so we need to
 * do something about them.  So, instead of #including system headers, we
 * make our own headers.
 */

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/************************************************************************/

static void
fail(const char *format, ...) {
  va_list args;
  va_start(args, format);
  fprintf(stderr, "Error: ");
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
  va_end(args);
  exit(EXIT_FAILURE);
}

#define fail(...) do fail(__VA_ARGS__); while (true)

/************************************************************************/

typedef enum integer_kind {
  signed_integer,
  unsigned_integer
} integer_kind;

static const char *
choose_integer_type(const size_t size, const integer_kind kind) {
  switch (kind) {
  case signed_integer:
    if (sizeof(signed char) == size) return "signed char";
    if (sizeof(short) == size) return "short";
    if (sizeof(int) == size) return "int";
    if (sizeof(long) == size) return "long";
    if (sizeof(long long) == size) return "long long";
    break;
  case unsigned_integer:
    if (sizeof(unsigned char) == size) return "unsigned char";
    if (sizeof(unsigned short) == size) return "unsigned short";
    if (sizeof(unsigned int) == size) return "unsigned int";
    if (sizeof(unsigned long) == size) return "unsigned long";
    if (sizeof(unsigned long long) == size) return "unsigned long long";
    break;
  }
  fail("Couldn't find a %s type of %zd bytes.",
       signed_integer == kind ? "signed" : "unsigned",
       size);
}

/************************************************************************/

static void
print_header(void) {
  printf("#ifndef CONFIG_H\n"
         "#define CONFIG_H\n"
         "\n"
         "/* THIS FILE IS GENERATED.  DO NOT EDIT! */\n");
}

static void
print_separator(const char *text) {
  printf("\n/** <%s> *", text);
  for (size_t width = strlen(text) + strlen("/** <> *"); width < 72; ++width)
    printf("*");
  printf("*/\n\n");
}

static void
print_integer_type(const size_t size,
                   const integer_kind kind,
                   const char *name) {
  printf("typedef %s %s;\n", choose_integer_type(size, kind), name);
}

static void
print_footer(void) {
  printf("\n"
         "#endif\n");
}

/************************************************************************/

#define INTEGER_TYPE(type)                      \
print_integer_type(sizeof(type),                \
                   (type)-1 < (type)0           \
                   ? signed_integer             \
                   : unsigned_integer,          \
                   #type)

int
main(int argc, char *argv[]) {
  print_header();

  print_separator("stdbool.h");

  INTEGER_TYPE(bool);

  print_separator("stddef.h");

  INTEGER_TYPE(size_t);
  INTEGER_TYPE(ptrdiff_t);

  print_separator("stdint.h");

  INTEGER_TYPE(int8_t);
  INTEGER_TYPE(int16_t);
  INTEGER_TYPE(int32_t);
  INTEGER_TYPE(int64_t);
  INTEGER_TYPE(uint8_t);
  INTEGER_TYPE(uint16_t);
  INTEGER_TYPE(uint32_t);
  INTEGER_TYPE(uint64_t);
  INTEGER_TYPE(int_least8_t);
  INTEGER_TYPE(int_least16_t);
  INTEGER_TYPE(int_least32_t);
  INTEGER_TYPE(int_least64_t);
  INTEGER_TYPE(uint_least8_t);
  INTEGER_TYPE(uint_least16_t);
  INTEGER_TYPE(uint_least32_t);
  INTEGER_TYPE(uint_least64_t);
  INTEGER_TYPE(int_fast8_t);
  INTEGER_TYPE(int_fast16_t);
  INTEGER_TYPE(int_fast32_t);
  INTEGER_TYPE(int_fast64_t);
  INTEGER_TYPE(uint_fast8_t);
  INTEGER_TYPE(uint_fast16_t);
  INTEGER_TYPE(uint_fast32_t);
  INTEGER_TYPE(uint_fast64_t);
  INTEGER_TYPE(intptr_t);
  INTEGER_TYPE(uintptr_t);
  INTEGER_TYPE(intmax_t);
  INTEGER_TYPE(uintmax_t);

  print_separator("wchar.h");

  INTEGER_TYPE(wchar_t);

  print_footer();

  return EXIT_SUCCESS;
}
