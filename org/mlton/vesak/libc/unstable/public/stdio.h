#ifndef STDIO_H
#define STDIO_H

/* Copyright (C) 2006 Vesa Karvonen
 *
 * This code is released under the MLton license, a BSD-style license.
 * See the LICENSE file or http://mlton.org/License for details.
 */

#include "common.h"

typedef struct FILE FILE;
typedef struct fpos_t fpos_t;

extern const int EOF_;
extern const int FILENAME_MAX_;
extern const int FOPEN_MAX_;
extern const int L_tmpnam_;
extern const int SEEK_CUR_;
extern const int SEEK_END_;
extern const int SEEK_SET_;
extern const int TMP_MAX_;
extern const int _IOFBF_;
extern const int _IOLBF_;
extern const int _IONBF_;
extern const size_t BUFSIZ_;

FILE *stderr_(void);
FILE *stdin_(void);
FILE *stdout_(void);

int remove(const char *filename);
int rename(const char *old, const char *new);
FILE *tmpfile(void);
char *tmpnam(char *s);
int fclose(FILE *stream);
int fflush(FILE *stream);
FILE *fopen(const char * restrict filename, const char * restrict mode);
FILE *freopen(const char * restrict filename, const char * restrict mode,
FILE * restrict stream);
void setbuf(FILE * restrict stream, char * restrict buf);
int setvbuf(FILE * restrict stream, char * restrict buf, int mode, size_t size);
/* int fprintf(FILE * restrict stream, const char * restrict format, ...); */
/* int fscanf(FILE * restrict stream, const char * restrict format, ...); */
/* int printf(const char * restrict format, ...); */
/* int scanf(const char * restrict format, ...); */
/* int snprintf(char * restrict s, size_t n, const char * restrict format, ...); */
/* int sprintf(char * restrict s, const char * restrict format, ...); */
/* int sscanf(const char * restrict s, const char * restrict format, ...); */
/* int vfprintf(FILE * restrict stream, const char * restrict format, va_list arg); */
/* int vfscanf(FILE * restrict stream, const char * restrict format, va_list arg); */
/* int vprintf(const char * restrict format, va_list arg); */
/* int vscanf(const char * restrict format, va_list arg); */
/* int vsnprintf(char * restrict s, size_t n, const char * restrict format, va_list arg); */
/* int vsprintf(char * restrict s, const char * restrict format, va_list arg); */
/* int vsscanf(const char * restrict s, const char * restrict format, va_list arg); */
int fgetc(FILE *stream);
char *fgets(char * restrict s, int n, FILE * restrict stream);
int fputc(int c, FILE *stream);
int fputs(const char * restrict s, FILE * restrict stream);
int getc(FILE *stream);
int getchar(void);
char *gets(char *s);
int putc(int c, FILE *stream);
int putchar(int c);
int puts(const char *s);
int ungetc(int c, FILE *stream);
size_t fread(void * restrict ptr, size_t size, size_t nmemb, FILE * restrict stream);
size_t fwrite(const void * restrict ptr, size_t size, size_t nmemb, FILE * restrict stream);
int fgetpos(FILE * restrict stream, fpos_t * restrict pos);
int fseek(FILE *stream, long int offset, int whence);
int fsetpos(FILE *stream, const fpos_t *pos);
long int ftell(FILE *stream);
void rewind(FILE *stream);
void clearerr(FILE *stream);
int feof(FILE *stream);
int ferror(FILE *stream);
void perror(const char *s);

#endif
