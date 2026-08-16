#pragma once

#include <stddef.h>
#include <stdarg.h>

typedef void FILE;

#define EOF (-1)

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

FILE *fopen(const char *filename, const char *mode);
int fclose(FILE *stream);
int remove(const char *filename);
int rename(const char *old_filename, const char *new_filename);
void clearerr(FILE *stream);

int feof(FILE *stream);
int ferror(FILE *stream);
int fflush(FILE *stream);

int fputs(const char *str, FILE *stream);
int fputc(int c, FILE *stream);
char* fgets(char *s, int size, FILE *stream);
int fgetc(FILE *stream);
int fgetpos(FILE *stream, void *pos);
long ftell(FILE *stream);
size_t fread(void *buffer, size_t size, size_t count, FILE *stream);

int putchar(int c);
int puts(char *s);

int printf(const char *format, ...);
int fprintf(FILE *stream, const char *format, ...);
int snprintf(char *buffer, unsigned long size, const char *format, ...);
int sscanf(const char *input, const char *format, ...);
int vfprintf(FILE *stream, const char *format, va_list args);
