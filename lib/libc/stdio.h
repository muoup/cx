#pragma once

#include <stddef.h>

typedef void FILE;

#define EOF (-1)

FILE *fopen(const char *filename, const char *mode);
int fclose(FILE *stream);
void clearerr(FILE *stream);

int feof(FILE *stream);
int ferror(FILE *stream);
int fflush(FILE *stream);

int fputs(const char *str, FILE *stream);
int fputc(int c, FILE *stream);
char* fgets(char *s, int size, FILE *stream);
int fgetc(FILE *stream);
int fgetpos(FILE *stream, void *pos);
size_t fread(void *buffer, size_t size, size_t count, FILE *stream);

int putchar(int c);
int puts(char *s);

int printf(const char *format, ...);