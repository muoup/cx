#pragma once

#define NULL (void*)0

typedef unsigned long size_t;
typedef size_t time_t;

typedef struct {
    int quot;
    int rem;
} div_t;

void* malloc(size_t size);
void* calloc(size_t num, size_t size);
void* realloc(void* ptr, size_t size);

void free(void* ptr);

int atoi(const char *str);
int rand();
int srand(unsigned int seed);
div_t div(int numer, int denom);

void exit(int status);
