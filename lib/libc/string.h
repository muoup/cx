#pragma once

#include <stdlib.h>

void memset(void* ptr, int value, size_t num);
void memcpy(void* dest, void* src, size_t num);
void memmove(void* dest, void* src, size_t num);
int memcmp(void* ptr1, void* ptr2, size_t num);

int strlen(const char* str);
int strcmp(const char* str1, const char* str2);
int strncmp(const char* str1, const char* str2, unsigned long count);
int strnlen(const char* str, unsigned long count);
char* strchr(const char* str, int character);
char* strrchr(const char* str, int character);
char* strstr(const char* str, const char* search);
char* strncpy(char* dest, const char* src, unsigned long count);
char* strdup(const char* str);
char* strerror(int error);
