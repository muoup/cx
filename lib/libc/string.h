#pragma once

#include <stdlib.h>

void memset(void* ptr, int value, size_t num);
void memcpy(void* dest, void* src, size_t num);
void memmove(void* dest, void* src, size_t num);
int memcmp(void* ptr1, void* ptr2, size_t num);

int strlen(const char* str);
int strcmp(const char* str1, const char* str2);