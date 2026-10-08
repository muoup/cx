#pragma once

typedef usize ptrdiff_t;
typedef usize size_t;

#define NULL ((void*)0)
#define offsetof(type, member) __builtin_offsetof(type, member)