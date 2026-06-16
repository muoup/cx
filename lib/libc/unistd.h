#pragma once

#include <stddef.h>

i64 read(int fd, void *buffer, size_t length);
i64 write(int fd, const void *buffer, size_t length);
int close(int fd);
