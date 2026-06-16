#pragma once

int socket(int domain, int type, int protocol);
int bind(int fd, const void *addr, unsigned int length);
int listen(int fd, int backlog);
int accept(int fd, void *addr, unsigned int *length);
