#pragma once

extern int *__errno_location(void);

#define errno (*__errno_location())

#define EISDIR 21
