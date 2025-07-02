#define NULL (void*)0

typedef u64 size_t;

void* malloc(u64 size);
void* calloc(u64 num, u64 size);
void* realloc(void* ptr, u64 size);

void free(void* ptr);

int atoi(const char *str);
int rand();
int srand(u32 seed);