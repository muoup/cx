void* malloc(u64 size);
void* calloc(u64 num, u64 size);
void* realloc(void* ptr, u64 size);

void free(void* ptr);

void memset(void* ptr, int value, usize num);
void memcpy(void* dest, void* src, usize num);
void memmove(void* dest, void* src, usize num);
int memcmp(void* ptr1, void* ptr2, usize num);

int strlen(const char* str);
int strcmp(const char* str1, const char* str2);