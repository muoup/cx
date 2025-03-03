void* malloc(size_t size);
void* realloc(void* ptr, size_t size);
void* calloc(size_t nmemb, size_t size);

void free(void* ptr);

void memset(void* ptr, int value, size_t num);
void memcpy(void* dest, void* src, size_t num);
void memmove(void* dest, void* src, size_t num);
int memcmp(void* ptr1, void* ptr2, size_t num);