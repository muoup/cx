void* fopen(char *filename, char *mode);
int fclose(void *stream);
void clearerr(void *stream);

int feof(void *stream);
int ferror(void *stream);
int fflush(void *stream);

int fgetpos(void *stream, void *pos);

int putchar(int c);
int puts(char *s);

void printf(const char *format, ...);