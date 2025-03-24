typedef void FILE;

void* fopen(char *filename, char *mode);
int fclose(FILE *stream);
void clearerr(FILE *stream);

int feof(void *stream);
int ferror(void *stream);
int fflush(void *stream);

int fgetpos(void *stream, void *pos);

int putchar(int c);

int puts(char *s);