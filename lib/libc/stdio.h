typedef void FILE;

FILE *fopen(char *filename, char *mode);
int fclose(FILE *stream);
void clearerr(FILE *stream);

int feof(FILE *stream);
int ferror(FILE *stream);
int fflush(FILE *stream);

char* fgets(char *s, int size, FILE *stream);
int fgetpos(FILE *stream, void *pos);

int putchar(int c);
int puts(char *s);

int printf(const char *format, ...);