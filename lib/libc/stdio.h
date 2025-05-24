@annotations std::annotations;

typedef void fpos_t;
typedef void FILE;

void* fopen(@NonNull const char *filename, @NonNull char *mode);
int fclose(@NonNull FILE *stream);
void clearerr(@NonNull FILE *stream);

int feof(@NonNull FILE *stream);
int ferror(@NonNull FILE *stream);
int fflush(@NonNull FILE *stream);

int fgetpos(@NonNull FILE *stream, @NonNull fpos_t *pos);

int putchar(@InRange(0, 255) int c);
int puts(@NonNull char *s);

int printf(@NonNull const char *format, ...);