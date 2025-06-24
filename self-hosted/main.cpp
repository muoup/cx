#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned long long u64;
typedef unsigned int u32;

struct string {
    char* data;
    u64 length;
    u64 capacity;
};

string new_string() {
    string s;
    s.data = (char*) malloc(16);
    s.length = 0;
    s.capacity = 16;
    s.data[0] = '\0'; // Initialize as an empty string
    return s;
}

void string_append(string* s, const char* str) {
    u64 str_length = strlen(str);

    if (s->length + str_length + 1 > s->capacity) {
        s->capacity = s->capacity * 2;
        s->data = (char*) realloc(s->data, s->capacity);
    }
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <name>\n", argv[0]);
        return 1;
    }

    string name = new_string();
    FILE *file = fopen(argv[1], "r");

    if (file == NULL) {
        printf("Error opening file");
        return 1;
    }

    char buffer[256];

    while (fgets(buffer, 255, file)) {
        string_append(&name, buffer);
        puts(buffer);
    }

    fclose(file);
    printf("File content:\n%s\n", name.data);

    return 0;
}