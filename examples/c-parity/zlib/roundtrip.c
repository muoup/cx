/* CX-STDOUT: input bytes=6291456 adler32=caff04a8 crc32=5e52e1b7 */
/* CX-STDOUT-NEXT: level=1 packed=2269486 crc32=2ebf68fb restored=yes */
/* CX-STDOUT-NEXT: level=6 packed=1885517 crc32=6f983f08 restored=yes */
/* CX-STDOUT-NEXT: level=9 packed=1840374 crc32=aaa621e9 restored=yes */

/*
 * Compresses a deterministic buffer of word-like data at several levels, restores it and
 * prints the sizes and checksums along the way.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "zlib.h"

#define INPUT_SIZE (6u << 20)

static unsigned int state = 0x2545f491u;

static unsigned int next_random(void) {
    state ^= state << 13;
    state ^= state >> 17;
    state ^= state << 5;
    return state;
}

/* Mostly words from a small vocabulary, broken up by short runs of noise. */
static void fill(unsigned char *data, size_t size) {
    static const char *const words[16] = {
        "compiler", "parity", "lowering", "block", "register", "branch", "the", "of",
        "and", "pointer", "stack", "inline", "module", "a", "type", "value",
    };
    size_t at = 0;

    while (at < size) {
        unsigned int random = next_random();

        if (random % 16 == 0) {
            unsigned int run = (random >> 8) % 32;
            while (run-- > 0 && at < size)
                data[at++] = (unsigned char)next_random();
        } else {
            const char *word = words[(random >> 4) % 16];
            while (*word != '\0' && at < size)
                data[at++] = (unsigned char)*word++;
            if (at < size)
                data[at++] = ' ';
        }
    }
}

int main(void) {
    static const int levels[] = {1, 6, 9};
    uLong bound = compressBound(INPUT_SIZE);
    unsigned char *input = malloc(INPUT_SIZE);
    unsigned char *packed = malloc(bound);
    unsigned char *restored = malloc(INPUT_SIZE);
    size_t i;

    if (input == NULL || packed == NULL || restored == NULL)
        return 1;

    fill(input, INPUT_SIZE);
    printf("input bytes=%u adler32=%08lx crc32=%08lx\n", INPUT_SIZE,
           adler32(adler32(0L, Z_NULL, 0), input, INPUT_SIZE),
           crc32(crc32(0L, Z_NULL, 0), input, INPUT_SIZE));

    for (i = 0; i < sizeof(levels) / sizeof(levels[0]); i++) {
        uLongf packed_size = bound;
        uLongf restored_size = INPUT_SIZE;

        if (compress2(packed, &packed_size, input, INPUT_SIZE, levels[i]) != Z_OK)
            return 2;
        if (uncompress(restored, &restored_size, packed, packed_size) != Z_OK)
            return 3;

        printf("level=%d packed=%lu crc32=%08lx restored=%s\n", levels[i], packed_size,
               crc32(crc32(0L, Z_NULL, 0), packed, (uInt)packed_size),
               restored_size == INPUT_SIZE && memcmp(input, restored, INPUT_SIZE) == 0
                   ? "yes"
                   : "no");
    }

    free(input);
    free(packed);
    free(restored);
    return 0;
}
