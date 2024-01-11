#include <stdint.h>
#include <stdlib.h>
#include "blake3.h"

void jet_blake3 (uint8_t* out , size_t wid , uint8_t *byt)
{
        blake3_hasher hasher;
        blake3_hasher_init(&hasher);
        blake3_hasher_update(&hasher, byt, wid);
        blake3_hasher_finalize(&hasher, out, 32);
}

blake3_hasher *blake3_hasher_new ()
{
        blake3_hasher *hasher = malloc(sizeof(blake3_hasher));
        blake3_hasher_init(hasher);
        return hasher;
}

void blake3_hasher_update_byte (blake3_hasher *h, uint8_t byte)
{
        blake3_hasher_update(h, &byte, 1);
}

void blake3_hasher_update_word (blake3_hasher *h, uint64_t word)
{
        blake3_hasher_update(h, &word, 8);
}

void jet_blake3_hasher_update (blake3_hasher *h, uint64_t* word, size_t input_len)
{
        blake3_hasher_update(h, word, input_len);
}

void jet_blake3_hasher_finalize (blake3_hasher *h, uint8_t* out)
{
        blake3_hasher_finalize(h, out, 32);
}

int jet_revmemcmp(const void *s1, const void *s2, size_t n)
{
    if (n == 0) {
        return 0;
    }

    const unsigned char *p1 = (const unsigned char*) s1 + n - 1;
    const unsigned char *p2 = (const unsigned char*) s2 + n - 1;

    while (n > 0) {
        if (*p1 != *p2) { return *p1 - *p2; }
        p1--;
        p2--;
        n--;
    }

    return 0;
}
