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
