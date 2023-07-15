#include <stdint.h>
#include "blake3.h"

void jet_blake3 (uint8_t* out , size_t wid , uint8_t *byt)
{
        blake3_hasher hasher;
        blake3_hasher_init(&hasher);
        blake3_hasher_update(&hasher, byt, wid);
        blake3_hasher_finalize(&hasher, out, 32);
}
