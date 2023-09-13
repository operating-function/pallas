/*
        Copyright 2023 The Plunder Authors
        Use of this source code is governed by a BSD-style license that can be
        found in the LICENSE file.
*/

#define _GNU_SOURCE

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>

struct exp;
typedef struct exp Exp;

typedef uint64_t u64;

typedef struct bignum {
        u64 sz;
        u64 *buf;
} Bigy;

typedef struct cell {
        Exp *f;
        Exp *x;
} Cell;

typedef enum tag { WORD, BIGY, CELL } Tag;

struct exp {
        Tag tag;
        union {
                u64 w;
                Bigy b;
                Cell c;
        };
};

static inline Exp *W(u64 w) {
        Exp *res = malloc(sizeof(Exp));
        *res = (Exp){ .tag = WORD, .w = w };
        return res;
}

static inline Exp *B(u64 sz, u64 *buf) {
        Exp *res = malloc(sizeof(Exp));
        *res = (Exp){ .tag = BIGY, .b = (Bigy){sz,buf} };
        return res;
}

static inline Exp *C(Exp *f, Exp *x) {
        Exp *res = malloc(sizeof(Exp));
        *res = (Exp){ .tag = CELL, .c = (Cell){f,x} };
        return res;
}

void print_exp(Exp*);

void print_app(Exp *x) {
        if (x->tag == CELL) {
                print_app(x->c.f);
                putchar(' ');
                print_exp(x->c.x);
        } else {
                print_exp(x);
        }
}

void print_exp(Exp *x) {
        switch (x->tag) {
            case WORD:
                printf("%lu", x->w);
                break;
            case BIGY: {
                Bigy b = x->b;
                putchar('[');
                for (int i=0; i<b.sz; i++)
                        if (i == 0) {
                                printf("%lx", b.buf[b.sz-1]);
                        } else {
                                printf(".%016lx", b.buf[b.sz - (i+1)]);
                        }
                putchar(']');
                break;
            }
            case CELL:
                putchar('(');
                print_app(x);
                putchar(')');
                break;
        }
}

Exp *frag_load(Exp **tab, u64 tabSz, int *, u64 *, u64 **);

Exp *frag_load_cell(Exp **tab, u64 tabSz, int *use, u64 *acc, u64 **mor) {
        Exp *f = frag_load(tab, tabSz, use, acc, mor);
        Exp *x = frag_load(tab, tabSz, use, acc, mor);
        return C(f,x);
}
    
u64 u64_bits (u64 w) {
        if (!w) { return 0; }
        return 64 - __builtin_clzll(w);
}

Exp *frag_load(Exp **tab, u64 tabSz, int *use, u64 *acc, u64 **mor) {
        u64 isCell = ((*acc >> *use) & 1ULL);

        // move forward by one bit.
        (*use)++;
        if (*use == 64) {
                *use = 0;
                *acc = **mor;
                *mor = (*mor)+1;
        }


        if (isCell) {
                return frag_load_cell(tab, tabSz, use, acc, mor);
        }

        // `tmp` is the remaining bits from acc (high bits) combined
        // with the low bits of the next word.  We mask out the `refSz`
        // low bits from this to get the index into the backrefs table.

        u64 maxref = tabSz-1;
        u64 refSz = u64_bits(maxref);
        int remain = 64 - *use;
        u64 tmp = ((*acc >> *use) | (**mor << remain)); // combine
        u64 ref = tmp & ((1ULL << refSz) - 1);          // mask

        // move forward by refSz bits.
        *use += refSz;
        if (*use >= 64) {
                *use -= 64;
                *acc = **mor;
                *mor = (*mor)+1;
        }

        return tab[ref];
}

Exp *seed_load(u64 *buf) {
        u64 n_holes = buf[0];
        u64 n_bigs  = buf[1];
        u64 n_words = buf[2];
        u64 n_bytes = buf[3];
        u64 n_frags = buf[4];

        if (n_holes != 0) {
                fprintf(stderr, "file is just one seed, expected seedpod\n");
                exit(5);
        }

        u64 n_entries = n_bigs + n_words + n_bytes + n_frags;

        Exp **tab = malloc(sizeof(Exp*) * n_entries);

        // How big are the bignats?
        u64 bigwidths[n_bigs];
        for (int i=0; i<n_bigs; i++) {
                bigwidths[i] = buf[5+i];
        }

        Exp **next_ref = tab;
        int used = 5 + n_bigs; // number of words used

        for (int i=0; i<n_bigs; i++) {
                u64 wid  = bigwidths[i];
                *next_ref++ = B(wid, buf+used);
                used += wid;
        }

        for (int i=0; i<n_words; i++) {
                *next_ref++ = W(buf[used++]);
        }

        {
                uint8_t *byte_buf = (void*) (buf + used);
                for (int i=0; i<n_bytes; i++) {
                        *next_ref++ = W(byte_buf[i]);
                }
                used += (n_bytes / 8);
        }

        int use = 8 * (n_bytes%8);
        u64 acc = buf[used];
        u64 *more = &buf[used+1];

        for (int i=0; i<n_frags; i++) {
                u64 tabSz = (next_ref - tab);
                *next_ref++ = frag_load_cell(tab, tabSz, &use, &acc, &more);
        }

        return next_ref[-1];
}

u64 *load_seed_file (const char* filename, u64 *sizeOut) {
        FILE * f = fopen (filename, "rb");

        if (!f) exit(2);

        fseek(f, 0, SEEK_END);
        u64 szBytes = ftell(f);

        u64 szWords = (szBytes / 8) + (szBytes%8 ? 1 : 0);

        fseek(f, 0, SEEK_SET);
        u64 *buf = calloc(szWords+1, 8); // We add an extra word here
                                         // so that we can over-read
                                         // by one word, this simplifies
                                         // decoding.
        if (!buf) exit(3);
        if (fread (buf, 1, szBytes, f) != szBytes) exit(4);
        fclose(f);

        *sizeOut = szWords;
        return buf;
}

int main(int argc, char **argv) {
        char *filename = argv[1];

        if (argc != 2) {
                fprintf(stderr, "usage: ./a.out file.seed\n");
                exit(1);
        }

        u64 seedSz;
        u64 *words = load_seed_file(filename, &seedSz);

        Exp *loaded = seed_load(words);

        // Can't free words yet because the loaded bignums still contain
        // pointers this buffer.

        print_exp(loaded); printf("\n");

        return 0;
}
