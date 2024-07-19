/*
        Copyright 2023 The Plunder Authors
        Use of this source code is governed by a BSD-style license that can be
        found in the LICENSE file.
*/

// TODO We should have a `claim` call that basically asserts that the
// context is either freshly allocated, or freshly wiped.

#define DEBUG 0

#define debugfancy(fmt, ...)                                  \
        do {                                                  \
            if (DEBUG) {                                      \
                fprintf(stdout, "%s:%d:%s(): " fmt, __FILE__, \
                        __LINE__, __func__, __VA_ARGS__);     \
            }                                                 \
        } while (0)

#define debugf(fmt, ...)                           \
        do {                                       \
            if (DEBUG) {                           \
                fprintf(stdout, fmt, __VA_ARGS__); \
            }                                      \
        } while (0)


#define debugs(str)                         \
        do {                                \
            if (DEBUG) {                    \
                fprintf(stdout, "%s", str); \
            }                               \
        } while (0)

#define die(...) (fprintf(stdout, __VA_ARGS__),exit(1))

#define INLINE __attribute__((always_inline)) inline

#define BIG_CONSTANT(x) (x##ULL)


// Inlines /////////////////////////////////////////////////////////////////////

/*
        Taken from murmur3.  Used to bit-mix direct atoms and
        shape-signatures.
*/
static INLINE uint64_t fmix64 (uint64_t k) {
        k ^= k >> 33;
        k *= BIG_CONSTANT(0xff51afd7ed558ccd);
        k ^= k >> 33;
        k *= BIG_CONSTANT(0xc4ceb9fe1a85ec53);
        k ^= k >> 33;

        return k;
}

static inline int
word64_bits (uint64_t w) {
        if (!w) { return 0; }
        return 64 - __builtin_clzll(w);
}

static inline uint32_t
word64_bytes (uint64_t w) {
        uint32_t bits = word64_bits(w);
        uint32_t aligned = bits/8;
        if (bits%8) return aligned + 1;
        return aligned;
}


// Types ///////////////////////////////////////////////////////////////////////


typedef struct hash256 { uint64_t a, b, c, d; } hash256_t;

typedef struct { uint32_t ix; } bar_t;
typedef struct { uint32_t ix; } nat_t;
typedef struct { uint32_t ix; } frag_t;
typedef struct { uint32_t ix; } treenode_t;

typedef struct seed_ctx *Seed;

/*
    This strange representation (msw direct, then lsw-first array)
    allows the atoms underlying bars to be worked with in the same way
    as normal atoms, but without needing a copy.

    logically, the atom underlying a bar is the same bytes as the bar
    itself, but with a one-byte and 0-or-more 1-bytes appended.  We can
    implement this "appended" behavior without a copy by having the
    most-significant word be stored direct.  For example:

    THe bytestring "0123456789" is stored as:

        msw = {'8','9',1,0,0,0,0,0}
        nex = 1
        buf = "0123456789" -- Only the first 8-bytes are used.

    And inserting the bytestring "123" results in:

        msw = {'1','2','3',1,0,0,0,0}
        nex = 0
        buf = "123" -- all ignored

    The number 5 is stored as:

        msw = 5
        nex = 0
        buf = NULL

    The number 2^64 is stored as:

        msw = 1
        nex = 1
        buf = {0,1}  --  Last element ignored

    msw: most significant word
    nex: number of extra words
    buf: array of extra words (least significant first)
    hax: hash-table hash
*/
typedef struct leaf {
        uint64_t msw, nex, *buf, hax;
} leaf_t;

struct ser {
        uint8_t *buf;
        size_t wid;
};


// Functions ///////////////////////////////////////////////////////////////////

Seed seed_make();

void seed_wipe(Seed);
void seed_free(Seed);
void seed_done(Seed);
void seed_dbug(Seed);
void seed_show(Seed);

// All holes must be allocated up-front, before anything else.
treenode_t seed_hole   (Seed);
treenode_t seed_word   (Seed, uint64_t word);
treenode_t seed_nat    (Seed, size_t num_words, uint64_t *words);
treenode_t seed_barnat (Seed, size_t num_bytes, uint8_t  *bytes);
treenode_t seed_cons   (Seed, treenode_t h, treenode_t t);

size_t seed_size(Seed);
size_t seed_save(Seed, size_t, uint8_t*);
void   seed_load(Seed, size_t, uint8_t*);
void   seed_touch(Seed, treenode_t);

// delete me
void print_tree_pub(Seed ctx, treenode_t tree);
