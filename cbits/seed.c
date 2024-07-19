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
#include <inttypes.h>
#include <gmp.h>

#include "seed.h"
#include "xxh3.h"
#include "libbase58.h"
#include "blake3.h"


// Shims ///////////////////////////////////////////////////////////////////////

#ifdef __clang__
#define MUL_NO_OVERFLOW ((size_t)1 << (sizeof(size_t) * 4))
static void *reallocarray(void *optr, size_t nmemb, size_t size) {
    if ((nmemb >= MUL_NO_OVERFLOW || size >= MUL_NO_OVERFLOW) &&
         nmemb > 0 && SIZE_MAX / nmemb < size) {
        errno = ENOMEM;
        return NULL;
    }
    return realloc(optr, size * nmemb);
}
#endif

#define MAX(x, y) (((x) > (y)) ? (x) : (y))

#if defined(__APPLE__) || defined(__MACH__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__DragonFly__)
#define BSD
#endif


// Forward Declarations ////////////////////////////////////////////////////////

static void print_fragment_outline    (Seed, frag_t);
static void print_fragment            (Seed, frag_t);
static void print_nat                 (Seed, nat_t);
static void print_tree_outline        (Seed, treenode_t);
static void print_tree                (Seed, treenode_t);
static void seed_debug_interior_nodes (Seed);
static void seed_debug_leaves         (Seed, bool);

#if DEBUG
static void showbits_(char*, int wid, int num, uint64_t bits);
#define showbits(tag, wid, num, bits) showbits_(tag, wid, num, bits)
#else
#define showbits(tag, wid, num, bits) ;
#endif

static INLINE nat_t alloc_nat(Seed);


// Types ///////////////////////////////////////////////////////////////////////

typedef struct {
        treenode_t head;
        treenode_t tail;
        uint32_t leaves;
} FragVal;

typedef struct treenode_value {
        uint64_t word;
} treenode_value;

/*
        A row in the nodes_table hash table.

        It's empty if val.ent.word == UINT64_MAX

        TODO Should we use pointer.ix == UINT32_MAX instead, for consistency?
*/
typedef struct {
        treenode_value val; // (Word32, Word32)
        uint32_t hax;       // Bit mixed + truncated version of `val.word`
        treenode_t ptr;
} NodeEntry;

/*
        It's empty if ptr.ix == UINT32_MAX

        TODO: Should `leaf` be a pointer?  That would make the hash-table
        smaller but would make scanning for equality slower.
        This decision requires benchmarking.
*/
typedef struct leaves_table_entry {
        leaf_t leaf;
        treenode_t ptr;
} LeafEntry;

typedef struct seed_ctx {
    // One entry per tree-node
    treenode_value *treenodes;
    uint32_t *refcounts; // number of references for each node
    uint32_t *depths;    // tree depth at each node.
    int32_t treenodes_width;
    int32_t treenodes_count;

    // Number of external references:
    uint32_t holes_count;

    // Array of unique nat-leaves;
    leaf_t *nats;
    uint32_t nats_width;
    uint32_t nats_count;

    // Leaf Deduplication table.
    LeafEntry *leaves_table;
    uint32_t leaves_table_width;
    uint32_t leaves_table_count;

    // Interior-Node Deduplication table.
    NodeEntry *nodes_table;
    uint32_t nodes_table_width;
    uint32_t nodes_table_count;

    // Array of duplicate tree-nodes (and the top-level node).  Each one
    // is an index into `treenodes`.
    FragVal *frags;
    uint32_t frags_width;
    uint32_t frags_count;

    uint32_t *ordering;     // Array(Index (into=nats))
    uint32_t *rev_ordering; // Array(Index (into=ordering))

    int32_t num_bytes;
    int32_t num_words;
} *Seed;


// These are basically trivial macros, but written as inline functions
// for type-safety.

static INLINE treenode_value TAG_FRAG(frag_t frag) {
        uint64_t index = (uint64_t) frag.ix;
        uint64_t word  = (index | 7ULL << 61);
        return (treenode_value){ word };
}

static INLINE treenode_value TAG_PIN(int hole_id) {
        uint64_t word  = (hole_id | 5ULL << 61);
        return (treenode_value){ word };
}

static INLINE treenode_value TAG_NAT(nat_t nat) {
        uint64_t index = (uint64_t) nat.ix;
        uint64_t word  = (index | 6ULL << 61);
        return (treenode_value){ word };
}

// Pack two 32-bit indicies into one 64-bit words.  Since these indicies
// should both fit in 31 bits, we rely on the top-bit being set to zero
// and use that as a type-tag.
static INLINE treenode_value TAG_PAIR(treenode_t hed, treenode_t tel) {
        uint64_t hed_word  = (uint64_t) hed.ix;
        uint64_t tel_word  = (uint64_t) tel.ix;
        uint64_t result = ((hed_word << 32) | tel_word);
        return (treenode_value){ .word = result };
}

static INLINE uint64_t NODEVAL_TAG(treenode_value v) {
        return (v.word >> 61);
}

// If the high bit is set, it is an atom, pin, or fragment.
static inline bool NODEVAL_ISBACKREF (treenode_value v) {
        return (v.word >> 63) ? true : false; // TODO
}


static INLINE treenode_t NODEVAL_HEAD(treenode_value v) {
        return (treenode_t){ .ix = (uint32_t) (v.word >> 32) };
}

// We rely on the cast to drop the high-bits.
static INLINE treenode_t NODEVAL_TAIL(treenode_value v) {
        return (treenode_t){ .ix = (uint32_t) v.word };
}

// We rely on the cast to drop the high-bits.
static INLINE nat_t NODEVAL_NAT(treenode_value v) {
        return (nat_t){ .ix = (uint32_t) v.word };
}

// We rely on the cast to drop the high-bits.
static INLINE uint32_t NODEVAL_PIN(treenode_value v) {
        return (uint32_t) v.word;
}


// We rely on the cast to drop the high-bits.
static INLINE frag_t NODEVAL_FRAG(treenode_value v) {
        return (frag_t){ .ix = (uint32_t) v.word };
}


// Memory Management ///////////////////////////////////////////////////////////

Seed seed_make () {
        Seed res = calloc(1, sizeof(struct seed_ctx));

        // One entry per unique node (both leaves and interior nodes)
        res->treenodes       = calloc(64, sizeof(res->treenodes[0]));
        res->refcounts       = calloc(64, sizeof(res->refcounts[0]));
        res->depths          = calloc(64, sizeof(res->refcounts[0]));
        res->treenodes_width = 64;
        res->treenodes_count = 0;

        // number of external references
        res->holes_count = 0;

        // Array of unique nat-leaves
        res->nats       = calloc(16, sizeof(res->nats[0]));
        res->nats_width = 16;
        res->nats_count = 0;

        // Deduplication table for leaves
        size_t leaves_table_bytes = (1<<6) * sizeof(res->leaves_table[0]);
        res->leaves_table       = malloc(leaves_table_bytes);
        res->leaves_table_width = (1<<6);
        res->leaves_table_count = 0;
        memset(res->leaves_table, 255, leaves_table_bytes);

        // Deduplication table for interior nodes
        size_t nodes_table_bytes = (1<<6) * sizeof(res->nodes_table[0]);
        res->nodes_table       = malloc(nodes_table_bytes);
        res->nodes_table_width = (1<<6);
        res->nodes_table_count = 0;
        memset(res->nodes_table, 255, nodes_table_bytes);

        // Array of duplicate tree-nodes (and the top-level node).  Each one
        // is an index into `treenodes`.
        res->frags       = calloc(32, sizeof(res->frags[0]));
        res->frags_width = 32;
        res->frags_count = 0;

        res->ordering = NULL;
        res->rev_ordering = NULL;

        return res;
}

/*
        We don't free ny memory or shrink any tables, we just set the
        size-counts of everything to 0 and empty all the hashtable slots.
*/
void seed_wipe (Seed ctx) {
        ctx->treenodes_count    = 0;
        ctx->holes_count        = 0;
        ctx->nats_count         = 0;
        ctx->frags_count        = 0;
        ctx->leaves_table_count = 0;
        ctx->nodes_table_count  = 0;

        memset(ctx->leaves_table, 255,
               (sizeof(ctx->leaves_table[0]) * ctx->leaves_table_width));

        memset(ctx->nodes_table, 255,
               (sizeof(ctx->nodes_table[0]) * ctx->nodes_table_width));
}

void seed_free (Seed ctx) {
        free(ctx->treenodes);
        free(ctx->refcounts);
        free(ctx->depths);
        free(ctx->nats);
        free(ctx->leaves_table);
        free(ctx->nodes_table);
        free(ctx->frags);
        free(ctx);
}

static INLINE treenode_t alloc_treenode(Seed c, treenode_value v, uint32_t depth) {
        uint32_t res = c->treenodes_count++;
        uint32_t wid = c->treenodes_width;

        if (res >= wid) {
                wid *= 2;
                c->treenodes = reallocarray(c->treenodes, wid, sizeof(c->treenodes[0]));
                c->refcounts = reallocarray(c->refcounts, wid, sizeof(c->refcounts[0]));
                c->depths    = reallocarray(c->depths,    wid, sizeof(c->depths[0]));
                c->treenodes_width = wid;
        }

        c->treenodes[res] = v;
        c->refcounts[res] = 1; // nicer debug output than starting at 0
        c->depths[res] = depth;

        return (treenode_t){ .ix = res };
}

treenode_t seed_hole(Seed ctx) {
        debugs("    seed_hole()\n");
        int i = ctx->holes_count++;
        treenode_value v = TAG_PIN(i);
        return alloc_treenode(ctx, v, 0);
}


// Inserting ///////////////////////////////////////////////////////////////////

static void rehash_nodes_if_full(Seed ctx) {
        uint32_t oldwid = ctx->nodes_table_width;
        uint32_t num    = ctx->nodes_table_count;

        // If capacity is >= 50%, resize.
        if (num*2 < oldwid) return;

        uint32_t newwid = oldwid*2;

        debugf("\t\tREHASH_NODES (old=%u new=%u)\n", oldwid, newwid);

        NodeEntry *oldtab = ctx->nodes_table;
        NodeEntry *newtab = malloc(newwid * sizeof(*newtab));

        memset(newtab, 255, newwid * sizeof(*newtab));

        uint64_t newmask = newwid - 1;

        /*
                Loop over the whole of oldtab, and re-insert every
                non-empty slot.  Inserts are guarenteed to be unique,
                so we just need to, starting at the correct bucket,
                scan for an empty slot and write the value there.
        */
        for (int end=oldwid, i=0; i<end; i++) {
                NodeEntry ent = oldtab[i];

                uint64_t j = ent.hax;

                if (ent.val.word == UINT64_MAX) continue; // empty slot

                for (;; j++) {
                        j &= newmask;
                        NodeEntry *tar = newtab + j;
                        if (tar->val.word == UINT64_MAX) {
                                *tar = ent;
                                break;
                        }
                }
        }

        free(ctx->nodes_table);

        ctx->nodes_table_width = newwid;
        ctx->nodes_table       = newtab;
}

static void rehash_leaves_if_full(Seed ctx) {
        uint32_t oldwid = ctx->leaves_table_width;
        uint32_t num    = ctx->leaves_table_count;

        // If capacity is >= 50%, resize.
        if (num*2 < oldwid) return;

        uint32_t newwid = oldwid*2;

        debugf("\t\tREHASH_LEAVES (old=%u new=%u)\n", oldwid, newwid);

        LeafEntry *oldtab = ctx->leaves_table;
        LeafEntry *newtab = malloc(newwid * sizeof(*newtab));

        memset(newtab, 255, newwid * sizeof(*newtab));

        uint64_t newmask = newwid - 1;

        /*
                Loop over the whole of oldtab, and re-insert every
                non-empty slot.  Inserts are guarenteed to be unique,
                so we just need to, starting at the correct bucket,
                scan for an empty slot and write the value there.
        */
        for (int end=oldwid, i=0; i<end; i++) {
                LeafEntry ent = oldtab[i];

                // empty slot
                if (ent.ptr.ix == UINT32_MAX) continue;

                uint64_t j = ent.leaf.hax;
                for (;; j++) {
                        j &= newmask;
                        LeafEntry *tar = newtab + j;
                        if (tar->ptr.ix == UINT32_MAX) {
                                *tar = ent;
                                debugf(("\t\t%d -> %"PRIu64"\n"), i, j);
                                break;
                        } else {
                                debugf(
                                    "\t\t\t(%d -> %"PRIu64") is taken\n",
                                      i, j
                                );
                        }
                }
        }

        free(ctx->leaves_table);

        ctx->leaves_table_width = newwid;
        ctx->leaves_table       = newtab;
}


static treenode_t insert_leaf(Seed ctx, leaf_t leaf) {
        /*
                Do a linear-search over the leaves table, and use the
                index of the corresponding nat, if we find one.

                We make sure ahead-of-time that there is enough space
                for us to insert.  This way we don't have to move things
                around while were are inserting.
        */

        rehash_leaves_if_full(ctx);

        LeafEntry *ent = NULL;

        uint64_t mask = ctx->leaves_table_width - 1;
        uint64_t ix = leaf.hax;

        for (;; ix++) {
                ix &= mask;

                ent = &(ctx->leaves_table[ix]);

                // marker indicating an empty slot
                if (ent->ptr.ix == UINT32_MAX) break;

                bool match = ent->leaf.hax == leaf.hax &&
                             ent->leaf.msw == leaf.msw &&
                             ent->leaf.nex == leaf.nex &&
                               ( leaf.nex == 0 ||
                                 0 == memcmp(ent->leaf.buf, leaf.buf, 8*leaf.nex)
                               );

                if (!match) continue;

                if (DEBUG) {
                    debugs("\t\tLEAF_MATCH:\n\t\t\t");
                    print_tree_outline(ctx, ent->ptr);
                    debugs(" = ");
                    print_tree(ctx, ent->ptr);
                    debugs("\n");
                }

                ctx->refcounts[ent->ptr.ix]++;

                return ent->ptr;
        }

        /*
                We didn't find any matching entries, but now `ent`
                is pointing into an empty slot, so we fill that and
                return it.
        */

        ctx->leaves_table_count++;

        nat_t nat = alloc_nat(ctx);
        ctx->nats[nat.ix] = leaf;
        treenode_t ptr = alloc_treenode(ctx, TAG_NAT(nat), 0);

        if (DEBUG) {
                debugs("\t\tNEW_PACKED_LEAF:\n\t\t\t");
                print_tree_outline(ctx, ptr);
                debugs(" = ");
                print_tree(ctx, ptr);
                debugs("\n");
        }

        *ent = (LeafEntry){leaf, ptr};

        return ptr;
}

static inline frag_t alloc_frag(Seed ctx, FragVal frag) {
        debugf("alloc_frag(%d, %d)\n", frag.head.ix, frag.tail.ix);

        uint32_t nex = ctx->frags_count++;
        uint32_t wid = ctx->frags_width;

        if (nex >= wid) {
                wid *= 2;
                ctx->frags = reallocarray(ctx->frags, wid, sizeof(ctx->frags[0]));
                ctx->frags_width = wid;
        }

        ctx->frags[nex] = frag;

        return (frag_t){ .ix = nex };
}

struct shatter {
        uint32_t ix;     // treenode id
        uint32_t refs;   // number of references to cell
        uint32_t leaves; // number of leaves in fragment
};

// See [shatter.txt]
static void shatter(Seed ctx, treenode_t top) {
        debugs("<shatter>\n");

        // Whole seed context is just a leaf
        if (top.ix == 0) {
            debugs("</shatter>");
            return;
        }

        // Our stack depth will be (treedepth*2)+1;
        int stksz = (ctx->depths[top.ix] * 2) + 2;

        struct shatter *stk = (stksz > 128)
                              ? malloc(stksz * sizeof(struct shatter))
                              : alloca(stksz * sizeof(struct shatter))
                              ;

        struct shatter *end = stk + (stksz - 1);
        struct shatter *sp  = end;

        sp->ix     = top.ix;
        sp->leaves = 0;

        debugf("    stksz   = %d\n", stksz);
        debugf("    toprefs = %d\n", ctx->refcounts[top.ix]);
        debugf("    depth   = %d\n", ctx->depths[top.ix]);

    loop:

        debugf(
            "  sp[%"PRIu64"] is t%u (leaves=%u)\n",
            end - sp,
            sp->ix,
            sp->leaves
        );

        // stk[0] unprocessed
        if (sp->leaves == 0) {
                treenode_value val = ctx->treenodes[sp->ix];

                switch (NODEVAL_TAG(val)) {
                    case 4: case 5: case 6: case 7: // leaf
                        debugf("    LEAF: t%u\n", sp->ix);
                        sp->leaves = 1;
                        goto loop;
                    default: // cell
                        debugf("  CELL: t%u\n", sp->ix);
                        sp->refs = ctx->refcounts[sp->ix];
                        sp[-1].ix     = NODEVAL_TAIL(val).ix;
                        sp[-1].leaves = 0;
                        sp[-2].ix     = NODEVAL_HEAD(val).ix;
                        sp[-2].leaves = 0;
                        debugf("    CELL: t%u = (%u %u)\n", sp[0].ix, sp[-2].ix, sp[-1].ix);
                        sp -= 2;
                        goto loop;
                }
        }

        // sp[1] not processed, swap
        if (sp[1].leaves == 0) {
                debugs("    SWAP\n");
                struct shatter tmp = sp[0];
                sp[0] = sp[1];
                sp[1] = tmp;
                goto loop;
        }

        // both processed, combine

        // stack is in this state: {head tail cell ...}
        {
                struct shatter *hed = sp;
                struct shatter *tel = sp+1;
                struct shatter *cel = sp+2;

                debugf("    combo: t%u = (t%u, t%u)\n", cel->ix, hed->ix, tel->ix);
                debugf("        hed.leaves = %u\n", hed->leaves);
                debugf("        tel.leaves = %u\n", tel->leaves);

                cel->leaves = tel->leaves + hed->leaves;
                sp += 2;

                // top node, or more refs than parent.
                if (sp == end || sp[0].refs > sp[2].refs) {
                        debugf("FRAG(%d, leaves=%d)\n", sp->ix, sp->leaves);

                        treenode_value val = ctx->treenodes[sp->ix];

                        treenode_t hed = NODEVAL_HEAD(val);
                        treenode_t tel = NODEVAL_TAIL(val);

                        frag_t frag = alloc_frag(ctx,
                                          (FragVal){ .head=hed,
                                                     .tail=tel,
                                                     .leaves=sp->leaves
                                                   });

                        debugf("    leaf_count = %u\n", sp->leaves);

                        ctx->treenodes[sp->ix] = TAG_FRAG(frag);
                        sp->leaves = 1;
                }

                debugf("        cel.leaves = %u\n", sp->leaves);

                if (sp == end) {
                        debugs("</shatter>\n");
                        if (stksz > 128) free(stk);
                        return;
                }

                goto loop;
        }
}

// Compare nats in reverse order (biggest element comes first after
// sorting).
int cmp_nat(const void *xv, const void *yv, void *sv) {
        Seed ctx = sv;

        // casts
        const uint32_t *xi = xv;
        const uint32_t *yi = yv;

        const leaf_t x = ctx->nats[*xi];
        const leaf_t y = ctx->nats[*yi];

        if (x.nex < y.nex) return 1;
        if (x.nex > y.nex) return -1;

        if (x.msw < y.msw) return 1;
        if (x.msw > y.msw) return -1;

        if (x.nex == 0) return 0;

        return mpn_cmp(x.buf, y.buf, x.nex);
}

#ifdef BSD
int cmp_nat_wrapper(void *sv, const void *xv, const void *yv) {
        return cmp_nat(xv, yv, sv);
}
#else
int cmp_nat_wrapper(const void *xv, const void *yv, void *sv) {
        return cmp_nat(xv, yv, sv);
}
#endif

static void print_nat(Seed, nat_t);

void seed_done(Seed ctx) {
        int num = ctx->treenodes_count;

        if (num == 0) {
                die("Can't finalize empty Seed context\n");
        }

        int nats = ctx->nats_count;

        treenode_t top = (treenode_t){ .ix = num - 1 };

        if (DEBUG) {
        printf("olin: "); print_tree_outline(ctx, top); printf("\n");
        printf("tree: "); print_tree(ctx, top); printf("\n");
        }

        shatter(ctx, top);

        ctx->ordering     = malloc(sizeof(int32_t) * nats);
        ctx->rev_ordering = malloc(sizeof(int32_t) * nats);

        for (int i = 0; i < nats; i++) { ctx->ordering[i] = i; }

#ifdef BSD
        qsort_r(ctx->ordering, nats, sizeof(int32_t), ctx, cmp_nat_wrapper);
#else
        qsort_r(ctx->ordering, nats, sizeof(int32_t), cmp_nat_wrapper, ctx);
#endif

        for (int i = 0; i < nats; i++) {
                ctx->rev_ordering[ctx->ordering[i]] = i;
        }

        int32_t num_bytes = 0;
        int32_t num_words = 0;
        for (int i = 0; i < nats; i++) {
                leaf_t l = ctx->nats[i];
                if (l.nex == 0) {
                        if (l.msw < 256) num_bytes++;
                        else num_words++;
                }
        }
        debugf("num_bytes=%d\n", num_bytes);
        debugf("num_words=%d\n", num_words);
        ctx->num_bytes = num_bytes;
        ctx->num_words = num_words;
}


// Inserting Nats //////////////////////////////////////////////////////////////

static INLINE nat_t alloc_nat(Seed c) {
        uint32_t res = c->nats_count++;
        uint32_t wid = c->nats_width;

        if (res >= wid) {
                wid *= 2;
                c->nats = reallocarray(c->nats, wid, sizeof(c->nats[0]));
                c->nats_width = wid;
        }

        return (nat_t){ .ix = res };
}

treenode_t seed_word(Seed ctx, uint64_t word) {
        uint32_t byte_width = word64_bytes(word);

        debugf(
            "\tseed_packed_nat(%"PRIu64", width=%"PRIu32")\n",
            word,
            byte_width
        );

        // hashing works different here vs seed_barnat and seed_nat.
        // This is fine, because words are never equal to things greater
        // than words.  This is useful because words are extremely common,
        // and we want inserting them to be fast.

        leaf_t leaf = (leaf_t) {
            .msw = word,
            .nex = 0,
            .buf = NULL,
            .hax = fmix64(word),
        };

        return insert_leaf(ctx, leaf);
}

treenode_t seed_barnat (Seed ctx, size_t num_bytes, uint8_t* bytes) {
        if (num_bytes < 8) {
                uint64_t word = 0;
                uint8_t *ptr = (void*) &word;
                memcpy(ptr, bytes, num_bytes);
                ptr[num_bytes] = 1;
                return seed_word(ctx, word);
        }

        // Because there is an is an implicit 1 byte at the end of `bytes`.
        int width_bytes = num_bytes + 1;

        // width in words
        uint64_t overflow = width_bytes % 8;
        uint64_t wid      = (width_bytes / 8) + (overflow ? 1 : 0);

        // the last word goes in `msw`.
        uint64_t nex = wid - 1;

        //  TODO: is this undefined behavior?  Is there a better way?
        uint64_t msw = 0;
        uint8_t *dst = (void*) &msw;

        {
                uint64_t num = overflow ? overflow-1 : 7;
                uint8_t *src = bytes + (nex*8);
                memcpy(dst, src, num);
                dst[num] = 1;
        }

        uint64_t hax = fmix64(msw) ^ fmix64(nex) ^ XXH3_64bits(bytes, nex*8);

        return insert_leaf(ctx, (leaf_t){
                        .msw = msw,
                        .nex = nex,
                        .buf = (uint64_t*) bytes,
                        .hax = hax,
                }
        );
}

treenode_t seed_nat(Seed ctx, size_t wid, uint64_t *words) {
        if (wid == 0) {
                return seed_word(ctx, 0);
        }

        if (wid == 1) {
                return seed_word(ctx, words[0]);
        }

        uint64_t nex = wid - 1;
        uint64_t msw = words[nex];
        uint64_t hax = fmix64(msw) ^ fmix64(nex) ^ XXH3_64bits(words, nex*8);

        if (msw == 0) {
                die("Invalid indirect nat: Most significant word is zero!\n");
                return seed_nat(ctx, wid-1, words);
        }

        return insert_leaf(ctx, (leaf_t){
                .msw = words[nex],
                .nex = nex,
                .buf = words,
                .hax = hax,
            }
        );
}


// Bumping reference counts ////////////////////////////////////////////////////

/*
        In some situations, the caller may know that something has
        already been interned, based on the pin-hash for example.

        In that situation, it can just re-use the existing treenode_t,
        but shatter() still requires that all of the reference-counts
        be bumped.  This routine does that.

        Since this operation is very expensive for huge trees, we optimize
        by using an explicit stack with a pre-computed maximum depth.

        While this is much cheaper than re-constructing the whole subtree,
        it still scales badly for large highly-duplicated trees.

        In those situations, break your input into pins, and serialize
        each pin separately.
*/
void seed_touch(Seed ctx, treenode_t x) {
        treenode_t *stk = alloca(sizeof(treenode_t) * (ctx->depths[x.ix] + 1));
        treenode_t *sp = stk;
        sp[0] = x;

        bool big = (ctx->depths[x.ix] > 499);

        if (big) putchar('<');

        while (sp >= stk) {
                treenode_value val = ctx->treenodes[sp[0].ix];

                if (NODEVAL_TAG(val) > 3) { sp--; continue; }

                ctx->refcounts[sp[0].ix]++;

                *(sp++) = NODEVAL_HEAD(val);
                *sp     = NODEVAL_TAIL(val);
        }

        if (big) putchar('>');
}


// Inserting Nats //////////////////////////////////////////////////////////////

/*
        Either returns a pointer to a matching treenode, or returns a
        pointer into an empty slot that may be written to.
*/
treenode_t seed_cons(Seed ctx, treenode_t hed, treenode_t tel) {
        rehash_nodes_if_full(ctx);

        treenode_value val = TAG_PAIR(hed, tel);

        uint64_t target = val.word;
        uint64_t mask   = ctx->nodes_table_width - 1;

        uint32_t hax = (uint32_t) fmix64(val.word);
        uint64_t i = hax;

        for (;; i++) {
                i &= mask;

                NodeEntry *cur = (ctx->nodes_table + i);

                uint64_t word = cur->val.word;

                if (word == target) {
                        debugf("\t\tTREE MATCH\n\t\t\tt%d = ", cur->ptr.ix);
                        if (DEBUG) print_tree(ctx, cur->ptr);
                        debugs("\n");

                        ctx->refcounts[cur->ptr.ix]++;

                        return cur->ptr;
                }

                if (word == UINT64_MAX) {
                        uint32_t depth = 1 + MAX(ctx->depths[hed.ix], ctx->depths[tel.ix]);

                        treenode_t ptr = alloc_treenode(ctx, val, depth);

                        cur->val = val;
                        cur->hax = hax;
                        cur->ptr = ptr;

                        ctx->nodes_table_count++;

                        return ptr;
                }
        }
}


// Serializing /////////////////////////////////////////////////////////////////

struct frag_state {
        uint64_t acc;
        uint64_t fil;
        uint64_t *out;
        treenode_t *stack;
        uint32_t num_nats;
        uint32_t num_holes;
        int refbits;
};

static void serialize_frag(Seed ctx, struct frag_state *st, FragVal frag) {
        treenode_t *stack = st->stack;
        treenode_t treeidx;

        uint64_t fil       = st->fil;
        uint64_t acc       = st->acc;
        uint64_t *out      = st->out;
        uint64_t numnats   = st->num_nats;
        uint64_t numholes  = st->num_holes;
        uint64_t numleaves = numnats + numholes;
        int refbits        = st->refbits;

        int sp = 0;

        stack[sp++] = frag.tail;
        stack[sp]   = frag.head;

        // this is the left-recursion depth.  It tracks the number of
        // 1-tags that we need to output before the head leaf.
        int deep = 0;

        while (sp >= 0) {

            recur: // skipping the check (not needed when recursing downwwards)

                treeidx = stack[sp];
                treenode_value val = ctx->treenodes[treeidx.ix];

                /*
                        If this is a node, push the tail and then recurse
                        instead the head.  Also increment `deep` to
                        track the number of 1-bits needed.
                */
                if (!NODEVAL_ISBACKREF(val)) {
                        deep++;
                        stack[sp++] = NODEVAL_TAIL(val);
                        stack[sp]   = NODEVAL_HEAD(val);
                        goto recur;
                }

                /*
                        Output `deep` one bits.  Since the depth can be
                        bigger than 64, we may need to output multiple
                        words here.
                */
                while (deep) {
                        int remain = 64 - fil;

                        /*
                                If all of the tag bits fit in the remaining
                                bits of the accumulator, that's easy.
                        */
                        if (deep < remain) {
                                acc |= ((1ULL<<deep)- 1ULL) << fil;
                                fil += deep;
                                deep=0;
                                break;
                        }

                        /*
                                Otherwise, fill the rest of the accumulator
                                with ones and repeat the whole process.
                        */
                        *out++ = acc | (UINT64_MAX << fil);
                        acc = fil = 0;
                        deep -= remain;
                }

                /*
                        We need to output a 0 bit here, to tag this as a leaf.
                        However, we can do that more efficiently by just
                        right shifting the significant-bits by one and then
                        outputting refbits+1 bits.
                */

                /*
                        This cast to u32 is important because the truncation
                        drops the tag bits.  This should always be safe, unless
                        there are billions of unique atoms in a single pin.
                */
                uint32_t leaf = (uint32_t) val.word;
                uint64_t bits;

                /*
                        5=pin, 6=nat, 7=frag
                */
                switch (NODEVAL_TAG(val)) {
                    case 5: bits = leaf; break;
                    case 6: bits = numholes + ctx->rev_ordering[leaf]; break;
                    case 7: bits = numleaves + leaf; break;
                    default: die("impossible: 4 is bar tag (not used)");
                }

                /*
                        `bits` is left-shifted by one to create a zero-tag
                        at the front (indicating a leaf).  And we bump `fil` by
                        (refbits+1) for the same reason.
                */
                bits <<= 1;
                uint64_t new_bits = (bits << fil);
                uint64_t overflow = (bits >> (64 - fil));
                acc |= new_bits;
                fil += refbits+1;

                /*
                        If the leaf data doesn't fit in the accumulator, output
                        the accumulator, and replace it with the overflow data.
                */
                if (fil >= 64) {
                        *out++ = acc;
                        acc = overflow;
                        fil -= 64;
                }

                sp--;
        }

        // Flush these local variables back into the state (which is
        // re-loaded when we process the next fragment).
        st->fil = fil;
        st->acc = acc;
        st->out = out;
}

size_t seed_size (Seed ctx) {
        uint64_t numholes = ctx->holes_count;
        uint64_t numnats  = ctx->nats_count;
        uint64_t numbytes = ctx->num_bytes;
        uint64_t numwords = ctx->num_words;
        uint64_t numbigs  = numnats - (numbytes + numwords);
        uint64_t numfrags = ctx->frags_count;

        // The backreferences table starts off holding all "external
        // references" and all atoms.
        uint64_t refrs = numholes + numnats;

        // This is the width before the bignat size information, it will
        // be increased as we go along.  It includs the header, all the
        // bytes, the works, and the word-width of each bignat.
        uint64_t width = 40 + numbytes + numwords*8 + numbigs*8;

        // Add the actual bignat data to the result width (each bignat
        // takes n words).
        for (int end=numbigs, j=0; j<end; j++) {
                int ix = ctx->ordering[j];
                uint64_t w = ctx->nats[ix].nex + 1;
                width += (8*w);
        }

        uint64_t treebits = 0;

        for (int end=numfrags, i=0; i<end; i++) {
                // `refers` is always at least one, because you can't
                // have a cell without some atom (or external reference)
                // to have as a leaf.
                uint64_t maxref     = refrs - 1;
                uint64_t leaf_width = word64_bits(maxref);

                // Calculate the size of the fragment using the number
                // of leaves and the bit-width of each leaf.
                // The following formula works because:
                //
                // - Each leaf requires leaf_width bits
                // - Each leaf requires a single tag bit.
                // - Each interior node requires a single tag bit.
                // - There are (num_leaves - 1) interior nodes
                // - Every fragment is a cell, and so the outermost cell
                //   does not requires a tag.
                uint64_t leaves     = ctx->frags[i].leaves;
                uint64_t frag_bits  = (leaves*leaf_width) + (leaves * 2) - 2;

                // Accumulate the number of bits used in this fragment,
                // and bump `refrs` since the next fragment can also
                // reference this one.
                treebits += frag_bits;
                refrs++;
        }

        // Tree-bits is padded to be a multiple of 8 (treat as a byte-array);
        uint64_t hanging_bits = treebits % 8;
        if (hanging_bits) treebits += (8 - hanging_bits);

        // Add in the tree bits;
        width += (treebits/8);

        // Result is always a multiple of 8 (so we can treat it as an
        // array of 64-bit words);
        uint64_t hanging_bytes = width % 8;
        if (hanging_bytes) width += (8 - hanging_bytes);

        return width;
}

size_t seed_save (Seed ctx, size_t width, uint8_t *top) {
        uint32_t num_holes = ctx->holes_count;
        uint32_t num_nats  = ctx->nats_count;
        uint64_t num_bytes = ctx->num_bytes;
        uint64_t num_words = ctx->num_words;
        uint64_t num_bigs  = num_nats - (num_bytes + num_words);
        uint64_t num_frags = ctx->frags_count;

        uint64_t *header = (void*) top;
        uint8_t *out     = top + 40 + (num_bigs*8);


        // size metadata (40 bytes)

        header[0] = num_holes;
        header[1] = num_bigs;
        header[2] = num_words;
        header[3] = num_bytes;
        header[4] = num_frags;

        // bignat sizes

        for (int end=num_bigs, i=0; i < end; i++) {
                int ix = ctx->ordering[i];
                leaf_t l = ctx->nats[ix];
                uint64_t nw = l.nex + 1;
                header[5+i] = nw;
        }


        // Actual atom data in decreasing order.  Bignums first.

        for (int end=num_bigs, i=0; i < end; i++) {
                int ix = ctx->ordering[i];
                leaf_t leaf = ctx->nats[ix];
                memcpy(out, leaf.buf, leaf.nex * 8);
                out += ((leaf.nex + 1) * 8);
                ((uint64_t*)out)[-1] = leaf.msw;
        }

        // Then words.

        for (int i=0; i<num_words; i++) {
                int ix = ctx->ordering[i + num_bigs];
                ((uint64_t*)out)[0] = ctx->nats[ix].msw;
                out += 8;
        }

        // Then bytes.

        for (int i=0; i < num_bytes; i++) {
                int ix = ctx->ordering[i + num_bigs + num_words];
                *out++ = (uint8_t) ctx->nats[ix].msw;
        }

        if (!num_frags) {
                size_t width = (out - top);
                size_t hanging_bytes = width % 8;
                if (hanging_bytes) width += (8 - hanging_bytes);
                return width;
        }

        struct frag_state st;

        /*
                treenode_t and refcounts are both 32 bits, and there is one
                refcount for every node in the whole tree.  The stack only
                needs to be as big as the depth of the deepest fragment, so
                `refcounts` is plenty big.

                At this stage, we have already shattered the tree, so it's
                fine to canibalize memory like this.

                We should, however, document that a Seed context is "used up"
                after serialization, and can only be freed or wiped after that.
        */
        st.stack = (void*) ctx->refcounts;

        /*
                If we are not in word-aligned state, we need to rewind to the
                last word-aligned byte.  We will load the extra bytes into the
                initial accumulator and treat those bits as "already filled".
        */
        int used = out - top;
        int clif = used % 8;
        out -= clif;

        /*
                We read in the current state of the first word, but we don't
                move the pointer.  When this word has been filled with bytes,
                we will flush it to the array, replacing the existing `clif`
                bytes with the same data they started with.
        */
        st.out = (uint64_t*) out;
        st.acc = *(st.out);
        st.fil = 8 * clif;

        int maxref = (num_holes + num_nats) - 1;

        st.num_nats  = num_nats;
        st.num_holes = num_holes;
        st.refbits   = word64_bits(maxref);

        int until_cliff = (1 << st.refbits) - (maxref + 1);

        /*
                `until_cliff` is the number of fragments we can output before
                the fragment width grows.

                After we output, if this is zero, then we:

                -   increment st.refbits
                -   Set until_cliff to (maxref - 1).

                Setting until_clif to the value of (maxref-1) works out
                because the size increases every doubling of this value.
        */

        for (int i=0; i<num_frags; i++) {
                serialize_frag(ctx, &st, ctx->frags[i]);

                maxref++;
                if (until_cliff == 0) {
                        st.refbits++;
                        until_cliff = maxref;
                }
                until_cliff--;
        }

        /*
                If there is still data in the accumulator word (st.acc),
                then flush that to the buffer.

                The only time this doesn't happen is if the output
                perfectly fits in a multiple of 64 bits.
        */
        if (st.fil > 0) { *st.out++ = st.acc; }

        /*
                Calculate the number of bytes written, and verify that we
                filled the whole buffer and didn't overflow it.
        */
        uint8_t *end = (uint8_t*) st.out;
        return (end - top);
}


typedef struct frag_loader_state {
        Seed ctx;
        uint64_t *ptr;      //  Pointer into the remaining words.
        int rem;            //  Remaining words in the buffer.
        uint64_t acc;       //  The last word read from the buffer.
        uint64_t red;       //  The number of bits of `acc` that have been consumed.

        uint64_t ref_bits;  //  The number of bits per leaf
        uint64_t max_ref;   //  The maximum ref that we can accept;
        uint64_t num_holes;
        uint64_t num_nats;
} FragLoadSt;

typedef struct load_fragtree_result {
        treenode_t tree;
        uint32_t leaves;
} FragRes;

static FragRes load_fragtree(FragLoadSt *s);

static inline FragVal
load_fragment(FragLoadSt *s) {
        FragRes hed = load_fragtree(s);
        FragRes tel = load_fragtree(s);

        uint32_t leaves = hed.leaves + tel.leaves;

        FragVal fv = { .head=hed.tree, .tail=tel.tree, .leaves=leaves };
        return fv;
}

static FragRes
load_fragtree(FragLoadSt *s) {
        int refbits = s->ref_bits;

        showbits("mor", 64, (64-s->red), (s->acc >> s->red)); debugs("\n");

        uint64_t bit = (s->acc >> s->red) & 1;

        s->red = (s->red + 1) % 64;

        showbits("bit", 1, 1, bit);
        showbits("mor", 64, (64-s->red), (s->acc >> s->red)); debugs("\n");

        // TODO: Haskell approach is better, actually.  Don't guard
        // against end of buffer here, just use "current word = 0"
        // if we are past.  Validate at the end by checking that the
        // pointer didn't overflow.

        if (!s->red) {
                if (!s->rem) die("Internal error: not enough space\n");
                s->rem--;
                s->acc = *(s->ptr)++;
        }

        showbits("mor", 64, (64-s->red), (s->acc >> s->red)); debugs("\n");

        // TODO: Haskell approach is better, actually.  Use clz to count
        // 1s, instead of creating individual masks per one-bit.
        if (bit) {
                debugs("cell\n");
                FragVal res = load_fragment(s);
                uint32_t depth = 1 + MAX( s->ctx->depths[res.head.ix]
                                        , s->ctx->depths[res.tail.ix]
                                        );
                treenode_t tr = alloc_treenode(s->ctx, TAG_PAIR(res.head, res.tail), depth);
                return (FragRes){ .tree=tr, .leaves=res.leaves };
        }

        debugs("leaf\n");
        debugf("refbits=%u\n", refbits);

        //      -   Read n bits.
        //      -   n is the bit-width of the maximum backref at this point.
        uint64_t leaf_mask = (1 << refbits) - 1;
        uint64_t leaf = (s->acc >> s->red) & leaf_mask;

        debugf(
            "\tacc=%"PRIu64" red=%"PRIu64" | mask=%"PRIu64" leaf=%"PRIu64"\n",
            s->acc, s->red, leaf_mask, leaf
        );

        int oldred = s->red;
        debugf("[[refbits=%d oldred=%d]]\n", refbits, oldred);

        s->red += refbits;

        if (s->red >= 64) {
                int extra   = (oldred + refbits) - 64;
                int remain  = 64-extra;
                int already = refbits - extra;

                uint64_t nex = s->ptr[0];

                uint64_t why = nex & ((1ULL << extra) - 1ULL);
                uint64_t more = why << already;

                debugf(
                    "[nex=%"PRIu64" extra=%d remain=%d already=%d more=%"PRIu64" why=%"PRIu64"]\n",
                    nex, extra, remain, already, more, why
                );

                leaf |= more;

                s->red -= 64;
                s->acc = nex;
                s->rem--;
                s->ptr++;
        }

        if (leaf > s->max_ref) {
                die("leaf val is out-of-bounds (%"PRIu64")\n", leaf);
        }

        treenode_value v;
        bool is_frag = false;

        if (leaf < s->num_holes) {
            debugf("got a pin: %ld\n", leaf);
            v = TAG_PIN(leaf);
        } else {
            leaf -= s->num_holes;
            if (leaf < s->num_nats) {
                debugf("got a nat: %ld\n", leaf);
                v = TAG_NAT((nat_t){leaf});
            } else {
                debugs("got a frag\n");
                leaf -= s->num_nats;
                v = TAG_FRAG((frag_t){leaf});
                is_frag = true;
            }
        }

        uint32_t depth = 0;

        if (is_frag) {
                uint32_t frag_ix = leaf;
                FragVal v = s->ctx->frags[frag_ix];
                depth = 1 + MAX(s->ctx->depths[v.head.ix], s->ctx->depths[v.tail.ix]);
        }

        treenode_t t = alloc_treenode(s->ctx, v, depth);

        return (FragRes){ .tree = t, .leaves = 0 };
}


/*
        Note that for indirect atoms we do not copy the data, we just
        retain the pointer that we are given.

        The livetime of that data must extend until the context is wiped
        or freed.
*/
void seed_load(Seed ctx, size_t wid, uint8_t *top) {
        struct ser st = { .buf = top, .wid = wid };

        if (st.wid < 40) {
                die("Input buffer is too small to include a seed header\n");
        }

        if (st.wid % 8) {
                die("Input buffer must contain a multiple of 8 bytes.\n");
        }

        uint64_t *header = (void*) st.buf;

        uint64_t num_holes = header[0];
        uint64_t num_bigs  = header[1];
        uint64_t num_words = header[2];
        uint64_t num_bytes = header[3];
        uint64_t num_frags = header[4];

        if (DEBUG) printf("\t(setting up %"PRIu64" holes)\n", num_holes);
        for (int i=0; i < num_holes; i++) seed_hole(ctx);

        int header_size = 40 + (8 * num_bigs);

        if (st.wid < header_size) {
                die("input buffer too small to include bignat widths\n");
        }

        if (DEBUG) printf("\t(loading %"PRIu64" bignat widths)\n", num_bigs);
        uint64_t bigwidths[num_bigs];
        for (int i=0; i<num_bigs; i++) { bigwidths[i] = header[5+i]; }

        st.buf += header_size;
        st.wid -= header_size;

        if (DEBUG) {
                printf("num_holes = %"PRIu64"\n", num_holes);
                printf("num_bytes = %"PRIu64"\n", num_bytes);
                printf("num_words = %"PRIu64"\n", num_words);
                printf("num_bigs  = %"PRIu64"\n", num_bigs);
                printf("num_frags = %"PRIu64"\n", num_frags);
                for (int i=0; i<num_bigs; i++) {
                        printf("bignat_width[%d] = %ld\n", i, bigwidths[i]);
                }
        }

        if (DEBUG) printf("\t(loading %"PRIu64" bigs)\n", num_bigs);
        for (int i=0; i<num_bigs; i++) {
                uint64_t wid  = bigwidths[i];  // TODO wid<2 is an error
                uint32_t nix  = alloc_nat(ctx).ix;
                uint64_t nex  = wid-1;
                uint64_t *buf = (uint64_t*) st.buf;
                uint64_t msw  = buf[nex];
                ctx->nats[nix] = (leaf_t){msw, nex, buf, 0};
                st.wid -= (8*wid);
                st.buf += (8*wid);
        }

        if (DEBUG) printf("\t(loading %"PRIu64" words)\n", num_words);
        for (int i=0; i<num_words; i++) {
                uint64_t word = ((uint64_t*)st.buf)[0];
                uint32_t nix = alloc_nat(ctx).ix;
                ctx->nats[nix] = (leaf_t){word, 0, NULL, 0};
                st.buf += 8;
                st.wid -= 8;
        }

        if (DEBUG) printf("\t(loading %"PRIu64" bytes)\n", num_bytes);
        for (int i=0; i<num_bytes; i++) {
                uint64_t byte = st.buf[i];
                uint32_t nix = alloc_nat(ctx).ix;
                ctx->nats[nix] = (leaf_t){byte, 0, NULL, 0};
        }
        st.wid -= num_bytes;
        st.buf += num_bytes;

        uint64_t num_nats = num_bytes + num_words + num_bigs;

        if (DEBUG) printf("\t(loading %"PRIu64" frags)\n", num_frags);
        if (num_frags) {
                int used = (st.buf - top);
                int clif = used % 8;

                if (st.wid < (8 - clif)) {
                        die("Internal error: not enough bits remain to serialize frags\n");
                }

                uint64_t red, acc, *ptr, rem;

                // If reading the leaves left us in a place where we
                // are not word-aligned, then bump the pointer back to
                // the start of the last word.  We start reading the
                // accumulator at the bit-offset `red`, so this doesn't
                // add any junk to the results.
                st.buf -= clif;
                st.wid += clif;

                debugf("\tst.wid = %"PRIu64"\n", st.wid);
                debugf("\tst.buf = 0x%lx\n", (uint64_t) st.buf);

                red = clif * 8;
                ptr = (uint64_t*) st.buf;
                acc = *ptr++;
                rem = (st.wid / 8) - 1;

                FragLoadSt s = {
                        .ctx = ctx,
                        .ptr = ptr,
                        .acc = acc,
                        .red = red,
                        .rem = rem,
                        .ref_bits = 0,
                        .max_ref = 0,
                        .num_nats = num_nats,
                        .num_holes = num_holes,
                };

                for (int i=0; i<num_frags; i++) {
                        int num_refs = num_holes + num_nats + i;
                        s.max_ref  = num_refs - 1;
                        s.ref_bits = word64_bits(s.max_ref);
                        debugf("ref_bits: %ld\n", s.ref_bits);

                        debugs("\t[frag_loader_state]\n");
                        debugf("\tptr = 0x%016lx\n", (uint64_t) s.ptr);

                        debugs("       ");
                        showbits("acc", 64, 64, s.acc);
                        debugs("\n");
                        debugs("       ");
                        showbits("mor", 64, (64-s.red), (s.acc >> s.red));
                        debugs("\n");

                        debugf("\tred = %"PRIu64"\n", s.red);
                        debugf("\trem = %d\n", s.rem);
                        debugf("\tref_bits = %"PRIu64"\n", s.ref_bits);
                        debugf("\tmax_ref = %"PRIu64"\n", s.max_ref);
                        debugf("\tnum_nats = %"PRIu64"\n", s.num_nats);

                        FragVal fv = load_fragment(&s);
                        frag_t frag = alloc_frag(ctx, fv);
                        debugf("loaded frag %u\n", frag.ix);
                }

                if (s.rem != 0) {
                        // TODO: cleaner handling of this edge-case
                        if (s.rem == -1 && s.red == 0);
                        else {
                                showbits("mor", 64, (64-s.red), (s.acc >> s.red)); debugs("\n");
                                showbits("mor", 64, 64, s.ptr[1]); debugs("\n");
                                die("EXTRA STUFF %d words unread!\n", s.rem);
                        }
                }

                if (s.acc >> s.red) {
                        // TODO: cleaner handling of this edge-case
                        if (s.rem == -1 && s.red == 0);
                        else {
                                showbits("mor", 64, (64-s.red), (s.acc >> s.red)); debugs("\n");
                                die("EXTRA BITS unread in last word: %lx\n", (s.acc >> s.red));
                        }
                }

        } else {
                uint32_t num_leaves = num_holes + num_nats;

                if (num_leaves == 0) {
                        die("no leaves.\n");
                }

                if (num_leaves > 1) {
                        die("No frags, but multiple leaves.\n");
                }

                if (num_holes == 0) {
                        treenode_value v = TAG_NAT((nat_t){0});
                        alloc_treenode(ctx, v, 0);
                }

                if (st.wid != 0) {
                        debugf("EXTRA STUFF %"PRIu64" bytes unread!\n", st.wid);
                }
        }
}



////////////////////////////////////////////////////////////////////////////////
// Testing /////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

#if DEBUG
static void showbits_(char *key, int wid, int num, uint64_t bits) {
        putchar('\t');
        putchar(' ');
        if (key) debugf("%s:", key);
        putchar('(');

        int extra = wid - num;
        for (int i=0; i<extra; i++) putchar('_');

        for (num--; num >= 0; num--) {
                putchar(((bits>>num)&1) ? '1' : '0');
        }
        putchar(')');
        putchar('\n');
}
#endif

// This is just used for printing
static uint8_t *leaf_bytes(leaf_t l, int *width) {
    if (l.msw == 0) {
            *width = 0;
            return NULL;
    }

    int wid = l.nex + 1;

    uint64_t *wbuf = calloc(8, wid);

    // TODO: memcpy
    for (int i=0; i < l.nex; i++) {
            wbuf[i] = l.buf[i];
    }

    wbuf[l.nex] = l.msw;

    uint8_t *bbuf = (void*) wbuf;

    int byte_width = (wid * 8);
    while (bbuf[byte_width-1] == 0) byte_width--;

    *width = byte_width;
    return (void*) bbuf;
}


static void print_nat(Seed ctx, nat_t nat) {
        leaf_t l = ctx->nats[nat.ix];

        if (l.nex == 0 && l.msw < 256) {
                printf("%ld", l.msw);
                return;
        }

        int wid;
        uint8_t *bytes = leaf_bytes(l, &wid);

        bool is_bar    = false;
        bool is_string = true;
        int end = wid - 1;

        if (bytes[end] == 1) end--, is_bar=true;

        for (int i = end; i>=0; i--) {
                uint8_t byte = bytes[i];
                if (isprint(byte)) continue;
                is_string=false;
                is_bar=false;
                break;
        }

        if (is_bar) printf("b#");

        if (is_bar || is_string) {
                putchar('{');
                fwrite(bytes, 1, (end+1), stdout);
                putchar('}');
                free(bytes);
                return;
        }

        free(bytes);

        if (l.nex == 0) {
                printf("%ld", l.msw);
        } else {
                printf("0x%lx", l.msw);
                for (int i=l.nex; i>0; i--) {
                        printf(".%016lx", l.buf[i-1]);
                }
        }
}

static void print_tree_outline_list(Seed ctx, treenode_t tree) {
        treenode_value val = ctx->treenodes[tree.ix];

        uint32_t offset = (uint32_t) val.word;

        switch (NODEVAL_TAG(val)) {
            case 4: die("impossible: 4 tag is not used");
            case 5: printf("p%u", offset); break;
            case 6: printf("n%u", offset); break;
            case 7: printf("f%u", offset); break;
            default: {
                treenode_t hed = NODEVAL_HEAD(val);
                treenode_t tel = NODEVAL_TAIL(val);
                print_tree_outline_list(ctx, hed);
                putchar(' ');
                print_tree_outline(ctx, tel);
            }
        }
}

static void print_tree_outline(Seed ctx, treenode_t tree) {
        treenode_value val = ctx->treenodes[tree.ix];

        uint32_t offset = (uint32_t) val.word;

        switch (NODEVAL_TAG(val)) {
            case 4: die("impossible: 4 tag is not used");
            case 5: printf("p%u", offset); break;
            case 6: printf("n%u", offset); break;
            case 7: printf("f%u", offset); break;
            default: {
                treenode_t hed = NODEVAL_HEAD(val);
                treenode_t tel = NODEVAL_TAIL(val);
                putchar('(');
                print_tree_outline_list(ctx, hed);
                putchar(' ');
                print_tree_outline(ctx, tel);
                putchar(')');
            }
        }
}

static void print_tree_list(Seed ctx, treenode_t tree) {
        treenode_value val = ctx->treenodes[tree.ix];

        switch (NODEVAL_TAG(val)) {
            case 4: die("impossible: 4 tag is not used");
            case 5: printf("p%d", NODEVAL_PIN(val)); break;
            case 6: print_nat(ctx, NODEVAL_NAT(val)); break;
            case 7: {
                FragVal frag = ctx->frags[NODEVAL_FRAG(val).ix];
                print_tree_list(ctx, frag.head);
                putchar(' ');
                print_tree(ctx, frag.tail);
                break;
            }
            default: {
                treenode_t hed = NODEVAL_HEAD(val);
                treenode_t tel = NODEVAL_TAIL(val);
                print_tree_list(ctx, hed);
                putchar(' ');
                print_tree(ctx, tel);
            }
        }
}

void print_tree_pub(Seed ctx, treenode_t tree) {
        print_tree(ctx, tree);
}

static void print_tree(Seed ctx, treenode_t tree) {
        treenode_value val = ctx->treenodes[tree.ix];

        switch (NODEVAL_TAG(val)) {
            case 4: die("impossible: four tag is not used");
            case 5: printf("p%d", NODEVAL_PIN(val)); break;
            case 6: print_nat(ctx, NODEVAL_NAT(val)); break;
            case 7: print_fragment(ctx, NODEVAL_FRAG(val)); break;
            default: {
                treenode_t hed = NODEVAL_HEAD(val);
                treenode_t tel = NODEVAL_TAIL(val);
                putchar('(');
                print_tree_list(ctx, hed);
                putchar(' ');
                print_tree(ctx, tel);
                putchar(')');
            }
        }
}

static void print_fragment_outline(Seed ctx, frag_t ref) {
        FragVal frag = ctx->frags[ref.ix];
        putchar('(');
        print_tree_outline_list(ctx, frag.head);
        putchar(' ');
        print_tree_outline(ctx, frag.tail);
        putchar(')');
}

static void print_fragment(Seed ctx, frag_t ref) {
        FragVal frag = ctx->frags[ref.ix];
        putchar('(');
        print_tree_list(ctx, frag.head);
        putchar(' ');
        print_tree(ctx, frag.tail);
        putchar(')');
}


static void seed_debug_leaves(Seed ctx, bool details) {
        printf("\n\tleaves: (width=%u, count=%u)\n\n",
               ctx->leaves_table_width,
               ctx->leaves_table_count);

        int wid = ctx->leaves_table_width;

        uint64_t mask = ctx->leaves_table_width - 1;

        for (int i=0; i<wid; i++) {
                LeafEntry ent = ctx->leaves_table[i];

                // Empty slot
                if (ent.ptr.ix == UINT32_MAX) continue;

                printf("\t\t%4d = ", i);
                print_tree_outline(ctx, ent.ptr);
                printf("\t(val=");
                print_tree(ctx, ent.ptr);
                printf(")\n");

                if (details) {
                        uint64_t width = (ent.leaf.nex + 1);
                        uint64_t bin = ent.leaf.hax & mask;
                        uint64_t distance = (((uint64_t)i) - bin);
                        printf("\t\t    bytes: %-4lu\n", width);
                        printf("\t\t    bin: %"PRIu64" [dist=%"PRIu64"]\n",
                               bin, distance);
                        printf("\t\t    hash: 0x%016lx\n", ent.leaf.hax);
                }
        }
}


static void seed_debug_interior_nodes(Seed ctx) {
        printf("\n\tinterior_nodes_table: (width=%u, count=%u)\n\n",
               ctx->nodes_table_width,
               ctx->nodes_table_count);

        int wid = ctx->nodes_table_width;

        uint64_t mask = ctx->nodes_table_width - 1;

        for (int i=0; i<wid; i++) {
                NodeEntry ent = ctx->nodes_table[i];

                // Empty slot
                if (ent.val.word == UINT64_MAX) continue;

                uint64_t bin = ent.hax & mask;
                uint64_t distance = (((uint64_t)i) - bin);

                printf(
                    "\t\t%4d = t%u\tbin=%"PRIu64"\tdist=%"PRIu64"\thash=%08x\n",
                    i, ent.ptr.ix, bin, distance, ent.hax
                );
        }
}


void seed_dbug(Seed ctx) {
        printf("\nseed_debug():\n");

        seed_debug_leaves(ctx, false);

        seed_debug_interior_nodes(ctx);

        {
                printf("\n\tnats: (width=%u, count=%u)\n\n",
                       ctx->nats_width,
                       ctx->nats_count);
                int num = ctx->nats_count;
                for (int i=0; i<num; i++) {
                        printf("\t\tn%d = ", i);
                        print_nat(ctx, (nat_t){ .ix = i });
                        printf("\n");
                }
        }

        {
                printf("\n\ttreenodes: (width=%u, count=%u)\n\n",
                       ctx->treenodes_width,
                       ctx->treenodes_count);
                int num = ctx->treenodes_count;
                for (int i=0; i<num; i++) {
                        int ref = ctx->refcounts[i];
                        treenode_value val = ctx->treenodes[i];
                        switch (NODEVAL_TAG(val)) {
                            case 4:
                                die("impossible: 4 tag is not used");
                                continue;
                            case 5:
                                printf("\t\tt%d = p%u\t\trefs=%u\n", i, (uint32_t) val.word, ref);
                                continue;
                            case 6:
                                printf("\t\tt%d = n%u\t\trefs=%u\n", i, (uint32_t) val.word, ref);
                                continue;
                            case 7:
                                printf("\t\tt%d = f%u\t\trefs=%u\n", i, (uint32_t) val.word, ref);
                                continue;
                            default: {
                                treenode_t hed = NODEVAL_HEAD(val);
                                treenode_t tel = NODEVAL_TAIL(val);
                                uint32_t deep = ctx->depths[i];
                                printf("\t\tt%d = (%u, %u)\trefs=%u\tdeep=%u\n", i, hed.ix, tel.ix, ref, deep);
                                continue;
                            }
                        }
                }
        }

        printf("\nSeed Fragments:\n\n");

        uint32_t count = ctx->frags_count;
        for (int i=0; i<count; i++) {
                FragVal cur = ctx->frags[i];
                printf("\tFragment[%d]:\n\n\t\t(t%d, t%d) = ", i, cur.head.ix, cur.tail.ix);
                print_fragment_outline(ctx, (frag_t){i});

                printf("\n\n\t\t    ");
                print_fragment(ctx, (frag_t){i});
                printf("\n\n");
        }
}

void seed_show(Seed ctx) {
        uint32_t frags = ctx->frags_count;

        if (frags) {
                print_fragment(ctx, (frag_t){ .ix = (frags-1) });
        } else {

                if (!ctx->treenodes_count) {
                        die("No frags and no leaves\n");
                }

                if (ctx->treenodes_count > 1) {
                        die("No frags but %d leaves.  Nonsense!\n", ctx->treenodes_count);
                }

                print_tree(ctx, (treenode_t){0});
        }

        putchar('\n');
}
