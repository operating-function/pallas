#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>

#include "jelly.h"
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


// Forward Declarations ////////////////////////////////////////////////////////

static void print_tree_outline(Jelly, treenode_t);
static void print_tree(Jelly, treenode_t);


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
    10xx.. indicates a pin
    11xx.. indicates a bar
    0xxx.. indicates a nat
*/
typedef struct tagged_width { uint64_t word; } tagged_width_t;

/*
        A row in the nodes_table hash table.

        It's empty if val.ent.word == UINT64_MAX

        TODO Should we use pointer.ix == UINT32_MAX instead, for
        consistency?
*/
typedef struct {
    treenode_value val;   // (Word32, Word32)
    uint32_t hash;        // Bit mixed + truncated version of `val.word`
    treenode_t pointer;   // If this is in a `desks` freelist,
                          // then this is instead an index into the
                          // desks array.
} NodeEntry;


/*
        If the byte-array width is less than nine, the data is directly
        inlined into `bytes`.

        It's empty if pointer.ix == UINT32_MAX
*/
typedef struct leaves_table_entry {
        uint64_t hash;
        tagged_width_t width;
        uint8_t *bytes;
        treenode_t pointer;
} LeafEntry;

typedef struct jelly_ctx {
    // One entry per tree-node
    treenode_value *treenodes;
    uint32_t *refcounts;
    int32_t treenodes_width;
    int32_t treenodes_count;

    // Array of unique pin-leaves.
    hash256_t **pins;
    uint32_t pins_width;
    uint32_t pins_count;

    // Array of unique bar-leaves
    leaf_t   *bars;
    uint32_t bars_width;
    uint32_t bars_count;

    // Array of unique nat-leaves;
    leaf_t   *nats;
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
} *Jelly;


// These are basically trivial macros, but written as inline functions
// for type-safety.

static INLINE treenode_value TAG_FRAG(frag_t frag) {
        uint64_t index = (uint64_t) frag.ix;
        uint64_t word  = (index | 7ULL << 61);
        return (treenode_value){ word };
}

static INLINE treenode_value TAG_PIN(pin_t pin) {
        uint64_t index = (uint64_t) pin.ix;
        uint64_t word  = (index | 4ULL << 61);
        return (treenode_value){ word };
}

static INLINE treenode_value TAG_BAR(bar_t bar) {
        uint64_t index = (uint64_t) bar.ix;
        uint64_t word  = (index | 5ULL << 61);
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
static INLINE frag_t NODEVAL_FRAG(treenode_value v) {
        return (frag_t){ .ix = (uint32_t) v.word };
}

// We rely on the cast to drop the high-bits.
static INLINE bar_t NODEVAL_BAR(treenode_value v) {
        return (bar_t){ .ix = (uint32_t) v.word };
}

// We rely on the cast to drop the high-bits.
static INLINE pin_t NODEVAL_PIN(treenode_value v) {
        return (pin_t){ .ix = (uint32_t) v.word };
}

// Memory Management ///////////////////////////////////////////////////////////

Jelly jelly_make () {
        Jelly res = calloc(1, sizeof(struct jelly_ctx));

        // One entry per unique node (both leaves and interior nodes)
        res->treenodes       = calloc(64, sizeof(res->treenodes[0]));
        res->refcounts       = calloc(64, sizeof(res->refcounts[0]));
        res->treenodes_width = 64;
        res->treenodes_count = 0;

        // Array of unique pin-leaves.
        res->pins       = calloc(16, sizeof(res->pins[0]));
        res->pins_width = 16;
        res->pins_count = 0;

        // Array of unique bar-leaves
        res->bars       = calloc(16, sizeof(res->bars[0]));
        res->bars_width = 16;
        res->bars_count = 0;

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

        return res;
}

/*
        We don't free ny memory or shrink any tables, we just set the
        size-counts of everything to 0 and empty all the hashtable slots.
*/
void jelly_wipe (Jelly ctx) {
        ctx->treenodes_count = 0;
        ctx->pins_count = 0;
        ctx->bars_count = 0;
        ctx->nats_count = 0;
        ctx->frags_count = 0;

        ctx->leaves_table_count = 0;
        ctx->nodes_table_count = 0;

        memset(ctx->leaves_table, 255,
               (sizeof(ctx->leaves_table[0]) * ctx->leaves_table_width));

        memset(ctx->nodes_table, 255,
               (sizeof(ctx->nodes_table[0]) * ctx->nodes_table_width));
}

void jelly_free (Jelly ctx) {
        free(ctx->treenodes);
        free(ctx->refcounts);
        free(ctx->pins);
        free(ctx->bars);
        free(ctx->nats);
        free(ctx->leaves_table);
        free(ctx->nodes_table);
        free(ctx->frags);
        free(ctx);
}


static INLINE treenode_t alloc_treenode(Jelly c, treenode_value v) {
        uint32_t res = c->treenodes_count++;
        uint32_t wid = c->treenodes_width;

        if (res >= wid) {
                wid *= 2;
                c->treenodes = reallocarray(c->treenodes, wid, sizeof(c->treenodes[0]));
                c->refcounts = reallocarray(c->refcounts, wid, sizeof(c->refcounts[0]));
                c->treenodes_width = wid;
        }

        c->treenodes[res] = v;
        c->refcounts[res] = 1; // nicer debug output than starting at 0

        return (treenode_t){ .ix = res };
}


// Tagged Widths ///////////////////////////////////////////////////////////////

/*
        0xxx.. indicates a nat
        10xx.. indicates a pin
        11xx.. indicates a bar
*/
static INLINE tagged_width_t NAT_TAGGED_WIDTH(uint32_t bytes) {
        uint64_t word = bytes;
        return (tagged_width_t){ .word = word };
}

static INLINE tagged_width_t PIN_TAGGED_WIDTH(uint32_t bytes) {
        uint64_t word = bytes;
        word |= 2ULL << 62;
        return (tagged_width_t){ .word = word };
}

static INLINE tagged_width_t BAR_TAGGED_WIDTH(uint32_t bytes) {
        uint64_t word = bytes;
        word |= 3ULL << 62;
        return (tagged_width_t){ .word = word };
}


// Inserting ///////////////////////////////////////////////////////////////////

typedef struct {
        tagged_width_t width;
        uint64_t hash;
        uint64_t word;
        treenode_t (*new_leaf)(Jelly, bool*, leaf_t);
        bool *is_unique;
} PackedInsertRequest;

typedef struct {
        tagged_width_t width;
        uint64_t hash;
        leaf_t leaf;
        treenode_t (*new_leaf)(Jelly, bool*, leaf_t);
        bool *is_unique;
} IndirectInsertRequest;

static void jelly_debug_leaves(Jelly, bool);
static void jelly_debug_interior_nodes(Jelly);

static void rehash_nodes_if_full(Jelly ctx) {
        uint32_t oldwid = ctx->nodes_table_width;
        uint32_t num    = ctx->nodes_table_count;

        // If capacity is >= 50%, resize.
        if (num*2 < oldwid) return;

        uint32_t newwid = oldwid*2;

        debugf("\t\tREHASH_NODES (old=%u new=%u)\n", oldwid, newwid);

        // debugs("OLD TABLE:\n");
        // jelly_debug_interior_nodes(ctx);
        // debugs("\n");

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
        for (uint64_t i=0; i<oldwid; i++) {
                NodeEntry ent = oldtab[i];

                uint64_t j = ent.hash;

                if (ent.val.word == UINT64_MAX) continue; // empty slot

                for (;; j++) {
                        j &= newmask;
                        NodeEntry *tar = newtab + j;
                        if (tar->val.word == UINT64_MAX) {
                                *tar = ent;
                                // debugf("\t\t%lu -> %lu\n", i, j);
                                break;
                        } // else {
                                // debugf("\t\t\t(%lu -> %lu) is taken\n", i, j);
                        // }
                }
        }

        free(ctx->nodes_table);

        ctx->nodes_table_width = newwid;
        ctx->nodes_table       = newtab;

        // debugs("NEW TABLE:\n");
        // jelly_debug_interior_nodes(ctx);
        // debugs("\n");
}

static void rehash_leaves_if_full(Jelly ctx) {
        uint32_t oldwid = ctx->leaves_table_width;
        uint32_t num    = ctx->leaves_table_count;

        // If capacity is >= 50%, resize.
        if (num*2 < oldwid) return;

        uint32_t newwid = oldwid*2;

        debugf("\t\tREHASH_LEAVES (old=%u new=%u)\n", oldwid, newwid);

        // debugs("OLD TABLE:\n");
        // jelly_debug_leaves(ctx, false);
        // debugs("\n");

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
        for (uint64_t i=0; i<oldwid; i++) {
                LeafEntry ent = oldtab[i];

                // empty slot
                if (ent.pointer.ix == UINT32_MAX) continue;

                uint64_t j = ent.hash;
                for (;; j++) {
                        j &= newmask;
                        LeafEntry *tar = newtab + j;
                        if (tar->pointer.ix == UINT32_MAX) {
                                *tar = ent;
                                debugf("\t\t%lu -> %lu\n", i, j);
                                break;
                        } else {
                                debugf("\t\t\t(%lu -> %lu) is taken\n", i, j);
                        }
                }
        }

        free(ctx->leaves_table);

        ctx->leaves_table_width = newwid;
        ctx->leaves_table       = newtab;

        // debugs("NEW TABLE:\n");
        // jelly_debug_leaves(ctx, false);
        // debugs("\n");
}


static treenode_t insert_packed_leaf(Jelly ctx, PackedInsertRequest req) {
        uint64_t tag   = (req.width.word >> 62);
        uint64_t bytes = (req.width.word << 2) >> 2;

        if (word64_bytes(req.word) > 8) {
                die("packed leaf is too big to actually be packed\n");
        }

        debugf(
            "\tpacked_insert(tag=%lu, wid=%lu, hash=%lu, %lu)\n",
            tag,
            bytes,
            req.hash,
            req.word
        );

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
        uint64_t ix = req.hash;

        for (;; ix++) {
                ix &= mask;

                ent = &(ctx->leaves_table[ix]);

                if (ent->pointer.ix == UINT32_MAX) break;

                bool match = ent->hash == req.hash &&
                             ent->width.word == req.width.word &&
                             ent->bytes == (uint8_t*) req.word;

                if (!match) continue;

                debugs("\t\tPACKED_LEAF_MATCH:\n\t\t\t");
                ctx->refcounts[ent->pointer.ix]++;
                if (DEBUG) print_tree_outline(ctx, ent->pointer);
                debugs(" = ");
                if (DEBUG) print_tree(ctx, ent->pointer);
                debugs("\n");

                return ent->pointer;
        }

        /*
                We didn't find any matching entries, but now `ent`
                is pointing into an empty slot, so we fill that and
                return it.
        */

        ctx->leaves_table_count++;

        leaf_t leaf = (leaf_t){
                .width_bytes = bytes,
                .bytes = (uint8_t*) req.word
        };

        treenode_t pointer = req.new_leaf(ctx, req.is_unique, leaf);

        debugs("\t\tNEW_PACKED_LEAF:\n\t\t\t");
        if (DEBUG) print_tree_outline(ctx, pointer);
        debugs(" = ");
        if (DEBUG) print_tree(ctx, pointer);
        debugs("\n");

        *ent = (LeafEntry){
            .hash  = req.hash,
            .width = req.width,
            .bytes = (uint8_t*) req.word,
            .pointer = pointer,
        };

        return pointer;
}

static treenode_t insert_indirect_leaf(Jelly ctx, IndirectInsertRequest req) {
        debugf("\tinsert_indirect_leaf(wid=%u)\n", req.leaf.width_bytes);

        int wid = req.leaf.width_bytes;

        if (wid < 9) {
                die("Invalid indirect leaf: Too Short! Should be direct!\n");
        }

        /*
                Do a linear-search over the leaves table, and return the
                index of the corresponding nat, if we find one.
        */

        rehash_leaves_if_full(ctx); // Make sure there is space to insert,
                                    // if we do it later, we will invalidate
                                    // our pointer.

        LeafEntry *ent;

        uint64_t mask = ctx->leaves_table_width - 1;
        uint64_t ix = req.hash;

        int32_t byt_wid = req.leaf.width_bytes;
        uint8_t *byt    = req.leaf.bytes;

        for (;; ix++) {
                ix &= mask;

                ent = &(ctx->leaves_table[ix]);

                // An empty slot ends the search.
                if (ent->pointer.ix == UINT32_MAX) break;

                if (ent->hash != req.hash) continue;

                if (ent->width.word != req.width.word) continue;

                if (0 != memcmp(ent->bytes, byt, byt_wid)) continue;

                debugs("\t\tINDIRECT_LEAF_MATCH:\n\t\t\t");
                ctx->refcounts[ent->pointer.ix]++;
                if (DEBUG) print_tree_outline(ctx, ent->pointer);
                debugs(" = ");
                if (DEBUG) print_tree(ctx, ent->pointer);
                debugs("\n");

                return ent->pointer;
        }

        ctx->leaves_table_count++;

        treenode_t pointer = req.new_leaf(ctx, req.is_unique, req.leaf);

        debugs("\t\tNEW_INDIRECT_LEAF:\n\t\t\t");
        if (DEBUG) print_tree_outline(ctx, pointer);
        debugs(" = ");
        if (DEBUG) print_tree(ctx, pointer);
        debugs("\n");

        *ent = (LeafEntry){
            .hash  = req.hash,
            .width = req.width,
            .bytes = req.leaf.bytes,
            .pointer = pointer,
        };

        return pointer;
}

static inline frag_t alloc_frag(Jelly ctx, FragVal frag) {
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
static void shatter(Jelly ctx, treenode_t top) {
        debugs("<shatter>\n");

        // Whole jelly context is just a leaf
        if (top.ix == 0) {
            debugs("</shatter>");
            return;
        }

        // Our stack depth will be (treedepth*2)+1;
        //
        // A maximally duplicated tree has treedepth+1 nodes.
        //
        // So, this is a generous upper bound.
        int stksz = ctx->treenodes_count * 2;

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

    loop:

        debugf("sp[%lu] is t%u (leaves=%u)\n", end - sp, sp->ix, sp->leaves);

        // stk[0] unprocessed
        if (sp->leaves == 0) {
                treenode_value val = ctx->treenodes[sp->ix];

                switch (NODEVAL_TAG(val)) {
                    case 4: case 5: case 6: case 7: // leaf
                        debugf("    LEAF: t%u\n", sp->ix);
                        sp->leaves = 1;
                        goto loop;
                    default: // cell
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
                        treenode_value val = ctx->treenodes[sp->ix];

                        debugf("FRAG(%d, leaves=%d)\n", sp->ix, sp->leaves);

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

void jelly_done(Jelly ctx) {
        int num = ctx->treenodes_count;

        if (num == 0) {
                die("Can't finalize empty Jelly context\n");
        }

        treenode_t top = (treenode_t){ .ix = num - 1 };

        shatter(ctx, top);
}


// Inserting Nats //////////////////////////////////////////////////////////////

static INLINE nat_t alloc_nat(Jelly c) {
        uint32_t res = c->nats_count++;
        uint32_t wid = c->nats_width;

        if (res >= wid) {
                wid *= 2;
                c->nats = reallocarray(c->nats, wid, sizeof(c->nats[0]));
                c->nats_width = wid;
        }

        return (nat_t){ .ix = res };
}

static treenode_t
new_nat (Jelly ctx, bool *is_unique, leaf_t leaf) {
        nat_t nat = alloc_nat(ctx);
        ctx->nats[nat.ix] = leaf;
        return alloc_treenode(ctx, TAG_NAT(nat));
}

static INLINE treenode_t
jelly_packed_nat(Jelly ctx, uint32_t byte_width, uint64_t word) {
        debugf("\tjelly_packed_nat(%lu, width=%u)\n", word, byte_width);

        return insert_packed_leaf(ctx,
            (PackedInsertRequest){
                .width = NAT_TAGGED_WIDTH(byte_width),
                .hash = fmix64(word),
                .word = word,
                .new_leaf = new_nat,
                .is_unique = NULL,
            }
        );
}

treenode_t jelly_word(Jelly ctx, uint64_t word) {
    int wid = word64_bytes(word);
    return jelly_packed_nat(ctx, wid, word);
}

treenode_t jelly_nat(Jelly ctx, size_t wid, uint8_t *byt) {
        debugf("\tjelly_nat(width=%lu)\n", wid);

        if (wid == 0) {
                return jelly_word(ctx, 0);
	}

	if (wid < 9) {
                uint64_t word = 0;
                memcpy(&word, byt, wid);

                if (word64_bytes(word) != wid) {
                        die("Invalid small nat: width doesn't match data\n");
		}

                return jelly_word(ctx, word);
	}

        uint64_t hash = XXH3_64bits(byt, wid);

        if (byt[wid - 1] == 0) {
                die("Invalid indirect nat: Most significant byte is zero!\n");
        }

        return insert_indirect_leaf(ctx,
            (IndirectInsertRequest){
                .width = NAT_TAGGED_WIDTH(wid),
                .hash = hash,
                .leaf = (leaf_t){ .width_bytes = wid, .bytes = byt },
                .new_leaf = new_nat,
                .is_unique = NULL,
            }
        );
}




// Inserting Bars //////////////////////////////////////////////////////////////

INLINE bar_t alloc_bar(Jelly c) {
        uint32_t res = c->bars_count++;
        uint32_t wid = c->bars_width;

        if (res >= wid) {
                wid *= wid;
                c->bars = reallocarray(c->bars, wid, sizeof(c->bars[0]));
                c->bars_width = wid;
        }

        return (bar_t){ .ix = res };
}

static treenode_t
new_bar (Jelly ctx, bool *is_unique, leaf_t leaf) {
        bar_t bar = alloc_bar(ctx);
        ctx->bars[bar.ix] = leaf;
        return alloc_treenode(ctx, TAG_BAR(bar));
}

treenode_t jelly_bar(Jelly ctx, size_t wid, uint8_t *byt) {
        debugf("\tjelly_bar(width=%lu)\n", wid);

        if (wid < 9) {
                uint64_t word = 0;

                if (wid) memcpy(&word, byt, wid);

                debugf("\tjelly_packed_bar(%lu, width=%lu)\n", word, wid);

                return insert_packed_leaf(ctx,
                    (PackedInsertRequest){
                        .width = BAR_TAGGED_WIDTH(wid),
                        .hash = fmix64(word),
                        .word = word,
                        .new_leaf = new_bar,
                        .is_unique = NULL,
                    }
                );
        } else {
                uint64_t hash = XXH3_64bits(byt, wid);

                leaf_t leaf = { .width_bytes = wid, .bytes = byt };

                return insert_indirect_leaf(ctx,
                    (IndirectInsertRequest){
                        .width = BAR_TAGGED_WIDTH(wid),
                        .hash = hash,
                        .leaf = leaf,
                        .new_leaf = new_bar,
                        .is_unique = NULL,
                    }
                );
        }
}


// Inserting Pins //////////////////////////////////////////////////////////////

INLINE pin_t alloc_pin(Jelly c) {
        uint32_t res = c->pins_count++;
        uint32_t wid = c->pins_width;

        if (res >= wid) {
                wid *= 2;
                c->pins = reallocarray(c->pins, wid, sizeof(c->pins[0]));
                c->pins_width = wid;
        }

        return (pin_t){ .ix = res };
}

static treenode_t new_pin (Jelly ctx, bool *is_unique, leaf_t leaf) {
        if (is_unique != NULL) { *is_unique = true; }
        hash256_t *hash = (hash256_t*) leaf.bytes;
        pin_t pin = alloc_pin(ctx);
        ctx->pins[pin.ix] = hash;
        return alloc_treenode(ctx, TAG_PIN(pin));
}

treenode_t jelly_pin(Jelly ctx, bool *is_unique, uint8_t *hash_bytes) {
        hash256_t *pin = (void*) hash_bytes;

        debugf("\tjelly_pin(hash=%lx)\n", pin->a);

        uint64_t hash = pin->a;

        if (is_unique != NULL) { *is_unique = false; }

        return insert_indirect_leaf(ctx,
            (IndirectInsertRequest){
                .width = PIN_TAGGED_WIDTH(32),
                .hash = hash,
                .leaf = (leaf_t) { .width_bytes = 32, .bytes = (uint8_t*) pin },
                .new_leaf = new_pin,
                .is_unique = is_unique,
            }
        );
}


// Inserting Nats //////////////////////////////////////////////////////////////

/*
        Either returns a pointer to a matching treenode, or returns a
        pointer into an empty slot that may be written to.
*/
treenode_t jelly_cons(Jelly ctx, treenode_t hed, treenode_t tel) {
        rehash_nodes_if_full(ctx);

        treenode_value val = TAG_PAIR(hed, tel);

        uint64_t target = val.word;
        uint64_t mask   = ctx->nodes_table_width - 1;

        uint32_t hash = (uint32_t) fmix64(val.word);
        uint64_t i = hash;

        for (;; i++) {
                i &= mask;

                NodeEntry *cur = (ctx->nodes_table + i);

                uint64_t word = cur->val.word;

                if (word == target) {
                        debugf("\t\tTREE MATCH\n\t\t\tt%d = ", cur->pointer.ix);
                        if (DEBUG) print_tree(ctx, cur->pointer);
                        debugs("\n");

                        ctx->refcounts[cur->pointer.ix]++;

                        return cur->pointer;
                }

                if (word == UINT64_MAX) {
                        treenode_t pointer = alloc_treenode(ctx, val);

                        cur->val     = val;
                        cur->hash    = hash;
                        cur->pointer = pointer;

                        ctx->nodes_table_count++;

                        return pointer;
                }
        }
}


// Serializing /////////////////////////////////////////////////////////////////

// 127        -> 0b01111111
// 128        -> 0b10000001_10000000
// 255        -> 0b10000001_11111111
// 256        -> 0b10000001_00000000_00000001
// UINT64_MAX -> 0b10001000_11111111_(x8)
static uint64_t word_dumpsize(uint64_t word, int wid) {
        if (wid == 0 && word == 0) {
                return 1;
	}
        if (wid == 1 && word > 0 && word < 128) {
                return 1;
	}
        // Words are never big enough to require length-of-length
        return 1 + wid;
}

static void word_dump(uint8_t **ptr, int wid, uint64_t word) {
        uint8_t *buf = *ptr;
        if (wid == 0 && word == 0) {
                debugs("\tword_dump: empty (==0)\n");
                uint8_t byt = 0;
                *buf++ = byt;
	} else if (wid == 1 && word < 128 && word > 0) {
                debugf("\tword_dump: byte = %lu (wid=%d)\n", word, wid);
                *buf++ = (uint8_t) word;
        } else {
                uint8_t width = wid;
                uint8_t tagged_width = width | 0b10000000;

                debugf("\tword_dump: width = %u\n", width);
                debugf("\tword_dump: tagged_width = %u\n", tagged_width);

                *buf++ = tagged_width;

                // Little-endian architecture only.
                memcpy(buf, &word, width);

                buf += width;
        }

        *ptr = buf;
}

// width=63  -> 0b10111111_xxxxxxxx(x63)
// width=64  -> 0b11000001_01000000_xxxxxxxx*63
// width=64  -> 0b11000001_01000000_xxxxxxxx*64
// width=255 -> 0b11000001_11111111_xxxxxxxx*255
// width=256 -> 0b11000010_00000000_000000001_xxxxxxxx(*256)
static uint64_t leaf_dumpsize(leaf_t leaf) {
        if (leaf.width_bytes < 9) {
                // debugf("\tDIRECT (word=%lu)\n", (uint64_t) leaf.bytes);
                return word_dumpsize((uint64_t) leaf.bytes, leaf.width_bytes);
        }

        // single-byte length field
        if (leaf.width_bytes < 64) {
                // debugs("\tSHORT\n");
                return 1 + leaf.width_bytes;
        }

        // variable-byte length field
        // debugf("\tLONG %d %u\n", leaf.width_bytes, word64_bytes(leaf.width_bytes));
        return 1 + word64_bytes(leaf.width_bytes) + leaf.width_bytes;
}

static uint8_t *dump_leaf(uint8_t *buf, leaf_t leaf) {
        uint64_t wid = leaf.width_bytes;

        if (wid < 9) {
                debugs("\tdump direct\n");
                word_dump(&buf, wid, (uint64_t) leaf.bytes);
                return buf;
        }

        if (wid < 64) {
                debugf("\tdump small [wid=%lu]\n", wid);
                uint8_t wid_byte = wid;
                uint8_t wid_tagged = (wid_byte | 128); // 0b10xxxxxx
                debugf("\t\twidth_tagged = %u\n", wid_tagged);
                *buf++ = wid_tagged;
                memcpy(buf, leaf.bytes, wid);
                buf += wid;
                return buf;
        }

        {
                debugs("\tdump big\n");
                uint8_t widwid = word64_bytes(wid);

                // Tagged Length-of-Length
                *buf++ = (widwid | 192); // 0b11xxxxxx

                // Length (Taking advantage of Little-endian architecture)
                memcpy(buf, &wid, widwid);
                buf += widwid;

                // Actual data.
                memcpy(buf, leaf.bytes, wid);
                buf += wid;
                return buf;
        }
}

static void print_bar(Jelly, bar_t);
static void print_nat(Jelly, nat_t);
static void print_fragment_outline(Jelly, frag_t);

static void showbits(char *key, int wid, int num, uint64_t bits) {
        if (!DEBUG) return;
        putchar('\t');
        putchar('\t');
        if (key) debugf("%s:", key);
        putchar('\t');

        putchar('(');

        int extra = wid - num;
        for (int i=0; i<extra; i++) putchar('_');

        for (num--; num >= 0; num--) {
                putchar(((bits>>num)&1) ? '1' : '0');
        }
        putchar(')');
        putchar('\n');
}

struct frag_state {
        uint64_t acc;
        uint64_t fil;
        uint64_t *out;
        treenode_t *stack;
        int offs[4];
        int refbits;
};

static void
serialize_frag(Jelly ctx, struct frag_state *st, FragVal frag) {
        treenode_t *stack = st->stack;

        uint64_t fil  = st->fil;
        uint64_t acc  = st->acc;
        uint64_t *out = st->out;
        int *offs     = st->offs;
        int refbits   = st->refbits;

        int sp = 0;

        stack[sp++] = frag.tail;
        stack[sp]   = frag.head;

        int parens = 1;

        while (sp >= 0) {

                treenode_t t = stack[sp];

                treenode_value val = ctx->treenodes[t.ix];

                if (val.word >> 63) { // is a leaf
                        debugs("\n");
                        for (int x=0; x<sp; x++) { debugs(" "); }
                        if (DEBUG) print_tree_outline(ctx, t);
                        //debugs("\n");

                        debugs("\n");

                        showbits(NULL, 64, 64, acc);
                        showbits(NULL, 64, fil, acc);

                        // Output a zero bit
                        fil = (fil + 1) % 64;
                        if (!fil) { showbits("flush", 64, 64, acc); *out++ = acc; acc = 0; }

                        showbits("tag", 64, fil, acc);

                        // Important that we truncate the high bits here.
                        uint32_t lef = (uint32_t) val.word;

                        uint64_t tag = ((val.word << 1) >> 62);
                        uint64_t bits = lef + offs[tag];

                        uint64_t new_bits = (bits << fil);

                        uint64_t overflow = (bits >> (64 - fil));

                        showbits("OVO", 64, 64, overflow);
                        debugf("\t\t\t\t[fil=%lu]\n", fil);
                        debugf("\t\t\t\t[bits=%lu]\n", bits);
                        debugf("\t\t\t\t[tag=%lu]\n", tag);
                        debugf("\t\t\t\t[off=%d]\n", offs[tag]);

                        // debugf(" [extra=%u]", extra);
                        showbits("v", refbits, refbits, bits);
                        // wbits("newb", 64, 64, new_bits);

                        acc |= new_bits;
                        fil += refbits;

                        if (fil >= 64) {
                                showbits("flush", 64, 64, acc);
                                *out++ = acc;
                                acc = overflow;
                                fil -= 64;
                        }

                        showbits(NULL, 64, fil, acc);
                        showbits(NULL, 64, 64, acc);

                        debugs("\n");
                        sp--;
                } else { // is a node
                        debugs("\n");
                        for (int x=0; x<sp; x++) debugs(" ");
                        debugs("#");
                        parens++;

                        debugs("\n");
                        showbits(NULL, 64, 64, acc);
                        showbits(NULL, 64, fil, acc);

                        // Output a 1 bit.
                        acc |= (1ULL << fil);
                        showbits("tag", 64, (fil+1), acc);
                        fil = (fil + 1) % 64;
                        if (!fil) { showbits("flush", 64, 64, acc); *out++ = acc; acc = 0; }

                        showbits(NULL, 64, fil, acc);
                        showbits(NULL, 64, 64,  acc);

                        // Replaces the current stack pointer.
                        stack[sp++] = NODEVAL_TAIL(val);
                        stack[sp]   = NODEVAL_HEAD(val);
                }
        }

        st->fil = fil;
        st->acc = acc;
        st->out = out;
}

size_t jelly_head_size(Jelly ctx) {
        size_t numpins = ctx->pins_count;
        return 8 + (32*numpins);
}

size_t jelly_body_size(Jelly ctx) {
        uint64_t width = 0;

        uint32_t numpins  = ctx->pins_count;
        uint32_t numbars  = ctx->bars_count;
        uint32_t numnats  = ctx->nats_count;
        uint32_t numfrags = ctx->frags_count;

        uint64_t refrs = numpins;

        refrs += numbars;
        width += word_dumpsize(numbars, word64_bytes(numbars));
        debugf("buffer_bytes (bar_width) = %lu\n", width);
        for (int i=0; i<numbars; i++) {
                width += leaf_dumpsize(ctx->bars[i]);
                debugf("buffer_bytes (after b%d) = %lu\n", i, width);
                debugf("\n\tb%d = ", i);
                if (DEBUG) print_bar(ctx, (bar_t){ .ix = i });
                debugs("\n\n");
        }

        refrs += numnats;
        width += word_dumpsize(numnats, word64_bytes(numnats));
        debugf("buffer_bytes (after nats count) = %lu\n", width);
        for (int i=0; i<numnats; i++) {
                width += leaf_dumpsize(ctx->nats[i]);
                debugf("buffer_bytes (after n%d) = %lu\n", i, width);
                debugf("\n\tn%d = ", i);
                if (DEBUG) print_nat(ctx, (nat_t){ .ix = i });
                debugs("\n\n");
        }

        width += word_dumpsize(ctx->frags_count, word64_bytes(ctx->frags_count));
        debugf("buffer_bytes (after frags count) = %lu\n", width);

        uint64_t treebits = 0;

        for (int i=0; i<numfrags;  i++) {
                uint32_t maxref     = refrs - 1;
                uint32_t leaf_width = word64_bits(maxref);
                uint32_t leaves     = ctx->frags[i].leaves;
                uint32_t frag_bits  = (leaves*leaf_width) + (leaves * 2) - 2;
                    // (... - 2) because we can omit the outermost
                    // one bit.  Every fragment is a pair, so the
                    // parser just reads two forms per fragment:
                    // (head, tail).

                treebits += frag_bits;
                refrs++;

                debugf("tree_bits (frags) = %lu\n", treebits);
                debugf("\n\t[maxref=%u leafwid=%u leaves=%u bits=%u]\n", maxref, leaf_width, leaves, frag_bits);
                debugf("\n\t\tf%d = ", i);
                if (DEBUG) print_fragment_outline(ctx, (frag_t){i});
                debugs("\n\n");
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

        debugf("padded byte width = %lu\n", width);

        return width;
}

void jelly_save_head(Jelly ctx, size_t width, uint8_t *buf) {
        uint32_t numpins  = ctx->pins_count;

        debugf("numpins = %u\n", numpins);

        ((uint64_t*)buf)[0] = (uint64_t) numpins;

        buf += 8;

        for (int i=0; i<numpins; i++) {
                debugf("p%d\n", i);
                hash256_t *pin = ctx->pins[i];
                memcpy(buf, pin, 32);
                buf += 32;
        }
}

void jelly_save_body(Jelly ctx, size_t width, uint8_t *top) {
        uint32_t numpins  = ctx->pins_count;
        uint32_t numbars  = ctx->bars_count;
        uint32_t numnats  = ctx->nats_count;
        uint32_t numfrags = ctx->frags_count;

        // Dumping

        uint8_t *out = (uint8_t*) top;

        // Dumping Leaves

        debugf("numbars = %u\n", numbars);
        word_dump(&out, word64_bytes(numbars), numbars);
        for (int i=0; i<numbars; i++) {
                debugf("b%d\n", i);
                out = dump_leaf(out, ctx->bars[i]);
        }

        debugf("numnats = %u\n", numnats);
        word_dump(&out, word64_bytes(numnats), numnats);
        for (int i=0; i<numnats; i++) {
                debugf("n%d\n", i);
                out = dump_leaf(out, ctx->nats[i]);
        }

        debugf("numfrags = %u\n", numfrags);
        word_dump(&out, word64_bytes(numfrags), numfrags);

        if (!numfrags) {
                // TODO Validate that we have filled the buffer.
                return;
	}

        struct frag_state st;

        // treenode_t and refcounts are both 32 bits, and there is one
        // refcount for every node in the whole tree.  The stack only
        // needs to be as big as the depth of the deepest fragment, so
        // `refcounts` is plenty big.
        //
        // At this stage, we have already shattered the tree, so it's
        // fine to canibalize memory like this.
        //
        // Should, however, document that a Jelly context is "used up"
        // after serialization, and can only be freed or wiped after that.
        st.stack = (void*) ctx->refcounts;

        // If we are not in word-aligned state, we need to rewind to
        // the last word-aligned byte.
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

        debugf("\tfil = %lu\n", st.fil);
        debugs("       "); showbits("acc", 64, 64, st.acc); debugs("\n");
        debugs("       "); showbits("mor", 64, st.fil, st.acc); debugs("\n");

        for (int i=0; i<numfrags; i++) {
                int pinoff  = 0;
                int baroff  = pinoff + numpins;
                int natoff  = baroff + numbars;
                int frgoff  = natoff + numnats;
                int maxref  = frgoff + i - 1;
                int refbits = word64_bits(maxref);

                debugf("\trefbits = %d\n", refbits);
                debugf("\nFRAG(%d) [maxref=%d refbits=%d]\n\n", i, maxref, refbits);

                st.offs[0] = pinoff;
                st.offs[1] = baroff;
                st.offs[2] = natoff;
                st.offs[3] = frgoff;
                st.refbits = refbits;

                serialize_frag(ctx, &st, ctx->frags[i]);
        }

        if (st.fil > 0) {
                debugs("\n");
                showbits("final_flush", 64, 64, st.acc);
                debugs("\n\n");
                *(st.out) = st.acc;
                st.out++;
        }

       uint8_t *end = (uint8_t*) st.out;
       if (end - top != width) {
               die("bytes_read=%lu != width=%lu\n", (end-top), width);
       }
}


static INLINE uint8_t load_byte(struct ser *st) {
    debugf("\tload_byte() [remain=%lu]\n", st->wid);

    debugs("\t    [");
    for (int i=0; i<10; i++) debugf("%u%s", (uint8_t) st->buf[i], (i==9 ? "" : " "));
    debugs("]\n");

    if (st->wid == 0) die("load_byte(): EOF");
    uint8_t byte = *(st->buf++);
    st->wid--;
    debugf("\t  -> %u [remain=%lu]\n", byte, st->wid);
    return byte;
}


static uint64_t load_word(struct ser *st) {
    uint8_t byt = load_byte(st);

    // If the high-bit is zero, this is a direct byte.
    if (byt < 128) {
            debugf("\t\tload_byte = %u\n", byt);
            return (uint64_t) byt;
    }

    // The low six bits indicates the length, either the length
    // byte-array, or the length of the length of the byte-array.
    uint8_t len = byt & 0b00111111;

    // What about the second-highest bit?  If that bit is set, then `len`
    // is the length of the length-word, otherwise it indicates the
    // number of bytes used to render the word itself.
    uint8_t flag = byt & 0b01000000;

    debugf("\tlen=%u byt=%u flag=%u\n", len, byt, flag);

    if (flag) {
            die("load_word(): Number has a length-of-length, "
                "which is impossible for something that fits "
                "in 8 bytes\n");
    }

    if (len > 8) {
            die("load_word(): length-byte is too big (len=%u)", len);
    }

    if (st->wid < len) {
            die("load_word(): EOF after length byte");
    }

    debugf("\tlen=%u\n", len);

    uint64_t result = 0;

    for (int i=0; i<len; i++) {
            debugf("\tresult = %lu\n", result);
            uint8_t byte = *(st->buf++);
            uint64_t new = byte;
            new <<= 8 * i;
            result |= new;
    }

    debugf("\tresult = %lu\n", result);

    st->wid -= len;

    return result;
}

static INLINE leaf_t load_leaf(struct ser *st) {
        debugs("load_leaf:\n");

        leaf_t result;

        uint8_t byt = load_byte(st);

        if (byt < 128) {
                debugf("\tSingle-byte leaf: %u\n", byt);
                uint64_t word = byt;
                return (leaf_t) {
                        .width_bytes = (word == 0 ? 0 : 1),
                        .bytes       = (uint8_t*) word,
                };
        }

        uint8_t flag = byt & (1<<6);
        uint8_t len = byt & ((1<<6)-1);

        debugf("\tflag=%u len=%u\n", flag, len);

        if (st->wid < len) {
                die("load_word(): EOF after length byte");
        }

        // Packed Leaf
        if (!flag && len < 9) {

                debugs("\tpacked\n");

                uint64_t word = 0;

                for (int i=0; i<len; i++) {
                        uint8_t new_byte = *(st->buf++);
                        uint64_t new = new_byte;
                        debugf("\t\tword = %lu, new= %lu\n", word, new);
                        word |= (new << (8*i));
                }

                debugf("\t\tword = %lu\n", word);

                st->wid -= len;

                result.bytes = (uint8_t*) word;
                result.width_bytes = len;
                return result;
        }

        // Short indirect leaf, byte indicates length.
        if (!flag) {
                debugs("\tshort indirect\n");
                result.bytes = (uint8_t*) st->buf;
                result.width_bytes = len;

                st->wid -= len;
                st->buf += len;

                return result;
        }

        debugs("\tlong indirect\n");

        if (len > 8) die("impossibly big length-of-length\n");

        uint64_t actual_length = 0;

        for (int i=0; i<len; i++) {
                uint64_t new = *(st->buf++);
                actual_length |= (new << (8*i));
        }
        st->wid -= len;

        if (st->wid < actual_length) {
                die("load_word(): EOF after variable-width length\n");
        }

        result.bytes = (uint8_t*) st->buf;
        result.width_bytes = actual_length;

        st->wid -= actual_length;
        st->buf += actual_length;

        return result;
}

struct frag_loader_state {
        Jelly ctx;
        uint64_t *ptr; // Pointer into the remaining words.
        int rem;       // Remaining words in the buffer.
        uint64_t acc;  // The last word read from the buffer.
        uint64_t red;  // The number of bits of `acc` that have been consumed.

        uint64_t ref_bits; // The number of bits per leaf
        uint64_t max_ref;  // The maximum ref that we can accept;
        uint64_t num_pins;
        uint64_t num_bars;
        uint64_t num_nats;
};

struct load_fragtree_result {
        treenode_t tree;
        uint32_t leaves;
};

static struct load_fragtree_result
load_fragtree(struct frag_loader_state *s);

static FragVal load_fragment(struct frag_loader_state *s) {
        struct load_fragtree_result hed = load_fragtree(s);
        struct load_fragtree_result tel = load_fragtree(s);

        uint32_t leaves = hed.leaves + tel.leaves;

        FragVal fv = { .head=hed.tree, .tail=tel.tree, .leaves=leaves };
        return fv;
}

// TODO: Can this be faster?  A for loop over an `offs` table?
static treenode_value decode_leaf(struct frag_loader_state *s, uint32_t leaf) {
        if (leaf < s->num_pins) {
                return TAG_PIN((pin_t){leaf});
        }
        leaf -= s->num_pins;

        if (leaf < s->num_bars) {
                return TAG_BAR((bar_t){leaf});
        }
        leaf -= s->num_bars;

        if (leaf < s->num_nats) {
                return TAG_NAT((nat_t){leaf});
        }
        leaf -= s->num_nats;

        return TAG_FRAG((frag_t){leaf});
}

static struct load_fragtree_result
load_fragtree(struct frag_loader_state *s) {
        int refbits = s->ref_bits;

        showbits("mor", 64, (64-s->red), (s->acc >> s->red)); debugs("\n");

        uint64_t bit = (s->acc >> s->red) & 1;

        s->red = (s->red + 1) % 64;

        showbits("bit", 1, 1, bit); debugs("\n");
        showbits("mor", 64, (64-s->red), (s->acc >> s->red)); debugs("\n");

        if (!s->red) {
                if (!s->rem) die("Internal error: not enough space\n");
                s->rem--;
                s->acc = *(s->ptr)++;
        }

        showbits("mor", 64, (64-s->red), (s->acc >> s->red)); debugs("\n");

        if (bit) {
                debugs("cell\n");
                FragVal res = load_fragment(s);
                treenode_t tr = alloc_treenode(s->ctx, TAG_PAIR(res.head, res.tail));
                return (struct load_fragtree_result){ .tree=tr, .leaves=res.leaves };
        }

        debugs("leaf\n");
        debugf("refbits=%u\n", refbits);

        //      -   Read n bits.
        //      -   n is the bit-width of the maximum backref at this point.
        uint64_t leaf_mask = (1 << refbits) - 1;

        uint64_t leaf = (s->acc >> s->red) & leaf_mask;

        debugf("acc=%lu red=%lu | mask=%lu leaf=%lu\n", s->acc, s->red, leaf_mask, leaf);

        int oldred = s->red;
        debugf("[[refbits=%d oldred=%d]]\n", refbits, oldred);

        s->red += refbits;

        if (s->red >= 64) {
                int extra   = (oldred + refbits) - 64;
                int remain  = 64-extra;
                int already = refbits - extra;

                uint64_t nex = s->ptr[0];

                uint64_t why = nex & ((1<<extra) - 1);
                uint64_t more = why << already;

                debugf("[[nex=%lu extra=%d remain=%d already=%d more=%lu why=%lu]]\n", nex, extra, remain, already, more, why);

                leaf |= more;

                s->red -= 64;
                s->acc = nex;
                s->rem--;
                s->ptr++;
        }

        if (leaf > s->max_ref) {
                die("leaf val is out-of-bounds (%lu)\n", leaf);
        }

        treenode_value v = decode_leaf(s, leaf);

        treenode_t t = alloc_treenode(s->ctx, v);

        return (struct load_fragtree_result){ .tree = t, .leaves = 0 };
}

void jelly_load_head(Jelly ctx, size_t wid, uint8_t *buf) {
        if (wid < 8) {
                die("jelly head is not big enough to hold a length (%lu < 8)\n", wid);
	}

	uint64_t num_pins = *((uint64_t*) buf);

	uint64_t buf_size = 8 + (num_pins * 32);

	if (wid != buf_size) {
	        die("jelly head has wrong size: (width:%lu != expected:%lu)\n", wid, buf_size);
	}

        for (int i=0; i<num_pins; i++) {
                uint32_t pix = alloc_pin(ctx).ix;
                ctx->pins[pix] = (hash256_t*) buf;
                buf += 32;
        }
}


/*
        Note that for pins, indirect atoms, and indirect bars we do not
        copy, we just slice the input buffer.
*/
void jelly_load_body(Jelly ctx, size_t wid, uint8_t *top) {
        struct ser st = { .buf = top, .wid = wid };

        if (st.wid % 8) {
                die("Input buffer must contain a multiple of 8 bytes.\n");
        }

        debugs("\n");

        uint64_t num_pins = ctx->pins_count;
        debugf("num_pins = %lu\n", num_pins);

        uint64_t num_bars = load_word(&st);
        debugf("num_bars = %lu\n", num_bars);
        for (int i=0; i<num_bars; i++) {
                uint32_t bix = alloc_bar(ctx).ix;
                ctx->bars[bix] = load_leaf(&st);
        }

        uint64_t num_nats = load_word(&st);

        debugf("num_nats = %lu\n", num_nats);
        for (int i=0; i<num_nats; i++) {
                uint32_t nix = alloc_nat(ctx).ix;
                ctx->nats[nix] = load_leaf(&st);
        }

        uint64_t num_frags = load_word(&st);
        debugf("num_nats = %lu\n", num_nats);

        debugf("num_nats = %lu\n", num_nats);

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

                debugf("\tst.wid = %lu\n", st.wid);
                debugf("\tst.buf = 0x%lx\n", (uint64_t) st.buf);

                red = clif * 8;
                ptr = (uint64_t*) st.buf;
                acc = *ptr++;
                rem = (st.wid / 8) - 1;

                struct frag_loader_state s = {
                        .ctx = ctx,
                        .ptr = ptr,
                        .acc = acc,
                        .red = red,
                        .rem = rem,
                        .ref_bits = 0,
                        .max_ref = 0,
                        .num_pins = num_pins,
                        .num_bars = num_bars,
                        .num_nats = num_nats,
                };

                for (int i=0; i<num_frags; i++) {
                        int num_refs = num_pins + num_bars + num_nats + i;
                        s.max_ref  = num_refs - 1;
                        s.ref_bits = word64_bits(s.max_ref);

                        debugs("\t[frag_loader_state]\n");
                        debugf("\tptr = 0x%016lx\n", (uint64_t) s.ptr);

                        debugs("       "); showbits("acc", 64, 64, s.acc); debugs("\n");
                        debugs("       "); showbits("mor", 64, (64-s.red), (s.acc >> s.red)); debugs("\n");
                        debugf("\tred = %lu\n", s.red);
                        debugf("\trem = %d\n", s.rem);
                        debugf("\tref_bits = %lu\n", s.ref_bits);
                        debugf("\tmax_ref = %lu\n", s.max_ref);
                        debugf("\tnum_pins = %lu\n", s.num_pins);
                        debugf("\tnum_bars = %lu\n", s.num_bars);
                        debugf("\tnum_nats = %lu\n", s.num_nats);

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
                uint32_t num_leaves = num_pins + num_bars + num_nats;

                if (num_leaves == 0) {
                        die("No pins and no leaves.\n");
                }

                if (num_leaves > 1) {
                        die("No frags, but multiple leaves.\n");
                }

                treenode_value v;
                if (num_pins) { v = TAG_PIN((pin_t){0}); }
                else if (num_bars) { v = TAG_BAR((bar_t){0}); }
                else { v = TAG_NAT((nat_t){0}); }
                alloc_treenode(ctx, v);

                if (st.wid != 0) {
                        debugf("EXTRA STUFF %lu bytes unread!\n", st.wid);
                }
        }
}



// Testing /////////////////////////////////////////////////////////////////////

static void print_nat(Jelly ctx, nat_t nat) {
        leaf_t l = ctx->nats[nat.ix];
        if (l.width_bytes > 8) {
                printf("0x");
                for (int i = l.width_bytes - 1; i>=0; i--) {
                        uint8_t byte = l.bytes[i];
                        printf("%02x", (unsigned) byte);
                }
        } else {
                uint64_t value = (uint64_t) l.bytes;
                printf("%lu", value);
        }
}

static void print_bar(Jelly ctx, bar_t bar) {
        leaf_t l = ctx->bars[bar.ix];
        uint8_t *buf = NULL;

        if (l.width_bytes > 8) {
                buf = l.bytes;
        } else {
                buf = (uint8_t*) &(l.bytes);
        }

        printf("\"");
        for (int i = 0; i < l.width_bytes; i++) {
                uint8_t byte = buf[i];
                printf("%c", byte);
        }
        printf("\"");
}


static size_t print_pin(Jelly ctx, pin_t pin) {
        char b58[96];
        size_t b58sz = 96;

        bool okay = b58enc(b58, &b58sz, (void*)ctx->pins[pin.ix], 32);

        if (!okay) die("print_pin(): b58enc failed!\n");

        putchar('[');
        fwrite(b58, 1, b58sz, stdout);
        putchar(']');
        return b58sz;
}

static void print_tree_outline(Jelly, treenode_t);
static void print_fragment_outline(Jelly, frag_t);
static void print_fragment(Jelly, frag_t);
static void print_tree(Jelly, treenode_t);

static void print_tree_outline_list(Jelly ctx, treenode_t tree) {
        treenode_value val = ctx->treenodes[tree.ix];

        uint32_t offset = (uint32_t) val.word;

        switch (NODEVAL_TAG(val)) {
            case 4: printf("p%u", offset); break;
            case 5: printf("b%u", offset); break;
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

static void print_tree_outline(Jelly ctx, treenode_t tree) {
        treenode_value val = ctx->treenodes[tree.ix];

        uint32_t offset = (uint32_t) val.word;

        switch (NODEVAL_TAG(val)) {
            case 4: printf("p%u", offset); break;
            case 5: printf("b%u", offset); break;
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

static void print_tree_list(Jelly ctx, treenode_t tree) {
        treenode_value val = ctx->treenodes[tree.ix];

        switch (NODEVAL_TAG(val)) {
            case 4: print_pin(ctx, NODEVAL_PIN(val)); break;
            case 5: print_bar(ctx, NODEVAL_BAR(val)); break;
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

static void print_tree(Jelly ctx, treenode_t tree) {
        treenode_value val = ctx->treenodes[tree.ix];

        switch (NODEVAL_TAG(val)) {
            case 4: print_pin(ctx, NODEVAL_PIN(val)); break;
            case 5: print_bar(ctx, NODEVAL_BAR(val)); break;
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

static void print_fragment_outline(Jelly ctx, frag_t ref) {
        FragVal frag = ctx->frags[ref.ix];
        putchar('(');
        print_tree_outline_list(ctx, frag.head);
        putchar(' ');
        print_tree_outline(ctx, frag.tail);
        putchar(')');
}

static void print_fragment(Jelly ctx, frag_t ref) {
        FragVal frag = ctx->frags[ref.ix];
        putchar('(');
        print_tree_list(ctx, frag.head);
        putchar(' ');
        print_tree(ctx, frag.tail);
        putchar(')');
}


static void jelly_debug_leaves(Jelly ctx, bool details) {
        printf("\n\tleaves: (width=%u, count=%u)\n\n",
               ctx->leaves_table_width,
               ctx->leaves_table_count);

        int wid = ctx->leaves_table_width;

        uint64_t mask = ctx->leaves_table_width - 1;

        for (int i=0; i<wid; i++) {
                LeafEntry ent = ctx->leaves_table[i];

                // Empty slot
                if (ent.pointer.ix == UINT32_MAX) continue;

                printf("\t\t%4d = ", i);
                print_tree_outline(ctx, ent.pointer);
                printf("\t(val=");
                print_tree(ctx, ent.pointer);
                printf(")\n");

                if (details) {
                        uint64_t width = ((ent.width.word << 2) >> 2);
                        uint64_t bin = ent.hash & mask;
                        uint64_t distance = (((uint64_t)i) - bin);
                        printf("\t\t    bytes: %-4lu\n", width);
                        printf("\t\t    bin: %lu [dist=%lu]\n", bin, distance);
                        printf("\t\t    hash: 0x%016lx\n", ent.hash);
                }
        }
}


static void jelly_debug_interior_nodes(Jelly ctx) {
        printf("\n\tinterior_nodes_table: (width=%u, count=%u)\n\n",
               ctx->nodes_table_width,
               ctx->nodes_table_count);

        int wid = ctx->nodes_table_width;

        uint64_t mask = ctx->nodes_table_width - 1;

        for (int i=0; i<wid; i++) {
                NodeEntry ent = ctx->nodes_table[i];

                // Empty slot
                if (ent.val.word == UINT64_MAX) continue;

                uint64_t bin = ent.hash & mask;
                uint64_t distance = (((uint64_t)i) - bin);

                printf("\t\t%4d = t%u\tbin=%lu\tdist=%lu\thash=%08x\n", i, ent.pointer.ix, bin, distance, ent.hash);

                // printf("\n\n\t\t  ");
                // print_tree(ctx, ent.pointer);
                // print_tree_outline(ctx, ent.pointer);
                // printf("\n\n");
        }
}


void jelly_dbug(Jelly ctx) {
        //if (!DEBUG) return;

        printf("\njelly_debug():\n");

        jelly_debug_leaves(ctx, false);

        jelly_debug_interior_nodes(ctx);

        {
                printf("\n\tpins: (width=%u, count=%u)\n\n",
                       ctx->pins_width,
                       ctx->pins_count);
                int num = ctx->pins_count;
                for (int i=0; i<num; i++) {
                        printf("\t\tp%d = ", i);
                        print_pin(ctx, (pin_t){ .ix = i });
                        printf("\n");
                }
        }


        {
                printf("\n\tbars: (width=%u, count=%u)\n\n",
                       ctx->bars_width,
                       ctx->bars_count);
                int num = ctx->bars_count;
                for (int i=0; i<num; i++) {
                        printf("\t\tb%d = ", i);
                        print_bar(ctx, (bar_t){ .ix = i });
                        printf("\n");
                }
        }


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
                        switch (val.word >> 61) {
                            case 4:
                                printf("\t\tt%d = p%u\t\trefs=%u\n", i, (uint32_t) val.word, ref);
                                continue;
                            case 5:
                                printf("\t\tt%d = b%u\t\trefs=%u\n", i, (uint32_t) val.word, ref);
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
                                printf("\t\tt%d = (%u, %u)\trefs=%u\n", i, hed.ix, tel.ix, ref);
                                continue;
                            }
                        }
                }
        }

        printf("\nJelly Fragments:\n\n");

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

void jelly_show(Jelly ctx) {
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
