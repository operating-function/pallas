#ifdef ENABLE_HARNESS
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "jelly.h"
#include "libbase58.h"


// Forward Declarations ////////////////////////////////////////////////////////

treenode_t read_one(Jelly);
treenode_t read_many(Jelly);


// Testing Harness /////////////////////////////////////////////////////////////

uint8_t hex_value(char a, char b) {
    if (b == EOF) die("partial hex literal");

    uint8_t top_byte = (isdigit(a) ? (a - '0') : (tolower(a) - ('a' - 10)));
    uint8_t low_byte = (isdigit(b) ? (b - '0') : (tolower(b) - ('a' - 10)));
    uint8_t result   = (top_byte << 4) | low_byte;

    return result;
}

uint64_t pack_bytes_msb(size_t width, char *bytes) {
        uint64_t res = 0;
        for (int i=0; i<width; i++) {
                uint64_t tmp = (uint8_t) bytes[i];
                res = (res<<8) | tmp;
        }
        return res;
}

uint64_t pack_bytes_lsb(size_t width, char *bytes) {
        debugf("\t\tpack_bytes_lsb(width=%lu, ", width);
        uint64_t res = 0;
        for (int i=0; i<width; i++) {
                uint64_t tmp = bytes[i];
                debugf("%02lx(%d)", tmp, i*8);
                res = res | (tmp << (i*8));
        }
        debugs(")\n");
        return res;
}

/*
        0x     ()    []           -> {}
        0xf    (15)  []           -> {15} <> {}
        0xff   ()    [255]        -> {255}
        0xfff  (15)  [255]        -> {15} <> {255}
        0xfffe ()    [255, 254]   -> {254, 255}

        0x       ()    []           -> {}
        0x0      (0)   []           -> {}
        0x00     ()    []           -> {}
        0x00f    (15)  []           -> {15} <> {}
        0x00ff   ()    [255]        -> {255}
        0x00ff0  (0)   [255]        -> {0} <> {255}
        0x00ff00 (00)  [255,0]      -> {0,255}
*/
leaf_t read_hex() {
        uint8_t *hex_digits = malloc(2048);
        int use = 0;
        int wid = 2048;

    getloop: {

        char c = getchar();

        uint8_t x;

        switch (c) {
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                x = (c - '0');
                break;
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
                x = 10 + (c - 'a');
                break;
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
                x = 10 + (c - 'A');
                break;
            default:
                goto end;
        }

        if (use >= wid) {
                wid *= 2;
                uint8_t *new = realloc(hex_digits, wid);
                if (new == NULL) die("realloc");
                hex_digits = new;
        }

        hex_digits[use++] = x;

        goto getloop;
    }

    end: {

        uint8_t *freeme = hex_digits;

        // Ignore leading zeros.
        while (use > 0 && !hex_digits[0]) {
                hex_digits++;
                use--;
        }

        if (use == 0) {
            free(freeme);
            return (leaf_t){ .width_bytes = 0, .bytes = 0 };
        }

        int byt_wid = (use/2) + (use%2);

        uint8_t *bytes = malloc(byt_wid);

        uint8_t *bptr = &(bytes[byt_wid - 1]);

        // If there's an odd number of hex-digits, the high-byte is just
        // the hex-digit-value of the first hex-digit.
        if (use%2) {
                *bptr = hex_digits[0];
                hex_digits++;
                use--;
                bptr--;
        }

        for (int i=0; i<use; i+=2) {
                uint8_t byte = (hex_digits[i] << 4) | hex_digits[i+1];
                *bptr-- = byte;
        }

        free(freeme);

        debugf("read_hex(): byt_wid=%d\n", byt_wid);

        return (leaf_t){ .width_bytes = byt_wid, .bytes = bytes };
    }
}

uint64_t read_word(uint64_t acc) {
        debugs("read_word()\n");
        int c;

        while (isdigit(c = getchar())) {
                acc *= 10;
                acc += (uint64_t)(c - '0');
        }

        if (c != EOF) ungetc(c, stdin);
        return acc;
}

leaf_t read_string() {
        char *buf = malloc(1024);
        size_t wid = 1024;
        int ix = 0;
    loop: {

        char c;

        switch (c = getchar()) {
            case '"':
            case EOF:
                goto end;
            case '\\': {
                char d = getchar();
                if (d == EOF) goto end;
                c = d;
                break;
            }
        }

        buf[ix++] = c;
        if (ix >= wid) { wid *= 2; buf = realloc(buf, wid); }
        goto loop;
    }

    end: {
        size_t count = ix;
        leaf_t leaf;
        leaf.width_bytes = count;

        debugf("\tread_string() -> \"%s\" (%lu)\n", buf, count);

        leaf.bytes = calloc(count+1, 1);
        memcpy(leaf.bytes, buf, count);

        debugf("\t\t(width=%d)\n", leaf.width_bytes);
        free(buf);
        return leaf;
    }
}

void eat_comment() {
    loop: {
        int c = getchar();
        switch (c) {
            case EOF:
            case '\n':
                return;
            default:
                goto loop;
        }
    }
}

void eat_space() {
    loop: {
        int c = getchar();
        switch (c) {
            case '#':
                eat_comment();
                goto loop;
            case '\n':
            case '\t':
            case ' ':
                goto loop;
            default:
                ungetc(c, stdin);
                return;
        }
    }
}

treenode_t read_leaf(Jelly ctx) {
        int c = getchar();

        switch (c) {
            case '"': {
                // TODO Don't pack
                leaf_t leaf = read_string();
                return jelly_bar(ctx, leaf.width_bytes, leaf.bytes);
            }
            case '[':
                die("TODO: Parse pins\n");
            case '0': {
                int d = getchar();
                if (d == 'x') {
                        leaf_t leaf = read_hex();
                        return jelly_nat(ctx, leaf.width_bytes, leaf.bytes);
                }
                ungetc(d, stdin);
            } // fallthrough
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9': {
                uint64_t word = read_word((uint64_t)(c - '0'));
                return jelly_word(ctx, word);
            }
            default:
                die("Not a leaf: '%c'\n", c);
        }
}

treenode_t read_many(Jelly ctx) {
       int c;

       treenode_t acc = read_one(ctx);
    loop:
        eat_space();

        switch (c = getchar()) {
            case '(': {
                treenode_t list = read_many(ctx);
                acc = jelly_cons(ctx, acc, list);
                goto loop;
            }
            case EOF:
            case ')':
                return acc;
            default:
                ungetc(c, stdin);
                treenode_t elmt = read_leaf(ctx);
                acc = jelly_cons(ctx, acc, elmt);
                goto loop;
        }
}

treenode_t read_one(Jelly ctx) {
        eat_space();
        int c = getchar();
        switch (c) {
            case '(':
                return read_many(ctx);
            case EOF:
                die("Unexpected EOF (read_one)\n");
            default:
                ungetc(c, stdin);
                return read_leaf(ctx);
        }
}



int main () {
        Jelly ctx = jelly_make();

        // read_many(ctx);

        hash256_t dumb_hash_1 = { fmix64(111111), fmix64(65535), fmix64(9),  65536 };
        hash256_t dumb_hash_2 = { fmix64(222222), fmix64(33333), (0ULL - 1), (65535ULL << 12) };

        bool uniq_1=false, uniq_2=false, uniq_3=false;

        treenode_t top = read_many(ctx);
        treenode_t tmp = jelly_pin(ctx, &uniq_1, (void*) &dumb_hash_2);
        top = jelly_cons(ctx, tmp, top);
        tmp = jelly_pin(ctx, &uniq_2, (void*) &dumb_hash_1);
        top = jelly_cons(ctx, tmp, top);

        // TODO: This breaks things for some reason.
        tmp = jelly_pin(ctx, &uniq_3, (void*) &dumb_hash_2);
        top = jelly_cons(ctx, tmp, top);

        printf("pin 1 is unique? %s\n", (uniq_1 ? "yes" : "no"));
        printf("pin 2 is unique? %s\n", (uniq_2 ? "yes" : "no"));
        printf("pin 3 is unique? %s\n", (uniq_3 ? "yes" : "no"));

        debugs("# Shattering Fragments\n");

        jelly_done(ctx);
        jelly_dbug(ctx);

        debugs("# Dumping to Buffer\n");

        size_t   hed_wid = jelly_head_size(ctx);
        uint8_t *hed_buf = calloc(hed_wid, 1);
        jelly_save_head(ctx, hed_wid, hed_buf);

        size_t   bod_wid = jelly_body_size(ctx);
        uint8_t *bod_buf = calloc(bod_wid, 1);
        jelly_save_body(ctx, bod_wid, bod_buf);

        debugs("# Wiping the Context\n");
        jelly_wipe(ctx);

        debugs("# Loading from Buffer\n");

        jelly_load_head(ctx, hed_wid, hed_buf);
        jelly_load_body(ctx, bod_wid, bod_buf);

        jelly_dbug(ctx);
        jelly_show(ctx);

        free(hed_buf);
        free(bod_buf);
        jelly_free(ctx);
}
#endif
