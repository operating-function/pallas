#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "seed.h"
#include "libbase58.h"

#ifdef DEBUG
#undef DEBUG
#endif
#define DEBUG 1

#define NUM_PINS 3

// Forward Declarations ////////////////////////////////////////////////////////

treenode_t read_one(Seed);
treenode_t read_many(Seed);


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
treenode_t read_hex(Seed ctx) {
        uint8_t *hex_digits = malloc(2048);
        memset(hex_digits, 0, 2048);
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
            return seed_word(ctx, 0);
        }

        int byt_wid = (use/2) + (use%2);

        uint8_t *bytes = calloc(1024,1);

        uint8_t *bptr = &(bytes[byt_wid - 1]);

        // {3, 4, 5}

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

        uint64_t words_used = (byt_wid / 8) + ((byt_wid % 8) ? 1 : 0);

        return seed_nat(ctx, words_used, (uint64_t*) bytes);
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

treenode_t read_string(Seed ctx) {
        char *buf = calloc(1, 1024);
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

        debugf("\tread_string() -> \"%s\" (%lu)\n", buf, count);

        uint8_t *bytes = calloc(count+1, 1);
        memcpy(bytes, buf, count);

        debugf("\t\t(width=%lu)\n", count);
        free(buf);
        treenode_t res = seed_barnat(ctx, count, bytes);
        printf("result: ");
        print_tree_pub(ctx, res);
        printf("\n:");
        return res;
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

treenode_t read_leaf(Seed ctx) {
        int c = getchar();

        switch (c) {
            case '[': {
                c = getchar();
                if (!isdigit(c)) die("pinref is not a digit\n");
                int pin = (c - '0');
                c = getchar();
                if (c != ']') die("pinref not closed\n");
                if (pin >= NUM_PINS) die("Invalid pin reference (too big)\n");
                return (treenode_t){ .ix = pin };
            }
            case '"': {
                // TODO Don't pack
                return read_string(ctx);
            }
            case '0': {
                int d = getchar();
                if (d == 'x') {
                        return read_hex(ctx);
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
                return seed_word(ctx, word);
            }
            default:
                die("Not a leaf: '%c'\n", c);
        }
}

treenode_t read_many(Seed ctx) {
       int c;

       treenode_t acc = read_one(ctx);
    loop:
        eat_space();

        switch (c = getchar()) {
            case '(': {
                treenode_t list = read_many(ctx);
                acc = seed_cons(ctx, acc, list);
                goto loop;
            }
            case EOF:
            case ')':
                return acc;
            default:
                ungetc(c, stdin);
                treenode_t elmt = read_leaf(ctx);
                acc = seed_cons(ctx, acc, elmt);
                goto loop;
        }
}

treenode_t read_one(Seed ctx) {
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

char* load_file(char const* path, long *lengthOut)
{
    char* buffer = 0;
    long length = 0;
    FILE * f = fopen (path, "rb"); //was "rb"

    if (f)
    {
      fseek (f, 0, SEEK_END);
      length = ftell (f);
      fseek (f, 0, SEEK_SET);
      buffer = (char*)malloc ((length+1)*sizeof(char));
      if (buffer)
      {
        size_t res = fread (buffer, sizeof(char), length, f);

        if (res < length) {
            fprintf(stderr, "FAILED TO READ\n");
            exit(1);
        }

      }
      fclose (f);
    }

    buffer[length] = '\0';

    *lengthOut = length;

    return buffer;
}

int main (int argc, char **argv) {
        if (argc == 2) {
                long bwid = 0;
                uint8_t *bbuf = (void*) load_file(argv[1], &bwid);
                Seed ctx = seed_make();
                seed_load(ctx, bwid, bbuf);
                seed_dbug(ctx);
                seed_show(ctx);
                seed_free(ctx);
                free(bbuf);
                return 0;
        }

        Seed     ctx;
        size_t   bwid;
        uint8_t *bbuf;

        ctx = seed_make();
        for (int i=0; i<NUM_PINS; i++) seed_hole(ctx);
        read_many(ctx);

        printf("seed_done()\n");
        seed_done(ctx);

        printf("seed_debug()\n");
        seed_dbug(ctx);

        printf("seed_save()\n");
        bwid = seed_size(ctx);
        bbuf = calloc(bwid, 1);
        seed_save(ctx, bwid, bbuf);

        printf("seed_wipe()\n");
        seed_wipe(ctx);

        printf("\nseed_load()\t\t[LOAD]\n\n");
        seed_load(ctx, bwid, bbuf);

        seed_dbug(ctx);
        seed_show(ctx);

        free(bbuf);
        seed_free(ctx);
}
