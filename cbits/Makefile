# CFLAGS="-g -U_FORTIFY_SOURCE -O0"
CFLAGS= -O3 -Wall -Werror

all: o.test_seed

test: all
	./o.test_seed <test_input.sexp

o.test_seed: \
        seed.o test_seed.o \
        xxhash.o xxh_x86dispatch.o \
        base58.o \
        jets.o \
        blake3.o blake3_dispatch.o blake3_portable.o blake3_sse2_x86-64_unix.o \
        blake3_sse41_x86-64_unix.o \
        blake3_avx2_x86-64_unix.o \
        blake3_avx512_x86-64_unix.o
	gcc ${CFLAGS} -lgmp $^ -o $@

blake3.o: blake3.c
	gcc ${CFLAGS} -c $<

blake3_dispatch.o: blake3_dispatch.c
	gcc ${CFLAGS} -c $<

blake3_portable.o: blake3_portable.c
	gcc ${CFLAGS} -c $<

blake3_sse2_x86-64_unix.o: blake3_sse2_x86-64_unix.S
	gcc ${CFLAGS} -c $<

blake3_sse41_x86-64_unix.o: blake3_sse41_x86-64_unix.S
	gcc ${CFLAGS} -c $<

blake3_avx2_x86-64_unix.o: blake3_avx2_x86-64_unix.S
	gcc ${CFLAGS} -c $<

blake3_avx512_x86-64_unix.o: blake3_avx512_x86-64_unix.S
	gcc ${CFLAGS} -c $<

test_seed.o: test_seed.c seed.h
	gcc ${CFLAGS} -c $<

seed.o: seed.c libbase58.h xxhash.h seed.h
	gcc ${CFLAGS} -c $<

jets.o: jets.c blake3.h
	gcc ${CFLAGS} -c $<

base58.o: base58.c libbase58.h
	gcc ${CFLAGS} -c $<

xxhash.o: xxhash.c xxhash.h
	gcc ${CFLAGS} -c $<

xxh_x86dispatch.o: xxh_x86dispatch.c xxhash.h
	gcc ${CFLAGS} -c $<

clean:
	rm -f *.o o.* test.tmp test.tmp?
