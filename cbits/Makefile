a.out: jelly.o test_jelly.o \
       xxhash.o xxh_x86dispatch.o \
       base58.o \
       jets.o \
       blake3.o blake3_dispatch.o blake3_portable.o blake3_sse2_x86-64_unix.o \
       blake3_sse41_x86-64_unix.o \
       blake3_avx2_x86-64_unix.o \
       blake3_avx512_x86-64_unix.o
	gcc -O3 -Werror -Wall $^

blake3.o: blake3.c
	gcc -O3 -Werror -Wall -c $<

blake3_dispatch.o: blake3_dispatch.c
	gcc -O3 -Werror -Wall -c $<

blake3_portable.o: blake3_portable.c
	gcc -O3 -Werror -Wall -c $<

blake3_sse2_x86-64_unix.o: blake3_sse2_x86-64_unix.S
	gcc -O3 -Werror -Wall -c $<

blake3_sse41_x86-64_unix.o: blake3_sse41_x86-64_unix.S
	gcc -O3 -Werror -Wall -c $<

blake3_avx2_x86-64_unix.o: blake3_avx2_x86-64_unix.S
	gcc -O3 -Werror -Wall -c $<

blake3_avx512_x86-64_unix.o: blake3_avx512_x86-64_unix.S
	gcc -O3 -Werror -Wall -c $<

test_jelly.o: test_jelly.c jelly.h
	gcc -DENABLE_HARNESS=1 -O3 -Werror -Wall -c $<

jelly.o: jelly.c libbase58.h xxhash.h jelly.h
	gcc -O3 -Werror -Wall -c $<

jets.o: jets.c blake3.h
	gcc -O3 -Werror -Wall -c $<

base58.o: base58.c libbase58.h
	gcc -O3 -Werror -Wall -c $<

xxhash.o: xxhash.c xxhash.h
	gcc -O3 -Werror -Wall -c $<

xxh_x86dispatch.o: xxh_x86dispatch.c xxhash.h
	gcc -O3 -Werror -Wall -c $<

clean:
	rm -f *.o a.out test.tmp test.tmp?
