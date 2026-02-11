// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
#include <sys/mman.h>
#include <sys/stat.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

typedef uint64_t Value;
typedef uint64_t u64;

void   mk_app    (void);
void   push_val  (Value*);
void   push_word (u64);
Value *a_Pin     (Value*);

Value *start_bignat_alloc(u64 wid);
Value *end_bignat_alloc(Value *v);

Value *pop (void);

extern Value **sp;

static inline void crash (const char *s) { puts(s),   exit(1); }
static inline void pexit (const char *s) { perror(s), exit(1); }

int ib_used;
int ob_used;

u64 *omor;
u64 *imor;
u64 iword;
u64 oword;

u64 next_word (void) {
	int remain = 64 - ib_used;
	if (remain == 64) return iword;
	return ((iword >> ib_used) | (*imor << remain));
}

u64 ibump (int bits) {
	ib_used += bits;

	while (ib_used >= 64) {
		ib_used -= 64;
		iword = *imor;
		imor++;
	}
}

int depth = 0;

void dent () {
	for (int i=0; i<depth; i++) putchar(' ');
}

void output_word (u64 leftover) {
/*
	dent();
	for (int i=0; i<64; i++) {
		if (i && i%8 == 0) putchar('.');
		u64 ick = oword;
		if (i) ick >>= i;
		printf("%01llu", (ick & 1ull));
	}
	printf("\n"); dent();
	for (int i=0; i<64; i++) {
		if (i && i%8 == 0) putchar('.');
		u64 ick = leftover;
		if (i) ick >>= i;
		printf("%01llu", (ick & 1ull));
	}
	printf("\n");
*/

	ob_used -= 64;
	*omor = oword;
	omor++;
	oword = leftover;
}

void write_bits (u64 bits, int reswid) {
	oword |= (bits << ob_used);

	ob_used += reswid;

	int hang = ob_used % 64;
	int writ = reswid - hang; // bits of bits that were
				  // already written to the
				  // first word.

	if (ob_used >= 64) {
		// depth += 4;
		// dent();
		// printf("(reswid=%d, hang=%d, writ=%d, extra=%lu)\n", reswid, hang, writ, bits>>writ);
		output_word(bits >> writ);
		// depth -= 4;
	}
}

int frag_size (int count) {
loop:
	u64 combo = next_word();
	combo = ~combo;
	if (combo == 0) { ibump(64); goto loop; }
	int ones = __builtin_ctzll(combo);
	count += ones;
	ibump(ones + 1);
	return count;
}

u64 u64_bits (u64 w) {
	if (!w) { return 0; }
	return 64 - __builtin_clzll(w);
}

void write_node (u64 sz, u64 refsz, u64 ref) {
	u64 result = ref;
	u64 reswid = refsz;

	if (sz == 0) {
		result <<= 1;
		result++;
		reswid++;
	} else {
		u64 szsz = u64_bits(sz);
		u64 mask = ((1ull << (szsz-1)) - 1);
		result <<= (szsz - 1);
		result |= (sz & mask);    // Drop the high bit!
		result <<= (szsz + 1);
		result |= (1ull << szsz); // mark the end of the size-of-size;
		reswid += szsz;
		reswid += szsz;
	}

	// printf("\n(sz=%lu, refsz=%lu, ref=%lu) =>\n\t", sz, refsz, ref);

/*
	for (int i=0; i<reswid; i++) {
		if (i && i%8 == 0) putchar('.');
		u64 ick = result;
		if (i) ick >>= i;
		printf("%01llu", (ick & 1ull));
	}
	printf("\n");
*/

	write_bits(result, reswid);
}

void frag_load (int already, u64 tabSz) {
	int sz = frag_size(already);

	u64 maxref = tabSz-1;
	u64 refsz = u64_bits(maxref);
	u64 combo = next_word();
	u64 ref = combo & ((1ULL << refsz) - 1ULL);                                  // mask

	ibump(refsz);

	dent(); printf("- %d %lu[%ld]\n", sz, ref, refsz);

	write_node(sz, refsz, (maxref - ref));

	if (!sz) return;
	depth++;
	for (int i=0; i<sz; i++) frag_load(0, tabSz);
	depth--;
}

int seed_load(u64 *inpbuf, u64 *outbuf) {
	u64 n_holes = inpbuf[0];
	u64 n_bigs  = inpbuf[1];
	u64 n_words = inpbuf[2];
	u64 n_bytes = inpbuf[3];
	u64 n_frags = inpbuf[4];

	printf("n_holes = %lu\n", n_holes);
	printf("n_bigs = %lu\n",  n_bigs);
	printf("n_words = %lu\n", n_words);
	printf("n_bytes = %lu\n", n_bytes);
	printf("n_frags = %lu\n", n_frags);

	int tabSz = 0;

	if (n_holes) {
		printf("#0\n");
		tabSz++;
	}

	// How big are the bignats?
	u64 bigwidths[n_bigs];
	for (int i=0; i<n_bigs; i++) {
		bigwidths[i] = inpbuf[5+i];
	}

	int used = 5 + n_bigs; // number of words used

	for (int i=0; i<n_bigs; i++) {
		u64 wid       = bigwidths[i];
		u64 tmp[wid];
		memcpy(tmp, inpbuf+used, wid*sizeof(u64));
		printf("bignum(%lu)\n", wid);
		for (int i=0; i<wid; i++) {
			printf("\t%lx\n", inpbuf[used+i]);
		}
		used += wid;
		tabSz++;
	}

	for (int i=0; i<n_words; i++) {
		printf("word: %lu\n", inpbuf[used++]);
		tabSz++;
	}

	{
		uint8_t *byte_buf = (void*) (inpbuf + used);
		for (int i=0; i<n_bytes; i++) {
			printf("byte: %d\n", byte_buf[i]);
			tabSz++;
		}
		used += (n_bytes / 8);
	}

	int hang = n_bytes % 8;
	int header_bytes = 8*used + hang;
	memcpy(outbuf, inpbuf, header_bytes);

	ib_used = 8*hang;
	ob_used = 8*hang;
	iword   = inpbuf[used];
	oword   = outbuf[used];
	omor    = &outbuf[used];
	imor    = &inpbuf[used+1];

	for (int i=0; i<n_frags; i++) {
		frag_load(1, tabSz);
		printf("\n\n");
		tabSz++;
	}

	if (ob_used) output_word(0);
	return (omor - outbuf);
}

void convert_seed (const char *input, const char *output) {
	int fd = open(input, O_RDONLY);
	if (fd < 0) pexit("open");

	struct stat statbuf;
	if (0 != fstat(fd, &statbuf)) pexit("fstat");

	u64 *obuf = calloc(statbuf.st_size, 8);

	u64 *ibuf = mmap(NULL, statbuf.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (ibuf == MAP_FAILED) pexit("mmap");

	if (0 != close(fd)) pexit("close");

	int owords = seed_load(ibuf, obuf);

	printf("wrote %d words\n", owords);

	FILE *of = fopen(output, "w");

	int written = fwrite(obuf, 8, owords, of);

	if (written != owords) crash("but why?");
	fclose(of);

	free(obuf);
}

int main (int argc, char **argv) {
	switch (argc) {
	case 1:
		convert_seed("oldseeds/add", "seeds/add");
		return 0;
	case 3:
		convert_seed(argv[1], argv[2]);
		return 0;
	default:
		crash("usage: renew input output");
	}
}
