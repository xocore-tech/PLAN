// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdnoreturn.h>
#include <limits.h>

typedef uint64_t u64;
typedef uint64_t Val;
typedef uint32_t u32;
typedef uint8_t  u8;

typedef void (*Exec)(Val);

extern u64 *stack_first;
extern u64 *stack_last;

extern   u64 mapd_size;
extern   u64 *heap_addr;
register u64 *heap_end  asm ("r13");
register u64 *heap_next asm ("r14");
register u64 *sp        asm ("r15");

static inline u64 *PTR (Val x) {
	return (void*) ((x << 16) >> 16);
}

static inline u64 get_bitsz (u64 x) {
	u64 *p     = PTR(x);
	u64 record = p[-1];
	u64 bitsz  = record >> 8;
	return bitsz;
}

extern const u64 nattag;
extern const u64 pintag;
extern const u64 lawtag;
extern const u64 clztag;
extern const u64 thktag;

static inline bool isdirect (u64 o) { return 0 == (o>>63); }

static inline bool isnat (u64 o) { return o < pintag; }
static inline bool ispin (u64 o) { return o >= pintag && o < lawtag; }
static inline bool islaw (u64 o) { return o >= lawtag && o < clztag; }
static inline bool isclz (u64 o) { return o >= clztag && o < thktag; }
static inline bool isthk (u64 o) { return o >= thktag; }

typedef struct raxrdx { u64 rax, rdx; } raxrdx;

u64	mkbuffer(u64 bytes);
u64	zcall0	(u64(*)(void));
u64	zcall1	(u64(*)(u64), u64);
u64	zcall2	(u64(*)(u64,u64), u64, u64);
u64	zcall3	(u64(*)(u64,u64,u64), u64, u64, u64);
u64	zcall4	(u64(*)(u64,u64,u64,u64), u64, u64, u64, u64);
u64	zcall5	(u64(*)(u64,u64,u64,u64,u64), u64, u64, u64, u64, u64);
raxrdx	zzcal2	(raxrdx(*)(u64,u64), u64, u64);

Val	fastmul	(Val, Val);
Val	opdiv	(Val, Val);
void	Judge	(u64);
Val	natwords(Val);

u64	reserve        (u64 size);
u64	rawreserve     (u64 size);
raxrdx reservecopymin  (u64 bignum, u64 min);
raxrdx reservecopyflex (u64 bignum, u64 min);

Val  c_eval         (Val);

// Assembly routines.  NOT SAFE TO CALL DIRECTLY.  MAY NOT ACTUALLY
// RETURN VALUES.
u64	fastlsh        (Val, Val);
u64	fastrsh        (void);
Val	fastmul        (Val, Val);
u64	force          (void);
Val	fastinc        (Val);
Val	fastdec        (Val);
Val	fastadd        (Val, Val);
Val	fastload       (Val, Val, Val);
Val	fastsub        (Val, Val);
Val	fastpokeop     (Val, Val, Val, Val);
Val	fastpeekop     (Val, Val, Val);
Val	fastclear      (Val, Val);
Val	fastsnore      (Val, Val, Val, Val, Val);
Val	faststore      (Val, Val, Val, Val);
Val	faststore8     (Val, Val, Val);
Val	faststore64    (Val, Val, Val);

void rpn_gc        (u64 words);

void bug_repl (void);
int  unittest (void);
bool streq    (const char *a, const char *b);

u64  seedfile       (const char *filename);


#define push(w) (*(--sp) = w)
#define pop() (*(sp++))
#define drop() (sp++)

u64 arity (u64);


int syscall_read(int fd, char *buf, u64 sz);

////////////////////////////////////////////////////////////////////////////////

Val	mkclz1	(Val a, Val b);
Val	mkclz2	(Val a, Val b, Val c);
Val	MkPin	(Val a);
Val	MkLaw	(Val a, Val b, Val c);
Val	NatCmp	(Val a, Val b);

Val	fastbitsz	(Val a);
Val	directbits	(Val a);
Val	fastbytsz	(Val a);

Val big (u64 words, u64 *buf);
u64 strlen (const char *s);
Val str_ (u64 n, const char *s);
Val str (const char *s);

u64 T[8] = {0};

static Val k1   (Val a, Val b)        { return zcall2(mkclz1, a, b);      }
static Val k2   (Val a, Val b, Val c) { return zcall3(mkclz2, a, b, c);   }
static Val pin  (Val i)               { return zcall1(MkPin, i);          }
static Val law  (Val a, Val b, Val c) { return zcall3(MkLaw, a, b, c);    }
static Val cadd (Val a, Val b)        { return zcall2(fastadd, a, b);     }
static Val cmul (Val a, Val b)        { return zcall2(fastmul, a, b);     }
static Val w1   (u64 w)               { return big(1, &w);                }
static Val w2   (u64 a, u64 b)        { T[0]=b; T[1]=a; return big(2,T);  }

#define assert_regeq(a,b) if (a != b) { __builtin_trap(); }

static void assert_nateq(Val a, Val b) {
	push(a); push(b);
	sp[0] = c_eval(sp[0]);
	sp[1] = c_eval(sp[1]);
	a=sp[1]; b=sp[0]; sp+=2;

	if (!isnat(a)) __builtin_trap();
	if (!isnat(b)) __builtin_trap();
	if (a == b) return;
	if (isdirect(b) || isdirect(a)) __builtin_trap();

	u64 az = get_bitsz(b);
	u64 bz = get_bitsz(a);

	if (az != bz) __builtin_trap();

	u64 *pa = PTR(b)-1;
	u64 *pb = PTR(a)-1;

	u64 n = ((az+63)/64)+1;

	for (int i=0; i<n; i++) {
		if (pa[i] != pb[i]) __builtin_trap();
	}
}

static void assert_arity (Val v, int expected) {
	int a = zcall1(&arity, v);
	if (a != expected) __builtin_trap();
}

u64 dmax = 0x7fffffffffffffff;

void test_opclear (void) {
	/* Direct Cases */
	push(str("Hello World!"));
	assert_regeq(0,                  zcall2(fastclear, 0,   1));
	assert_regeq(2,                  zcall2(fastclear, 0,   2));
	assert_regeq(2,                  zcall2(fastclear, 0,   3));
	assert_regeq(dmax,               zcall2(fastclear, 65,  dmax));
	assert_regeq(dmax,               zcall2(fastclear, 64,  dmax));
	assert_regeq(dmax,               zcall2(fastclear, 63,  dmax));
	assert_regeq(0x3fffffffffffffff, zcall2(fastclear, 62,  dmax));
	assert_regeq(0x7ffffffffffffffe, zcall2(fastclear, 0,   dmax));
	assert_regeq(dmax,               zcall2(fastclear, *sp, dmax))
	drop();

	/* Indirect Cases */
	push(w2(0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF));
	push(0);
	*sp = w2(0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF);
	assert_nateq(*sp,                zcall2(fastclear, 999, sp[1]));
	assert_nateq(*sp,                zcall2(fastclear, 128, sp[1]));
	assert_nateq(*sp,                zcall2(fastclear, *sp, sp[1]));
	*sp = w2(0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF);
	assert_nateq(*sp,                zcall2(fastclear, 127, sp[1]));
	*sp = w2(0xBFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF);
	assert_nateq(*sp,                zcall2(fastclear, 126, sp[1]));
	*sp = w2(0xDFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF);
	assert_nateq(*sp,                zcall2(fastclear, 125, sp[1]));
	*sp = w2(0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFE);
	assert_nateq(*sp,                zcall2(fastclear, 0, sp[1]));
	drop();
	drop();

}


void test_opstore (void) {
	u64 x = str("01234567");
	u64 abc = str("abc");

	/* Store8 */
	assert_regeq(str("za"),       zcall4(faststore, 1, 1, 'a', 'z'));
	assert_regeq(str("0123456a"), zcall4(faststore, 7, 1, abc, x));

	/* Store64 */
	assert_regeq(str("012345a"),  zcall4(faststore, 6, 8, 'a', x));
	assert_regeq(str("0123456a"), zcall4(faststore, 7, 8, 'a', x));

	/* Indirect Input */
	push(str("abcdefghijklmnopqrstuvwxyz"));
	push(str("Hello World!"));
	push(str("abclo World!"));
	assert_nateq(*sp, zcall4(faststore, 0, 3, sp[2], sp[1]));
	*sp = str("Habco World!");
	assert_nateq(*sp, zcall4(faststore, 1, 3, sp[2], sp[1]));
	*sp = str("Hello Worabc");
	assert_nateq(*sp, zcall4(faststore, 9, 3, sp[2], sp[1]));
	*sp = str("Hello Worlabc");
	assert_nateq(*sp, zcall4(faststore, 10, 3, sp[2], sp[1]));
	*sp = str("Hello Worldabc");
	assert_nateq(*sp, zcall4(faststore, 11, 3, sp[2], sp[1]));
	sp[1] = str("Hello World!!!!");
	sp[0] = str("Hello World!!!!abc"); // three words
	assert_nateq(*sp, zcall4(faststore, 15, 3, sp[2], sp[1]));
	sp[1] = str("H");
	sp[0] = str_(18, "H\0\0\0\0\0\0\0\0\0\0\0\0\0\0abc");
	assert_nateq(*sp, zcall4(faststore, 15, 3, sp[2], sp[1]));
	drop();
	drop();
	drop();

	/* Copy More Than Given */
	push(str("abcdefghijklmnopqrstuvwxyz"));              // copy from
	push(str("Hello World!!!!!!!!!!!!!!!!!!!!!"));        // copy to
	push(str_(32, "Heabcdefghijklmnopqrstuvwxyz\0\0!!")); // result
	assert_nateq(*sp, zcall4(faststore, 2, 28, sp[2], sp[1]));
	*sp = str("Heabcdefghijklmnopqrstuvwxyz!!!!");
	assert_nateq(*sp, zcall4(faststore, 2, 26, sp[2], sp[1]));
	drop();
	drop();
	drop();

	/* Direct Input */
	push(str("abc"));
	push(str("Hello World!"));
	push(str("abclo World!"));
	assert_nateq(*sp, zcall4(faststore, 0, 3, sp[2], sp[1]));
	*sp = str("Habco World!");
	assert_nateq(*sp, zcall4(faststore, 1, 3, sp[2], sp[1]));
	*sp = str("Hello Worabc");
	assert_nateq(*sp, zcall4(faststore, 9, 3, sp[2], sp[1]));
	*sp = str("Hello Worlabc");
	assert_nateq(*sp, zcall4(faststore, 10, 3, sp[2], sp[1]));
	*sp = str("Hello Worldabc");
	assert_nateq(*sp, zcall4(faststore, 11, 3, sp[2], sp[1]));
	sp[1] = str("Hello World!!!!");
	sp[0] = str("Hello World!!!!abc"); // three words
	assert_nateq(*sp, zcall4(faststore, 15, 3, sp[2], sp[1]));
	sp[1] = str("H");
	sp[0] = str_(18, "H\0\0\0\0\0\0\0\0\0\0\0\0\0\0abc");
	assert_nateq(*sp, zcall4(faststore, 15, 3, sp[2], sp[1]));
	drop();
	drop();
	drop();

	/* Copy More Than Given */
	push(str("abc"));                 // copy from
	push(str("Hello World!"));        // copy to
	push(str_(12, "Hello abc\0\0!")); // result
	assert_nateq(*sp, zcall4(faststore, 6, 5, sp[2], sp[1]));
	*sp = str("Hello Worlabc");
	assert_nateq(*sp, zcall4(faststore, 10, 999, sp[2], sp[1]));
	drop();
	drop();
	drop();
}

void test_opsnore_direct_direct (void) {
	u64 b   = str("b");
	u64 bb  = str("bb");
	u64 bs  = str("bbbbbbb");
	u64 hel = str("Hello\1");

	assert_regeq(str("bello\1"), zcall5(fastsnore, 0,0,1,  b,  hel));
	assert_regeq(str("bello\1"), zcall5(fastsnore, 0,0,1,  bb, hel));
	assert_regeq(str("bbllo\1"), zcall5(fastsnore, 0,0,2,  bb, hel));
	assert_regeq(str("bello\1"), zcall5(fastsnore, 1,0,2,  bb, hel));
	assert_regeq(str("Hello\1"), zcall5(fastsnore, 2,0,2,  bb, hel));
	assert_regeq(str("Hbllo\1"), zcall5(fastsnore, 0,1,1,  b,  hel));
	assert_regeq(str("Heblo\1"), zcall5(fastsnore, 0,2,1,  b,  hel));
	assert_regeq(str("Helbo\1"), zcall5(fastsnore, 0,3,1,  b,  hel));
	assert_regeq(str("Hebbo\1"), zcall5(fastsnore, 0,2,2,  bb, hel));
	assert_regeq(str("Helbb\1"), zcall5(fastsnore, 0,3,2,  bb, hel));
	assert_regeq(str("Hellb\1"), zcall5(fastsnore, 0,4,2,  bb, hel));
	assert_regeq(str("Hbblo\1"), zcall5(fastsnore, 0,1,22, bb, hel));
	assert_regeq(str("Hellb\1"), zcall5(fastsnore, 0,4,22, bb, hel));
	assert_regeq(str("Hbbbb\1"), zcall5(fastsnore, 0,1,22, bs, hel));

	// huge offsets and sizes
	push(str("Hello World!"));
	assert_regeq(str("Hello\1"), zcall5(fastsnore, 0,   *sp, *sp, bs, hel));
	assert_regeq(str("Hello\1"), zcall5(fastsnore, *sp, 0,   *sp, bs, hel));
	assert_regeq(str("Hello\1"), zcall5(fastsnore, *sp, *sp, *sp, bs, hel));
	drop();
}

void test_opsnore_indirect_indirect (void) {
	push(str("abcdefghijklmnopqrstuvwxyz"));

	push(0); push(0);

	sp[1] = str("Hello World!\1");
	sp[0] = str("aello World!\1");
	assert_nateq(*sp, zcall5(fastsnore, 0,0,1, sp[2], sp[1]));

	sp[1] = str("Hello World!\1");
	sp[0] = str("Hallo World!\1");
	assert_nateq(*sp, zcall5(fastsnore, 0,1,1, sp[2], sp[1]));

	sp[1] = str("Hello World!\1");
	sp[0] = str("bello World!\1");
	assert_nateq(*sp, zcall5(fastsnore, 1,0,1, sp[2], sp[1]));

	sp[1] = str("Hello World!\1");
	sp[0] = str("Hbllo World!\1");
	assert_nateq(*sp, zcall5(fastsnore, 1,1,1, sp[2], sp[1]));

	sp[1] = str("Hello World!\1");
	sp[0] = str("Hbcdefghijkl\1");
	assert_nateq(*sp, zcall5(fastsnore, 1,1,100, sp[2], sp[1]));

	sp[1] = str("Hello World!\1");
	sp[0] = str("Hello Worlkl\1");
	assert_nateq(*sp, zcall5(fastsnore, 10,10,100, sp[2], sp[1]));

	sp[1] = str("Hello World!\1");
	sp[0] = str("Hello World!\1");
	assert_nateq(*sp, zcall5(fastsnore, 100,1,100, sp[2], sp[1]));

	sp[1] = str("Hello World!\1");
	sp[0] = str("Hello World!\1");
	assert_nateq(*sp, zcall5(fastsnore, 1,100,100, sp[2], sp[1]));

	drop(); drop(); drop();
}

#define dosnore(res,iof,bof,wid,inp,buf) \
	push(str(inp)); push(str(buf)); push(str(res)); \
	assert_nateq(*sp, \
		zcall5(fastsnore, iof, bof, wid, sp[2], sp[1])); \
	drop(); drop(); drop()

void test_opsnore_direct_indirect (void) {
	dosnore("aello World!\1",   0, 0,1, "a",        "Hello World!\1");
	dosnore("aello World!\1",   0, 0,9, "a",        "Hello World!\1");
	dosnore("aello World!\1",   0, 0,1, "abcdefgh", "Hello World!\1");
	dosnore("bello World!\1",   1, 0,1, "abcdefgh", "Hello World!\1");
	dosnore("Hbllo World!\1",   1, 1,1, "abcdefgh", "Hello World!\1");
	dosnore("hello World!\1",   7, 0,1, "abcdefgh", "Hello World!\1");
	dosnore("hello World!\1",   7, 0,9, "abcdefgh", "Hello World!\1");
	dosnore("ghllo World!\1",   6, 0,9, "abcdefgh", "Hello World!\1");
	dosnore("abcdefghrld!\1",   0, 0,9, "abcdefgh", "Hello World!\1");
	dosnore("Hello World!\1",   9, 0,9, "abcdefgh", "Hello World!\1");
	dosnore("Hello Wordef\1",   3, 9,9, "abcdefgh", "Hello World!\1");
	dosnore("Hello World!\1",   0,99,9, "abcdefgh", "Hello World!\1");
	dosnore("hello World!\1",   7, 0,9, "abcdefgh", "Hello World!\1");
	dosnore("hello World!\1",   7, 0,1, "abcdefgh", "Hello World!\1");
	dosnore("Hello Worldh\1",   7,11,1, "abcdefgh", "Hello World!\1");
	dosnore("01234567890123a5", 0,14,1, "abcdefgh", "0123456789012345");
	dosnore("0123456abcdefgh5", 0, 7,8, "abcdefgh", "0123456789012345");
	dosnore("01234567abcdefg5", 0, 8,8, "abcdefgh", "0123456789012345");
}

#define dosnort(res,iof,bof,wid,inp,buf) \
	assert_regeq(str(res), zcall5(fastsnore, iof, bof, wid, inp, str(buf)))

void test_opsnore_indirect_direct (void) {
	push(str("abcdefghijklmnopqrstuvwxyz"));
	dosnort("Hello!\1", 0,  0,  0, *sp, "Hello!\1");
	dosnort("aello!\1", 0,  0,  1, *sp, "Hello!\1");
	dosnort("bello!\1", 1,  0,  1, *sp, "Hello!\1");
	dosnort("Hallo!\1", 0,  1,  1, *sp, "Hello!\1");
	dosnort("Heblo!\1", 1,  2,  1, *sp, "Hello!\1");
	dosnort("Heplo!\1", 15, 2,  1, *sp, "Hello!\1");
	dosnort("Hezlo!\1", 25, 2,  1, *sp, "Hello!\1");
	dosnort("Hello!\1", 26, 2,  1, *sp, "Hello!\1");
	dosnort("Heyzo!\1", 24, 2,  2, *sp, "Hello!\1");
	dosnort("Heyzo!\1", 24, 2, 22, *sp, "Hello!\1");
	dosnort("abcdef\1",  0, 0, 99, *sp, "Hello!\1");
	dosnort("abcdefg7",  0, 0, 99, *sp, "01234567");
	dosnort("0bcdefg7",  1, 1, 99, *sp, "01234567");
	drop();
}

void test_opsnore (void) {
	test_opsnore_direct_direct();
	test_opsnore_indirect_direct();
	test_opsnore_direct_indirect();
	test_opsnore_indirect_indirect();
}

void test_opstore8 (void) {
	/* Direct Cases */
	assert_regeq('a',       zcall3(faststore8, 0, 'a', 'z'));
	assert_regeq(str("za"), zcall3(faststore8, 1, 'a', 'z'));
	u64 x = str("01234567");
	u64 abc = str("abc");
	assert_regeq(str("a1234567"), zcall3(faststore8, 0, abc, x));
	assert_regeq(str("012345a7"), zcall3(faststore8, 6, abc, x));
	assert_regeq(str("0123456a"), zcall3(faststore8, 7, abc, x));

	/* Indirect Byte */
	push(str("aetheling"));
	assert_regeq(str("a1234567"), zcall3(faststore8, 0, *sp, x));
	assert_regeq(str("012345a7"), zcall3(faststore8, 6, *sp, x));
	assert_regeq(str("0123456a"), zcall3(faststore8, 7, *sp, x));
	drop();

	/* Indirect Number */
	push(str("Hello World!"));
	push(str("Hello World!a"));
	assert_nateq(*sp, zcall3(faststore8, 12, abc, sp[1]));
	*sp = str("Hello Worlda");
	assert_nateq(*sp, zcall3(faststore8, 11, abc, sp[1]));
	*sp = str("Hello Warld!");
	assert_nateq(*sp, zcall3(faststore8, 7, abc, sp[1]));
	*sp = str("Hallo World!");
	assert_nateq(*sp, zcall3(faststore8, 1, abc, sp[1]));
	drop();
	drop();
}

void test_opstore64 (void) {
	u64 x = str("01234567");
	u64 abc = str("abc");

	/* Direct Cases */
	assert_regeq('a',             zcall3(faststore64, 0, 'a', 'z'));
	assert_regeq(str("za"),       zcall3(faststore64, 1, 'a', 'z'));
	assert_regeq(str("a"),        zcall3(faststore64, 0, 'a', x));
	assert_regeq(str("012345a"),  zcall3(faststore64, 6, 'a', x));
	assert_regeq(str("0123456a"), zcall3(faststore64, 7, 'a', x));

	/* Direct Overflow */
	push(str("01234567a"));
	assert_nateq(*sp, zcall3(faststore64, 8, 'a', x));
	assert_regeq(abc, zcall3(faststore64, 0, abc, x));
	*sp = str("012345abc");
	assert_nateq(*sp, zcall3(faststore64, 6, abc, x));
	*sp = str("0123456abc");
	assert_nateq(*sp, zcall3(faststore64, 7, abc, x));
	*sp = str("01234567abc");
	assert_nateq(*sp, zcall3(faststore64, 8, abc, x));
	*sp = str_(13, "01234567\0abc");
	assert_nateq(*sp, zcall3(faststore64, 9, abc, x));
	drop();

	/* Indirect Word */
	push(str("aetheling"));
	push(0);
	*sp = str("aethelin");
	assert_regeq(*sp, zcall3(faststore64, 0, sp[1], x));
	*sp = str("0aethelin");
	assert_nateq(*sp, zcall3(faststore64, 1, sp[1], x));
	*sp = str("0123aethelin");
	assert_nateq(*sp, zcall3(faststore64, 4, sp[1], x));
	*sp = str("01234567aethelin");
	assert_nateq(*sp, zcall3(faststore64, 8, sp[1], x));
	*sp = str_(17, "01234567\0aethelin");
	assert_nateq(*sp, zcall3(faststore64, 9, sp[1], x));
	drop();
	drop();

	/* Indirect Number */
	push(str("aetheling"));
	push(str("Hello World!"));
	push(str("Hello World!aethelin"));
	assert_nateq(*sp, zcall3(faststore64, 12, sp[2], sp[1]));
	*sp = str("aethelinrld!");
	assert_nateq(*sp, zcall3(faststore64, 0, sp[2], sp[1]));
	*sp = str("Haethelinld!");
	assert_nateq(*sp, zcall3(faststore64, 1, sp[2], sp[1]));
	*sp = str("Hello Waethelin");
	assert_nateq(*sp, zcall3(faststore64, 7, sp[2], sp[1]));
	*sp = str_(21, "Hello World!\0aethelin");
	assert_nateq(*sp, zcall3(faststore64, 13, sp[2], sp[1]));
	drop();
	drop();
	drop();
}

// fastload(offset, width, nat) -> slice
void test_opload (void) {
	// direct inputs
	assert_regeq(0,           zcall3(fastload, 0,  0,  str("asdf")));
	assert_regeq(str("a"),    zcall3(fastload, 0,  1,  str("asdf")));
	assert_regeq(0,           zcall3(fastload, 1,  0,  str("asdf")));
	assert_regeq(str("s"),    zcall3(fastload, 1,  1,  str("asdf")));
	assert_regeq(str("asdf"), zcall3(fastload, 0,  20, str("asdf")));
	assert_regeq(0,           zcall3(fastload, 20, 0,  str("asdf")));
	assert_regeq(0,           zcall3(fastload, 20, 20, str("asdf")));

	// indirect tests.

	push(0);
	push(str("bcdefghijklmnop"));
	push(str("abcdefghijklmnop"));
	assert_regeq(0,           zcall3(fastload, 0,  0,  *sp));
	assert_regeq(0,           zcall3(fastload, 3,  0,  *sp));
	assert_regeq(str("abc"),  zcall3(fastload, 0,  3,  *sp));
	assert_regeq(str("def"),  zcall3(fastload, 3,  3,  *sp));
	assert_nateq(*sp,         zcall3(fastload, 0,  20, *sp));
	assert_regeq(0,           zcall3(fastload, 20, 0,  *sp));
	assert_regeq(0,           zcall3(fastload, 20, 20, *sp));
	assert_nateq(sp[1],       zcall3(fastload, 1,  20, *sp));

	// "huge sizes".

	sp[2] = w1(0xFFFFFFFFFFFFFFFF); // indirect
	assert_nateq(sp[1],       zcall3(fastload, 1,  9999999999, *sp));
	assert_nateq(sp[1],       zcall3(fastload, 1,  sp[2],     *sp));

	// "huge offsets".

	assert_regeq(str("op"), zcall3(fastload, 14,        8, *sp));
	assert_regeq(str("p"),  zcall3(fastload, 15,        8, *sp));
	assert_regeq(0,         zcall3(fastload, 17,        8, *sp));
	assert_regeq(0,         zcall3(fastload, 18,        8, *sp));
	assert_regeq(0,         zcall3(fastload, 999999999, 8, *sp));
	assert_regeq(0,         zcall3(fastload, sp[2],     8, *sp));
	sp += 2;
}

void test_lsh (void) {
	assert_regeq(0,                  zcall2(fastlsh, 0, 0));
	assert_regeq(0,                  zcall2(fastlsh, 0, 1));
	assert_regeq(1,                  zcall2(fastlsh, 1, 0));
	assert_regeq(2,                  zcall2(fastlsh, 1, 1));
	assert_regeq(0x4000000000000000, zcall2(fastlsh, 1, 62));
	assert_regeq(0x6000000000000000, zcall2(fastlsh, 3, 61));

	sp -= 2;

	sp[0] = w1(0x8000000000000000);
	sp[1] = zcall2(fastlsh, 1, 63);
	assert_nateq(sp[0], sp[1]);

	sp[0] = w1(0xfffffffffffffffe);
	sp[1] = zcall2(fastlsh, dmax, 1);
	assert_nateq(sp[0], sp[1]);

	sp[0] = w2(1, 0xfffffffffffffffc);
	sp[1] = zcall2(fastlsh, dmax, 2);
	assert_nateq(sp[0], sp[1]);

	sp[0] = w2(0x3fffffffffffffff, 0x8000000000000000);
	sp[0] = zcall2(fastlsh, sp[0], 1);
	sp[1] = w2(0x7fffffffffffffff, 0x0000000000000000);
	assert_nateq(sp[0], sp[1]);

	sp[1] = w2(0x7f, 0xfef7e04678000000);
	sp[0] = w2(0x7,  0xffef7e0467800000);
	sp[0] = zcall2(fastlsh, sp[0], 4);
	assert_nateq(sp[0], sp[1]);

	sp += 2;
}

void test_pokeop (void) {
	unsigned char bfr[64];

	for (int i=0; i<64; i++) bfr[i] = 255;

	/* direct cases */

	zcall4(fastpokeop, (u64)bfr, 7, 0, 5);
	assert_regeq(bfr[0], 7);
	assert_regeq(bfr[1], 0);
	assert_regeq(bfr[4], 0);
	assert_regeq(bfr[5], 255);

	zcall4(fastpokeop, (u64)bfr, 0x1122334455667788, 0, 8);
	assert_regeq(bfr[0], 0x88);
	assert_regeq(bfr[7], 0x11);
	assert_regeq(bfr[8], 0xFF);

	zcall4(fastpokeop, (u64)bfr, 0x1122334455667788, 0, 9);
	assert_regeq(bfr[0], 0x88);
	assert_regeq(bfr[7], 0x11);
	assert_regeq(bfr[8], 0x00);


	/* inddirect cases */

	zcall4(fastpokeop, (u64)bfr, str("Hello world!"), 0, 9);
	assert_regeq(bfr[0], 'H');
	assert_regeq(bfr[8], 'r');
	assert_regeq(bfr[9], 0xFF);

	zcall4(fastpokeop, (u64)bfr, str("Hello world!"), 0, 16);
	assert_regeq(bfr[0], 'H');
	assert_regeq(bfr[11], '!');
	assert_regeq(bfr[12], 0);
	assert_regeq(bfr[16], 0xff);
}

void test_peekop (void) {
	Val n;
	char *example = "Hello world!";
	n = zcall3(fastpeekop, (u64)example, 0, 0);
	assert_regeq(n, 1); // "\1"
	n = zcall3(fastpeekop, (u64)example, 0, 1);
	assert_regeq(n, 328); // "H\1"
	n = zcall3(fastpeekop, (u64)example, 0, 3);
	assert_regeq(n, 23881032); // "Hel\1"
	n = zcall3(fastpeekop, (u64)example, 0, 9);
	push(str("Hello wor\1"));
	push(n);
	assert_nateq(sp[0], sp[1]);
	sp[1] = str("Hello world!\1");
	sp[0] = zcall3(fastpeekop, (u64)example, 0, 12);
	assert_nateq(sp[0], sp[1]);
}

void test_smallops (void) {
	/* small ops */
	assert_regeq(6,   cmul(2, 3));
	assert_regeq(7,   cadd(6, 1));
	assert_regeq(200, cadd(7, 193));
}

void test_basicadds (void) {
	/* basic adds */
	assert_regeq(dmax, dmax);
	assert_regeq(dmax, cadd(dmax, 0));
	assert_regeq(dmax, cadd(0, dmax));
	assert_regeq(dmax, cadd(1, dmax-1));
	assert_nateq(dmax, dmax);
}

void test_bigadds (void) {
	/* big adds */
	push(w1(0xFFFFFFFFFFFFFFFF));
	push(w1(0x8000000000000000));
	assert_nateq(sp[0], sp[0]);
	assert_nateq(sp[0], cadd(1, dmax));
	assert_nateq(sp[0], cadd(dmax, 1));
	assert_nateq(sp[1], cadd(sp[0], dmax));
	sp[0] = w2(1,0);
	assert_nateq(*sp, cadd(1, sp[1]));
	sp[0] = w2(1,0xFFFFFFFFFFFFFFFE);
	assert_nateq(*sp, cadd(sp[1], sp[1]));
	sp += 2;
}

void test_bigeqs (void) {
	/* big eqs */
	push(w2(1,0));
	push(w2(1,0));
	assert_nateq(sp[0], sp[1]);
	sp += 2;
}

void test_bigmuls (void) {
        /* big muls */
        push(w1(0x8000000000000000));
        u64 a = 0x4000000000000000;
        assert_regeq(0, cmul(0, a));
        assert_regeq(a, cmul(1, a));
        assert_nateq(*sp, cmul(2, a));
        *sp = w2(1,0);
        assert_nateq(*sp, cmul(4, a));
        *sp = w2(0x1000000000000000,0);
        assert_nateq(*sp, cmul(a, a));
        sp++;
}

void test_bitsz (void) {
	assert_regeq(0, fastbitsz(0));
	assert_regeq(1, fastbitsz(1));
	assert_regeq(2, fastbitsz(2));
	assert_regeq(2, fastbitsz(3));
	assert_regeq(3, fastbitsz(4));
	assert_regeq(63, fastbitsz(dmax));
	assert_regeq(64, fastbitsz(cadd(1,dmax)));
	assert_regeq(65, fastbitsz(w2(1,0)));
	assert_regeq(66, fastbitsz(w2(3,0)));
	assert_regeq(150, fastbitsz(str("Hello there world!!")));

	assert_regeq(0, fastbytsz(0));
	assert_regeq(1, fastbytsz(1));
	assert_regeq(1, fastbytsz(255));
	assert_regeq(2, fastbytsz(256));
	assert_regeq(8, fastbytsz(dmax));
	assert_regeq(19, fastbytsz(str("Hello there world!!")));
}

void test_arities (void) {
	assert_arity(0, dmax);
	assert_arity(pin(0), 1);
	assert_arity(pin(1), 3);
	assert_arity(pin(2), 1);
	assert_arity(pin(3), 6);
	assert_arity(pin(4), 1);
	assert_arity(k1(1,0), dmax-1);
	assert_arity(k2(3,2,1), dmax-2);
	assert_arity(law(1,2,3), 2);
	assert_arity(law(9,9,9), 9);
}

raxrdx try_reservecopyflex (u64 val, u64 mucksz, u64 min) {
	push(val);

	// Fill the end of the heap with nonsense, so we can later verify
	// that the appropriate amount of memory is being zeroed.
	zcall1(rawreserve, mucksz);
	for (int i=0; i< mucksz; i++) heap_next[i] = 0xBADBEEFBADBEEF;

	// Perform the copy.
	raxrdx r = reservecopyflex(*sp, min);
	u64 *p = (u64*) r.rax;

	// Get the word-size of the input.
	u64 z = zcall1(natwords, *sp);

	// Verify that the data was copied in.
	if (val >> 63) {
		for (int i=0; i<z; i++) assert_regeq(p[i], PTR(*sp)[i]);
	} else {
		assert_regeq(p[0], val);
	}

	// Verify that reserved space was zeroed.
	for (int i=z; i<min; i++) assert_regeq(p[i], 0);

	sp++;
	return r;
}

void test_reserve() {
	assert_regeq(0, zcall1(natwords, 0));
	assert_regeq(1, zcall1(natwords, 1));
	push(w1(0xffffffffffffffff));
	assert_regeq(1, zcall1(natwords, *sp));
	*sp = w2(1,2);
	assert_regeq(2, zcall1(natwords, *sp));
	sp++;
	try_reservecopyflex(w2(1,2), 128, 0);
	try_reservecopyflex(w2(1,2), 128, 1);
	try_reservecopyflex(w2(1,2), 128, 2);
	try_reservecopyflex(w2(1,2), 128, 3);
	try_reservecopyflex(w2(1,2), 128, 128);
	try_reservecopyflex(0, 128, 0);
	try_reservecopyflex(1, 128, 1);
	try_reservecopyflex(dmax, 128, 2);
	try_reservecopyflex(dmax, 128, 128);
}

void test_buddy_explicit_free();

/* -------------------------------------------------------------------- */

void memcheck        (const char *);
void memcheck_forced (const char *);

void noreturn die   (const char*);
void          wrn   (const char*);
void          put_bytes (const char *msg, int sz);
void          put   (const char*);

#define crash(msg) die(msg)

enum mode { BASE, DECI, HEXI, SEMI, SEED, WORD, TEXT, CURL } mode = BASE;

static char bad_input[] = "bad input: X\n";

void mkapp          (void);
Val  c_MkLaw     (Val, Val, Val);
void	c_or           (void);
void	c_xor          (void);
void	c_and          (void);

u64 reserve (u64);
u64 claim   (u64);

void print_value(Val v, bool ln);
void print_stack ();
void print_heap (void);


Val big (u64 words, u64 *buf) {
	u64 *p = (u64*) zcall1(reserve, words);
	for (int i=0; i<words; i++) p[i] = buf[i];
	return zcall1(claim, words);
}

static void loadbuf (char *buf, u64 bsz) {
	u64 words = (bsz + 7) / 8;
	char *ptr = (char*)zcall1(reserve, words);
	for (int i=0; i<bsz; i++) ptr[i] = buf[i];
	*(--sp) = zcall1(claim, words);
}

static inline void swap (void) {
	Val tmp = *sp;
	*sp = sp[1];
	sp[1] = tmp;
}

static inline bool stack_has (int c) {
	if (sp + (c-1) <= stack_last) return true;
	return false;
}

static inline void check_stack (u64 need) {
	if (!stack_has(need)) die("stack underflow");
}

static inline void rpn_pop (void) {
	check_stack(1);
	sp++;
}

static inline void rpn_dup (void) {
	check_stack(1);
	sp--;
	*sp = sp[1];
}

static inline void rpn_swap (void) {
	check_stack(2);
	swap();
}


static inline void rpn_over (void) {
	check_stack(2);
	--sp;
	*sp = sp[2];
}

static inline void rpn_push (void) {
	u64 x = *sp;
	check_stack(x+2);
	*sp = sp[x+1];
}

static inline void rpn_rap (void) {
	check_stack(2);
	swap();
	mkapp();
}

static inline void rpn_app (void) {
	check_stack(2);
	mkapp();
}

static inline void rpn_eval (void) {
	check_stack(1);
	Val res = c_eval(*sp);
	*sp = res;
}

static inline void rpn_force (void) {
	check_stack(1);
	zcall0(&force);
}

static inline void rpn_pin (void) {
	check_stack(1);
	rpn_force();
	Val item = *(sp++);
	Val pin = (Val)zcall1(MkPin, item);
	*(--sp) = pin;
}

static inline void rpn_law (void) {
	check_stack(3);
	Val n = *sp++;
	Val a = *sp++;
	Val b = *sp++;
	Val r = c_MkLaw(n, a, b);
	*(--sp) = r;
}

extern int  g_max_pdepth;

static inline void rpn_pdepth (void) {
	check_stack(1);
	Val v = *sp;
	sp++;
	if (v > INT_MAX) { crash("impossible printer depth\n"); }
	g_max_pdepth = v;
}

extern int g_max_pdetph;
extern bool g_print_cords;
extern bool g_show_bitsize;

static inline void rpn_cords (void) { g_print_cords  = 1; }
static inline void rpn_bitsz (void) { g_show_bitsize = 1; }

static inline void rpn_sub (void) {
	check_stack(2);
	Val b = *sp++;
	Val a = *sp++;
	Val res = zcall2(fastsub, a, b);
	*(--sp) = res;
}

static inline void rpn_add (void) {
	check_stack(2);
	Val x = *sp++;
	Val y = *sp++;
	Val r = zcall2(fastadd, x, y);
	*(--sp) = r;
}

static inline void rpn_inc (void) {
	check_stack(1);
	Val a = *sp++;
	Val res = zcall1(fastinc, a);
	*(--sp) = res;
}

static inline void rpn_buffer (void) {
	check_stack(1);
	Val a = *sp++;
	push(zcall1(&mkbuffer, a));
}

static inline void rpn_opload (void) {
	check_stack(3);
	Val offset = *sp++;
	Val width = *sp++;
	Val nat = *sp++;
	Val z = zcall3(&fastload, offset, width, nat);
	push(z);
}

static inline void rpn_dec (void) {
	check_stack(1);
	Val a = *sp++;
	Val res = zcall1(fastdec,a);
	*(--sp) = res;
}

static inline void rpn_mul (void) {
	check_stack(2);
	Val b = *sp++;
	Val a = *sp++;
	Val res = zcall2(fastmul, a, b);
	*(--sp) = res;
}

static inline void rpn_div (void) {
	check_stack(2);
	Val b = *sp++;
	Val a = *sp++;
	Val res = zcall2(opdiv, a, b);
	*(--sp) = res;
}

char strbuf[65536];
int curldepth=0;

void bug_repl (void) {
	int i=0;
loop:
	char c;
	int n = syscall_read(0, &c, 1);
	if (!n) c=0;

	switch (mode) {
	case SEED: goto seed;
	case WORD: goto word;
	case TEXT: goto text;
	case CURL: goto curl;
	case DECI: goto deci;
	case HEXI: goto hexi;
	case SEMI: goto semi;
	case BASE: goto base;
	default: die("bad mode");
	}

semi:
	mode=SEMI;
	if (c == '\n' || c == 0) goto base;
	goto loop;

hexi:
	mode=HEXI;

	u64 x=0;

	if (c >= '0' && c <= '9')      { x = c - '0';        }
	else if (c >= 'a' && c <= 'f') { x = 10 + (c - 'a'); }
	else if (c >= 'A' && c <= 'F') { x = 10 + (c - 'A'); }
	else goto base;

	*sp = zcall2(fastlsh, *sp, 4);

	if (x) {
		// TODO: hack to get around missing indirect add
		*(--sp) = x;
		rpn_add();
	}
	goto loop;

seed:
	mode=SEED;
	if ( (c >= 'a' && c <= 'z') ||
	     (c >= 'A' && c <= 'Z') ||
	     (c >= '0' && c <= '9') ) {
		strbuf[i++] = c;
		strbuf[i] = '\0';
		goto loop;
	}
	*(--sp) = seedfile(strbuf);
	goto base;

word:
	mode=WORD;
	if (c >= 'a' && c <= 'z') {
		strbuf[i++] = c;
		strbuf[i] = '\0';
		goto loop;
	}

	if (streq(strbuf,"dup"))      { rpn_dup();              goto base; }
	if (streq(strbuf,"swap"))     { rpn_swap();             goto base; }
	if (streq(strbuf,"pop"))      { rpn_pop();              goto base; }
	if (streq(strbuf,"drop"))     { rpn_pop();              goto base; }
	if (streq(strbuf,"push"))     { rpn_push();             goto base; }
	if (streq(strbuf,"over"))     { rpn_over();             goto base; }
	if (streq(strbuf,"eval"))     { rpn_eval();             goto base; }
	if (streq(strbuf,"force"))    { rpn_force();            goto base; }
	if (streq(strbuf,"app"))      { rpn_app();              goto base; }
	if (streq(strbuf,"rap"))      { rpn_rap();              goto base; }
	if (streq(strbuf,"pin"))      { rpn_pin();              goto base; }
	if (streq(strbuf,"law"))      { rpn_law();              goto base; }
	if (streq(strbuf,"showheap")) { print_heap();           goto base; }
	if (streq(strbuf,"gc"))       { rpn_gc(8);              goto base; }
	if (streq(strbuf,"memcheck")) { memcheck_forced("rpn"); goto base; }
	if (streq(strbuf,"pdepth"))   { rpn_pdepth();           goto base; }
	if (streq(strbuf,"cords"))    { rpn_cords();            goto base; }
	if (streq(strbuf,"bitsz"))    { rpn_bitsz();            goto base; }
	if (streq(strbuf,"sub"))      { rpn_sub();              goto base; }
	if (streq(strbuf,"dec"))      { rpn_dec();              goto base; }
	if (streq(strbuf,"inc"))      { rpn_inc();              goto base; }
	if (streq(strbuf,"buffer"))   { rpn_buffer();           goto base; }
        if (streq(strbuf,"opload"))   { rpn_opload();           goto base; }

	put("unknown word: ");
	put(strbuf);
	put("\n");
	crash("bad input\n");

text:
	strbuf[i++] = c;
	if (c == '"') { loadbuf(strbuf, i-1); mode=BASE; }
	goto loop;

curl:
	if (c == '}') curldepth--;
	if (c == '{') curldepth++;
	strbuf[i++] = c;
	if (!curldepth) { loadbuf(strbuf, i-1); mode=BASE; }
	goto loop;

deci:
	mode=DECI;

	if (c == 'x' && sp[0] == 0) { mode=HEXI; goto loop; }

	if (c < '0' || c > '9') goto base;

	*(--sp) = 10;
	rpn_mul();
	*(--sp) = c - '0';
	rpn_add();
	goto loop;

base:
	mode=BASE;

	switch (c) {
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		*(--sp) = 0;
		goto deci;

	case 'a': case 'b': case 'c': case 'd': case 'e':
	case 'f': case 'g': case 'h': case 'i': case 'j':
	case 'k': case 'l': case 'm': case 'n': case 'o':
	case 'p': case 'q': case 'r': case 's': case 't':
	case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
		i=0; goto word;

	case ';':
		goto semi;

	case '"':
		i=0; strbuf[0]=0; mode=TEXT;
		break;

	case '{':
		i=0; strbuf[0]=0; curldepth=1; mode=CURL;
		break;

	case '$':
		for (i=0; i<6; i++) {
			strbuf[i] = "seeds/"[i];
		}
		strbuf[i] = 0;
		mode=SEED;
		break;

	case 'R': rpn_rap();   break;
	case 'A': rpn_app();   break;
	case 'L': rpn_law();   break;
	case 'F': rpn_force(); break;

	case 'P': rpn_pin();   break;
	case 'D': rpn_dup();   break;
	case 'E': rpn_eval();  break;

	case '\n': case ' ': case '\t': case '(': case ')':
		break;

	case '\\':
		put_bytes("\n", 1);
		break;

	case '?':
		print_stack();
		break;

	case '.':
		if (!stack_has(1)) break;
		print_value(*sp,1);
		sp++;
		break;

	case '_':
		if (!stack_has(1)) break;
		print_value(*sp,1);
		break;

	case '+': rpn_add(); break;
	case '-': rpn_sub(); break;
	case '*': rpn_mul(); break;

	case '/': rpn_div(); break;

	case '#':
		check_stack(1);
		*sp = zcall2(fastlsh, *sp, 4);
		break;

	case '<':
		check_stack(2);
		sp[1] = zcall2(fastlsh, sp[1], sp[0]);
		sp++;
		break;

	case '>':
		check_stack(2);
		zcall0(&fastrsh);
		break;

	case '|':
		check_stack(2);
		c_or();
		break;

	case '&':
		check_stack(2);
		c_and();
		break;

	case '^':
		check_stack(2);
		c_xor();
		break;

	case 0:
		goto end;

	default:
		bad_input[11] = c;
		die(bad_input);
		goto end;
	}
	goto loop;
end:
	memcheck("exit");
}

/* -------------------------------------------------------------------- */

int unittest (void) {
	test_reserve();
	test_smallops();
	test_basicadds();
	test_bigadds();
	test_bigeqs();
	test_bigmuls();
	test_bitsz();
	test_lsh();
	test_pokeop();
	test_peekop();
	test_arities();
	test_opclear();
	test_opstore8();
	test_opstore64();
	test_opstore();
	test_opsnore();
	test_opload();

	// TODO: This explicit free test broke when we started tracking GC
	// versus manually allocated objects, and needs to be rewritten.
	/* test_buddy_explicit_free(); */
	return 0;
}


// The stack and loom already setup in asm before this is called.
int rpn_main (int argc, char **argv) {
	if (argc != 1) {
		if (!streq(argv[1], "test")) return 1;
		return unittest();
	}

	bug_repl();
	return 0;
}
