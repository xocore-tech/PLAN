// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
// -*- c-basic-offset: 8; tab-width: 8; indent-tabs-mode: t -*-

#include "common.h"

#include <stdint.h>
#include <stdbool.h>
#include <stdnoreturn.h>
#include "printf.h"
#include <stdatomic.h>

#define GC_EVERY_TIME 0
#define CHECK_TRACING 0
#define TRACING       0
#define CHECKING      0

// Why "BADBEEFBADBEEF" instead of DEADBEEFDEADBEEF? Because 0x00BADBEEFBADBEEF
// is an untagged natural number due to the 0x00 in the msb, and can be spammed
// and then detected in natural number contexts like pin counts.
#define BADBEEF 0xBADBEEFBADBEEF
#define ENSURE_NOBEEF(x) \
	if (x == BADBEEF) { \
	    die("Found pointer to freed memory"); \
	}

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

extern   u64 *profile_base;

static inline u64 *PTR (Val x) {
	return (void*) ((x << 16) >> 16);
}

static inline u64 get_bitsz (u64 x) {
	u64 *p     = PTR(x);
	u64 record = p[-1];
	u64 bitsz  = record >> 8;
	return bitsz;
}

typedef struct raxrdx { u64 rax, rdx; } raxrdx;

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

static inline u64 get_array_size (u64 x) {
	u64 bitsz  = get_bitsz(x);
	u64 nodesz = bitsz >> 6;
	return nodesz - 1;
}

static inline u64 *ref (u64 x) {
	return (void*) ((x << 16) >> 16);
}

static inline u64 deref (u64 x) {
	return ref(x)[0];
}

static inline u64 *refix (u64 x, int i) {
	return &(ref(x))[i];
}

static inline u64 derefix (u64 r, int i) {
	return ref(r)[i];
}

const u64 nattag = 0x8200000000000000;
const u64 pintag = 0xC400000000000000;
const u64 lawtag = 0xC800000000000000;
const u64 clztag = 0xD000000000000000;
const u64 thktag = 0xE000000000000000;

static inline bool isdirect (u64 o) { return 0 == (o>>63); }

static inline bool isnat (u64 o) { return o < pintag; }
static inline bool ispin (u64 o) { return o >= pintag && o < lawtag; }
static inline bool islaw (u64 o) { return o >= lawtag && o < clztag; }
static inline bool isclz (u64 o) { return o >= clztag && o < thktag; }
static inline bool isthk (u64 o) { return o >= thktag; }

// TODO: Dedupe with the persist.c copy.
static inline u64 round_up_align(u64 base, u64 alignment) {
  return ((base + alignment - 1) / alignment) * alignment;
}

////////////////////////////////////////////////////////////////////////////////

typedef uint64_t u64;
typedef uint32_t u32;

#define push(w) (*(--sp) = w)
#define pop() (*(sp++))
#define drop() (sp++)

u64 arity (u64);

Val  mkclosure (u64);

////////////////////////////////////////////////////////////////////////////////

	// isnat() makes sense because:
	// - High two bits of indirect are 0b11
	// - High two bits of direct are 0b00 or 0b01.

#define isclosure(x) isclz(x)
#define isthunk(x)   isthk(x)

static inline u64 get_gc_header (u64 x) {
	return derefix(x, -1);
}

static inline u64 heapnode_bitsz (u64 x) {
	u64 header = get_gc_header(x);
	return (header >> 8);
}

static inline u8 get_gc_type (u64 x) {
	return get_gc_header(x) & 0xff;
}

static inline bool heapnode_isbin (u64 val) {
	return get_gc_type(val) == 0;
}

static inline bool heapnode_isclz (u64 val) {
	unsigned char ty = get_gc_type(val);
	return ty==3 || ty==4;
}

static inline bool heapnode_isthk (u64 val) {
	return get_gc_type(val) == 5;
}

static inline bool heapnode_islaw (u64 val) {
	return get_gc_type(val) == 2;
}

static inline bool heapnode_ispin (u64 val) {
	unsigned char ty = get_gc_type(val);
	return ty==1 || ty==6;
}

static inline bool is_closure_normalized (Val val) {
	return get_gc_type(val) == 3;
}

static inline Val unsafe_get_pin_item (Val pin) {
	return deref(pin);
}

static inline u64 get_nat_bitsz (Val nat) {
	if (nat >> 63) return get_bitsz(nat);
	return 64 - __builtin_clzll(nat);
}

void memcheck        (const char *);
void memcheck_forced (const char *);

void noreturn die   (const char*);
void          wrn   (const char*);
void          put   (const char*);

#define crash(msg) die(msg)

Val  c_eval         (Val);
Val  shrink         (u64* buf, u64 bufsz);
int  load           (const char *);
void syscall_write  (int, char*, u64);
int syscall_madvise(void* ptr, size_t len, int advice);
Val  alloc_u64      (u64);
void xdone          (Val thunk);
void xvar           (Val thunk);
void xhole          (Val thunk);
void mkapp          (void);
int  get_eval_arity (Val);
void expo           (u64 maxref, Val *env);

bool pdebug = false;
extern int  pdepth;
extern int  g_max_pdepth;

#include <assert.h>
#include <ctype.h>
#include <fcntl.h>
#include <limits.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdnoreturn.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <unistd.h>

static char buf1[128];
static char buf2[128];

char *hex = "0123456789abcdef";

noreturn void syscall_exit_group(u64);

void	c_xor          (void);
void	c_and          (void);
Val	c_div          (Val, Val);

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
void bug_word_hexi (u64 w, int newline);
void shiftl        (u64 *out, u64 *inp, u64 sz, u64 shift);


u64  heap_ptr_to_tagged_offset (u64* val);
u64* tagged_to_heap_ptr        (u64 val);

typedef struct bignat { u64 n, *p; } BigNat;

int string_length (const char *x) {
	int i = 0;
	while (x[i]) i++;
	return i;
}

u64 reserve (u64);
u64 claim   (u64);

void put_bytes (const char *msg, int sz) {
	if (pdebug)
		syscall_write(2, (char*)msg, sz);
	else
		syscall_write(1, (char*)msg, sz);
}

void put (const char *msg) {
	put_bytes(msg, string_length(msg));
}

void wrn (const char *msg) {
	syscall_write(2, (char*)msg, string_length(msg));
}

noreturn void die (const char *msg) {
	put(msg);
	syscall_exit_group(1);
}

void push_strnat (const char *str) {
	int bytes;
	for (bytes=0; str[bytes]; bytes++);

	int words = bytes+7 / 8;
	if (bytes > 8) {
		char *ptr = (char*)zcall1(reserve, words);
		for (int i=0; i<bytes; i++) ptr[i] = str[i];
		push(zcall1(claim, words));
	} else {
		u64 tmp = 0;
		char *ptr = (void*) &tmp;
		for (int i=0; i<bytes; i++) ptr[i] = str[i];
		push(tmp);
	}
}

void bug_word_base (u64 w, u64 base, int newline) {
	int n, i, sz;
	for (n=0; w; n++, w /= base) buf1[n] = hex[w % base];
	sz=n;
	if (!sz) return put(newline ? "0\n" : "0");
	for (i=0; n; i++, n--) buf2[i] = buf1[n-1];
	if (newline) buf2[i++] = '\n';
	buf2[i]   = 0;
	put_bytes(buf2, i);
}

void bug_full_word (u64 w, bool nl) {
	for (int i=0; i<16; i++, w >>= 4) buf1[15-i] = hex[w % 16];
	put_bytes(buf1, 16);
	if (nl) put("\n");
}

void bug_word_hexi (u64 w, int newline) {
	return bug_word_base(w, 16, newline);
}

void bug_word_deci (u64 w, int newline) {
	return bug_word_base(w, 10, newline);
}

#define DEBUG_MAX_DEPTH 20

int  g_max_pdepth   = 4;
bool g_print_cords  = 0;
bool g_show_bitsize = 0;
bool g_color        = 0;
int  pdepth         = 0;

void bug_bignat_hex (Val v, bool ln) {
	u64 *p = ref(v);

	u64 bitsz = get_bitsz(v);
	u64 wordsz = (bitsz + 63) / 64;

	if (g_show_bitsize) {
		put("n");
		bug_word_deci(bitsz, 0);
		put("(");
	}

	else put("0x");

	for (int i=wordsz-1; i>=0; i--) {
		if (i == wordsz-1) bug_word_hexi(p[i], 0);
		else {
			if (g_show_bitsize) put(".");
			bug_full_word(p[i], 0);
		}
	}

	if (g_show_bitsize) put(")");
	if (ln) put("\n");
}


const extern u64 heap_page_size;

void print_value (Val, bool);

void putval  (Val, bool);
void puthd   (Val);
void putitem (Val, bool);

void trace_value(Val v) {
	int olddepth = g_max_pdepth;
	bool oldcord = g_print_cords;
	g_max_pdepth = DEBUG_MAX_DEPTH;
	g_print_cords = 1;
	pdebug = 1;
	print_value(v, 1);
	g_max_pdepth = olddepth;
	g_print_cords = oldcord;
	pdebug = 0;
}

void print_value(Val v, bool ln) {
	pdepth = 0;
	putval(v,ln);
	pdepth = 0;
}

void xunknown (Val);
void xunknownnoupdate (Val);
void xhead  (Val);
void xvar  (Val);

void print_code (Val code) {
	if (code == (u64) xunknown) {
		// put(".");
	} else if (code == (u64) xhole) {
		put("H.");
	} else if (code == (u64) xvar) {
		put("V.");
	} else if (code == (u64) xdone) {
		// put("\\");
	} else if (code == (u64) xunknownnoupdate) {
		put("_");
	} else if (code == (u64) xhead) {
		put("HEAD ");
	} else {
		put("?");
	}
}

void pthk (Val v, bool ln) {
	if (pdepth >= g_max_pdepth) {
		put("(.."); goto end;
	}
	put("(");
	Val *app = ref(v);
	Val code = app[0];

	u64 record = app[-1];
	u64 nSlots = record >> 14;

	print_code(code);

	if (code == (u64) xdone) nSlots=2;

	for (int i=1; i<nSlots; i++) {
		if (i == 1) {
			pdepth--;
			putval(app[i], 0);
			pdepth++;
		} else {
			put(" ");
			putval(app[i], 0);
		}
	}
end:
	put(ln ? ")\n" : ")");
}

void pclz (Val v, bool ln) {
	int  sz = get_array_size(v);
	bool nf = is_closure_normalized(v);

	char *a = nf ? "[" : "{";
	char *z = nf ? "]" : "}";

	puthd(derefix(v,0));

	put(a);

	// unsigned char ty = get_gc_type(v);
	// put("t=");
	// putval((Val)ty, 0);
	// put(" ");

	if (pdepth >= g_max_pdepth) put("..");
	else
		for (int i=0; i<sz; i++) {
			if (i) put(" ");
			putval(derefix(v,i+1), 0);
		}
	put(z);
	if (ln) put("\n");
}

Val law_name (Val pin) { return derefix(pin, 0); }
Val law_args (Val pin) { return derefix(pin, 1); }
Val law_body (Val pin) { return derefix(pin, 2); }

void bug_law_name (Val nm) {
	bool tmp = g_print_cords;
	g_print_cords=1;
	putval(nm, 0);
	g_print_cords=tmp;
}

void bug_law (Val v, bool ln) {
	bug_law_name(law_name(v));
	put("(");
	putval(law_args(v), 0);
	put(")");
	if (pdepth < 3) {
		put("=>");
		putval(law_body(v), 0);
	}
	if (ln)
		put("\n");
}

Val pin_item    (Val pin) { return derefix(pin, 0); }
Val pin_jet     (Val pin) { return derefix(pin, 1); }
Val pin_hash    (Val pin) { return derefix(pin, 2); }
Val pin_subpins (Val pin) { return derefix(pin, 3); }

int nat_is_string (u64 o);

int word_string_length (u64 w);

int bignum_string_length (u64 o);

void bug_pin (Val v, bool ln) {
	Val i = pin_item(v);
	if (islaw(i)) {
		Val nm = law_name(i);
		if (nat_is_string(nm)) {
			if (0 == nm>>63) {
				int sz = word_string_length(nm);
				if (sz < 2) goto fallback;
				if (g_color) printf("\033[1;31m");
				put_bytes(((char*) &nm), sz);
				if (g_color) printf("\033[0m");
			} else {
				int sz = bignum_string_length(nm);
				void *np = ref(nm);
				if (g_color) printf("\033[1;31m");
				put_bytes(np, sz);
				if (g_color) printf("\033[0m");
			}
			if (ln) put("\n");
			return;
		}
	}
fallback:

	put("#");
	putitem(i, ln);
	// put(")");
	// put("PIN<");
	// putval(pin_item(v), 0);
	// put(" ");
	// putval(pin_jet(v), 0);
	// put(" ");
	// putval(pin_hash(v), 0);
	// put(" ");
	// putval(pin_subpins(v), 0);
	// put(" ");
	// bug_word_deci(arity, 0);
	// put(">");
	// if (ln) put("\n");
}

bool iswordchar (char c) {
	if (c >= '0' && c <= '9') return true;
	if (c >= 'a' && c <= 'z') return true;
	if (c >= 'A' && c <= 'Z') return true;
	if (c == '_') return true;
	return false;
}

bool isword (char *s, int sz) {
	for (int i=0; i<sz; i++)
		if (!iswordchar(s[i]))
			return false;
	return true;
}

int direct_bytes (u64 w) {
	int lz   = __builtin_clzl(w);
	int bits = 64 - lz;
	return (bits+7) / 8;
}

int word_string_length (u64 w) {
	int bytes = direct_bytes(w);

	if (bytes < 2) return -1;

	char *s = (char*) &w;
	for (int i=0; i<bytes; i++) {
		char c = s[i];
		if (c >= '!' && c <= '~') continue;
		if (c == ' ') continue;
		if (c == '\n') continue;
		return -1;
	}
	return bytes;
}

int bignum_string_length (u64 o) {
	char *buf = (void*)ref(o);
	u64 bic = get_bitsz(o);
	u64 byc = (bic + 7) / 8;
	for (int i=0; i<byc; i++) {
		char c = buf[i];
		if (c >= '!' && c <= '~') continue;
		if (c == ' ') continue;
		if (c == '\n') continue;
		return -1;
	}
	return byc;
}

int nat_is_string (u64 o) {
	return !!(o >> 63 ? bignum_string_length(o) : word_string_length(o));
}

static inline bool is_direct (Val o) {
	return (o >> 63 == 0);
}

void putval (Val v, bool ln) {
	pdepth++;
	u64 o = v;

	if (is_direct(o)) {
		int sz = word_string_length(o);
		if (g_print_cords && sz >= 1) {
			char *s = (char*) &o;
			if (isword(s, sz)) {
				if (g_color) printf("\033[1;34m");
				put("%");
				put_bytes(s, sz);
				if (g_color) printf("\033[0m");
			} else {
				if (g_color) printf("\033[1;32m");
				put("\"");
				put_bytes(s, sz);
				put("\"");
				if (g_color) printf("\033[0m");
			}
			if (ln) put("\n");
		} else {
			if (g_color) printf("\033[1;35m");
			bug_word_deci(o, ln);
			if (g_color) printf("\033[0m");
		}
		goto end;
	}

	if (!PTR(o)) {
		printf("MASK(0x%lx)%s", o >> 48, ln ? "\n" : "");
		goto end;
	}

	if (isnat(o)) {
		int sz = bignum_string_length(o);
		if (g_print_cords && sz >= 1) {
			char *s = (void*) ref(o);
			if (isword(s, sz)) {
				if (g_color) printf("\033[1;34m");
				put("%");
				put_bytes(s, sz);
				if (g_color) printf("\033[0m");
			} else {
				if (g_color) printf("\033[1;32m");
				put("\"");
				put_bytes(s, sz);
				put("\"");
				if (g_color) printf("\033[0m");
			}
			if (ln) put("\n");
		} else {
			if (g_color) printf("\033[1;35m");
			bug_bignat_hex(v, ln);
			if (g_color) printf("\033[0m");
		}
		goto end;
	}

	if (isclz(o)) pclz(v,ln);
	else if (ispin(o)) bug_pin(v,ln);
	else if (islaw(o)) bug_law(v,ln);
	else if (isthk(o)) pthk(v,ln);
	else crash("impossible");
end:
	pdepth--;
}

bool iswordchr (char c) {
	if ((c >= 'a') && (c <= 'z')) return 1;
	if ((c >= 'A') && (c <= 'Z')) return 1;
	if ((c >= '0') && (c <= '9')) return 1;
	return 0;
}

char wordbuf[9] = {0};

char* getdirectword (Val v) {
	if (v >> 63) return NULL;
	if (v == 0) return NULL;

	((Val*)wordbuf)[0] = v;

	for (int i=0;; i++) {
		char c = wordbuf[i];
		if (iswordchr(c)) continue;
		if (c) return NULL;             // non-word-char
		return wordbuf;
	}
}

void putitem (Val v, bool ln) {
	if (!pdebug) { putval(v, ln); return; }

	char *wbuf = getdirectword(v);
	if (!wbuf) { putval(v, ln); return; }
	put(wbuf);
	if (ln) put("\n");
}

void puthd (Val v) {
	if (v != 0) {
		putval(v, 0);
	}
}

void putexec (Val v) {
	void (*fp)(Val) = (void*) v;

	if (fp == xhole)
		put("&xhole\n");
	else if (fp == xdone)
		put("&xdone\n");
	else if (fp == xvar)
		put("&xvar\n");
	else if (fp == xhead)
		put("&xhead\n");
	else if (fp == xunknown)
		put("&xunknown\n");
	else if (fp == xunknownnoupdate)
		put("&xunknownnoupdate\n");
	else {
		put("bad_exec:");
		putval(v, 1);
	}
}

void putoper (Val v) {
	void (*fp)(u64) = (void*) v;

	if (fp == Judge) put("&Judge\n");
	else {
		put("oper:");
		putval(v, 1);
	}
}

void putslot(u64 ty, u64 *data, int i) {
	put("\t");
	if (ty == 5 && i==0) putexec(data[i]);
	else if (ty == 1 && i==3) putoper(data[i]);
	else if (ty == 2 && i==3) putoper(data[i]);
	else putval(data[i], 1);
}

void print_heap_node (u8 ty, u64 bitsz, u64 width, u64 *p) {
	if (bitsz < 64) {
		die("impossible: empty heap node\n");
	}

	u64 sz = (bitsz+63) / 64;

	switch (ty) {
	case 0:
		put("b[");
		bug_word_deci(sz, 0);
		put("]:");
		for (int i=0; i<sz; i++) {
			put("\t");
			bug_full_word(p[i], 1);
		}
		break;
	case 1:
		put("p[");
		bug_word_deci(sz, 0);
		put("]:");
		for (int i=0; i<sz; i++) putslot(ty, p, i);
		break;
	case 2:
		put("l[");
		bug_word_deci(sz, 0);
		put("]:");
		for (int i=0; i<sz; i++) putslot(ty, p, i);
		break;
	case 3: {
		put("kn[");
		bug_word_deci(sz, 0);
		put("]:");
		for (int i=0; i<sz; i++) putslot(ty, p, i);
		break;
	}
	case 4: {
		put("kl[");
		bug_word_deci(sz, 0);
		put("]:");
		for (int i=0; i<sz; i++) putslot(ty, p, i);
		break;
	}
	case 5: {
		put("th[");
		bug_word_deci(sz, 0);
		put("]:");
		for (int i=0; i<sz; i++) putslot(ty, p, i);
		break;
	}
	default:
		if (ty >> 7) {
			die("found MOVED heap node in live heap\n");
		}
		printf("ty=%d\n", ty);
		die("malformed GC heap node");
		break;
	}
}

void print_heap (void) {
	/*
	for (int i=0; i<heap_used_qwords; i++) {
		bug_full_word(loom_ptr[i]);
		put("\n");
	}
	*/

	u64 header, width, bitwidth;

	/*
	for (int i=survivor_live_start; i<survivor_live_next; i += width+1) {
		header   = loom_ptr[i];
		bitwidth = header >> 8;
		width = (bitwidth+63) / 64;
		print_heap_node((u8)header, bitwidth, width, i+1);
	}
	*/

	for (u64 *p = heap_addr; p<heap_next;) {
		// put("(");
		// bug_word_deci(i, 0);
		// put("<");
		// bug_word_deci(heap_used_qwords, 0);
		// put(")\n");
		header   = *p;
		bitwidth = header >> 8;
		width = (bitwidth+63) / 64;
		print_heap_node((u8)header, bitwidth, width, p+1);
		p += width+1;
	}
}

void print_heap_summary (void) {
	u64 header, width, bitwidth;
	u64 b = 0, bcount = 0, p = 0, l = 0, kn = 0, kl = 0, th = 0, total = 0;

	for (u64 *p = heap_addr; p<heap_next;) {
		bool inside_nursery = p >= heap_addr && p < heap_next;
		if (!inside_nursery) {
			continue;
		}

		header   = *p;
		bitwidth = header >> 8;
		width = (bitwidth+63) / 64;

		total += width;
		switch ((u8)header) {
		case 0:
			b += width;
			bcount++;
			break;
		case 1:
			p += width;
			break;
		case 2:
			l += width;
			break;
		case 3:
			kn += width;
			break;
		case 4:
			kl += width;
			break;
		case 5:
			th += width;
			break;
		default:
			die("malformed GC heap node");
		}
		p += width+1;
	}

	printf("heap report: b=%lu(%lu), p=%lu, l=%lu, kn=%lu, kl=%lu, th=%lu -> total=%lu\n",
	       b, bcount, p, l, kn, kl, th, total);
}

void debug_stack (const char *x) {
	wrn("[");
	if (x) wrn(x);
	wrn("]\n");
	for (Val *p=sp; p<=stack_last; p++) wrn("\t"),print_value(*p,1);
}

void debug_stack_ (char *x, int sz) {
	wrn("[");
	if (x) wrn(x);
	wrn("]\n");
	for (Val *p=sp; sz && p<=stack_last; sz--, p++) wrn("\t"),print_value(*p,1);
}


void print_stack () {
	for (Val *p=stack_last; p>=sp; p--) print_value(*p,1);
}

void silly_print_stack () {
	for (Val *p=stack_last; p>=sp; p--) bug_word_hexi(*p,1);
}

void mkapp          (void);
void eval           (void);
u64  seedfile       (const char *filename);

bool streq(const char *a, const char *b) {
loop:
	if (*a != *b) return false;
	if (*a) {
		if (*b) { a++, b++; goto loop; }
		return false;
	}
	if (*b) return false;
	return true;
}

int syscall_read(int fd, char *buf, u64 sz);


////////////////////////////////////////////////////////////////////////////////

// 0-100 percentage chance of performing a memory check.
int gc1_check_chance = 0;
int gc2_check_chance = 0;
int gc2_on_gc1_chance = 0;

static unsigned int rng_state = 1;
unsigned int rand_lcg(void) {
    rng_state = rng_state * 1103515245 + 12345;
    return rng_state;
}

int should_check(unsigned int chance) {
	if (chance == 0) {
		return 0;
	} else {
		return (rand_lcg() % chance) == 0; // 1/chance chance of firing
	}
}

void fail (char* s) {
	debug_stack("failed");
	wrn(s);
	die("FAIL\n");
}

u64 recalculate_bit_width (Val x) {
	u64 *obj = PTR(x);
	u64 head = obj[-1];
	u64 bits = head >> 8;
	u64 wsz  = (bits + 63) / 64;
	u64 msw  = obj[wsz-1];

	return ((64*wsz) - __builtin_clzll(msw));
}

#define BAD(x) { y=x; goto fail; }

// This is NOT recursive.  It just checks that the reference itself
// makes sense, and that the reference points to a heap node of the
// right type.
void refchk (Val ref, const char *where) {
	char *y = NULL;

	#if CHECK_TRACING > 2
	printf("refchk(%016lx)\n", ref);
	#endif

	if ((ref >> 63) == 0) return; // direct

	switch (ref >> 56) {
	case 0xD0: // arr
	case 0xC4: // pin
	case 0xC8: // law
	case 0xE0: // app
	case 0x82: // app
		break;
	default:
		BAD("Not a heap reference!\n");
	}

	u64 *ptr = PTR(ref);

	if (ptr == 0) return;

	u64 bitsz  = heapnode_bitsz(ref);
	u64 sz     = (bitsz+63) / 64;

	switch (ref >> 56) {
	case 0xD0: // arr
		return;
		if (!heapnode_isclz(ref))
			BAD("CHECKED closure does not point to an array.\n");
		if (sz < 2)
			BAD("CHECKED closure has size less than two (impossible!)\n");
		return;
	case 0xC4: // pin
		if (!heapnode_ispin(ref)) {
			u64 x = 1;
			x++;
			BAD("CHECKED PIN does not point to a pin node.\n");
		}
		if (sz < 6)
			BAD("CHECKED PIN does not point to array that's >= size 6.\n");
		if (ptr[2] == BADBEEF) // pin_count can never be badbeef sized.
			BAD("CHECKED PIN points to a freed pin.\n");
		return;
	case 0xC8: // law
		if (!heapnode_islaw(ref))
			BAD("CHECKED LAW does not point to a law node.\n");
		if (6 != sz)
			BAD("CHECKED LAW does not point to array of size 6.\n");
		return;
	case 0xE0: // app
		if (!heapnode_isthk(ref)) {
			heapnode_isthk(ref);
			BAD("CHECKED APP does not point to a thunk.\n");
		}
		if (sz == 0) BAD("empty thunk!");
		if (sz == 1) BAD("thunk with no params!");
		return;
	case 0x82: // app
		if (!heapnode_isbin(ref)) {
			u64 x = 0;
			x++;
			BAD("CHECKED bignum does not point to binary object.\n");
		}
		int bsz    = get_bitsz(ref);
		int expect = (bsz+63) / 64;
		if (sz != expect) {
			printf("nodesz=%lu, bitsz=%d, expect=%d\n", sz, bsz, expect);
			BAD("CHECKED nat tag does not match heap node size.\n");
		}
		int recalc = recalculate_bit_width(ref);
		u64 msw = derefix(ref, sz-1);
		if (msw == 0) {
			printf("ref=0x%lx\n", ref);
			printf("top=0x%lx\n", heap_addr);
			printf("ptr=0x%lx\n", PTR(ref));
			printf("nex=0x%lx\n", heap_next);
			printf("end=0x%lx\n", heap_end);
			printf("tag=%d, actual=%d, msw=%lu\n", bsz, recalc, msw);
			crash("Nat with 0 msw");
		}
		if (recalc != bsz) {
			BAD("CHECKED nat bit size is wrong!\n");
		}
		return;
	default:
		BAD("bad pointer tag!\n");
	}

	// Validate that this ref points into one of the valid heap areas
	u64 *p = PTR(ref);

	// bool inside_survivors = offset >= survivor_live_start &&
	                        // offset < survivor_live_next;

	bool inside_nursery = p >= heap_addr && p < heap_next;
	if (/* !inside_survivors && */ !inside_nursery) {
		BAD("CHECKED reference outside of valid areas");
	}

	return;
fail:
	fail(y);
}

void exechk (Val *p, u64 hdr, u64 v) {
	void (*fp)(Val) = (void*) v;

	if (fp == xhole) return;
	if (fp == xdone) return;
	if (fp == xvar) return;
	if (fp == xhead) return;
	if (fp == xunknown) return;
	if (fp == xunknownnoupdate) return;

	die("thunk with bad executioner!\n");
}

void check_node (Val *p, const char *where) { // , u64 ty, u64 sz, u64 hix) {
	char *y = NULL;

	#if CHECK_TRACING > 2
	unsigned long off = p - heap_addr;
	printf("check_node(off=%ld ptr=0x%lx)\n", off, (u64) p);
	#endif

	u64 hdr   = *p;
	u64 bitsz = hdr >> 8;
	u64 ty    = hdr & 0xff;
	u64 wsz   = (bitsz + 63) / 64;

	if (hdr >> 63) BAD("CHECKED heap node marked as moved (not during GC).\n");
	if (wsz >> 32) BAD("CHECKED Impossibly large heap node!\n");
	if (wsz >> 16) BAD("CHECKED Suspiciously large heap node.\n");
	if (wsz == 0)  BAD("CHECKED zero-sized heap node (impossible!)");

	u64 *data = &p[1];

	if (ty == 0) {
		u64 msw = data[wsz-1];
		if (msw == 0)
			BAD("CHECKED A binary heap node has a zero MSW.\n");
		return;
	}

	if (wsz < 2) BAD("Non-nat node with size=1\n");

	if (ty > 5) BAD("invalid gc type\n")

	if (bitsz & 0x7) BAD("Non-nat has a size that isn't a multiple of 8\n");

	if (ty == 5) exechk(p, hdr, data[0]);

	for (int i=0; i<wsz; i++) refchk(data[i], where);

	return;

fail:
	printf("MEMCHECK FAILURE (%s)\n", where);
	printf("top=0x%016lx\n", heap_addr);
	printf("ptr=0x%016lx\n", p);
	printf("nex=0x%016lx\n", heap_next);
	printf("end=0x%016lx\n", heap_end);
	printf("hdr=0x%016lx\n\n", hdr);
	printf("bsz=%lu\n", bitsz);
	printf("wsz=%lu\n", wsz);
	printf("typ=%lu\n", ty);
	fail(y);
}

void put (const char *s);

void memcheck_forced (const char *where);

void memcheck (const char *where) {
	#if CHECKING
	memcheck_forced(where);
	#endif
}

void memcheck_forced_before_gc (void) {
	memcheck_forced("GC (before)");
}

void memcheck_forced_after_gc () {
	memcheck_forced("GC (after)");
}

void before_gc (u64 needed) {
	#if TRACING > 0
	u64 used = heap_next - heap_addr;
	u64 size = heap_end  - heap_addr;
	printf("gc(used=%lu, size=%lu, needed=%lu)\n", used, size, needed);
	#endif

	if (should_check(gc1_check_chance)) {
		memcheck_forced("GC (before)");
	}
}

void request_gc2();

void after_gc (u64 needed) {
	#if GC_EVERY_TIME > 0
	heap_end = heap_next + (needed+1);
	#endif

	#if TRACING > 0
	u64 used = heap_next - heap_addr;
	u64 size = heap_end  - heap_addr;
	printf("after_gc(used=%lu, size=%lu)\n", used, size);
	#endif

	if (should_check(gc2_on_gc1_chance)) {
		printf("injected gc2 in gc1\n");
		request_gc2();
	}

	if (should_check(gc1_check_chance)) {
		memcheck_forced("GC (after)");
	}
}

void memcheck_forced (const char *where) {
	#if CHECK_TRACING > 0
	printf("MEMCHK: %s\n", where);
	#endif

	#if CHECK_TRACING > 1
	printf("STKCHK\n");
	#endif

	for (u64 *p=stack_last; p>=sp; p--) refchk(*p, where);

	#if CHECK_TRACING > 1
	printf("HEAPCHK\n");
	#endif

	#if TRACING > 2
	printf("NURSERYCHK\n");
	printf("start=%ld, end=%ld\n", 0, heap_next);
	#endif

	for (u64 *p=heap_addr; p<heap_next;) {
		u64 hdr = *p;
		u64 bsz = hdr >> 8;
		u64 wsz = (bsz+63) / 64;
		#if CHECK_TRACING > 2
		printf("p=%lx, hdr=%lx, wid=%lu\n", (u64)p, hdr, wsz);
		#endif
		check_node(p, where);
		p += wsz;
		p++;
	}

	#if CHECK_TRACING > 0
	printf("MEMCHK OK!\n");
	#endif
}

////////////////////////////////////////////////////////////////////////////////

Val	mkclz1	(Val a, Val b);
Val	mkclz2	(Val a, Val b, Val c);
Val	MkPin	(Val a);
Val	MkLaw	(Val a, Val b, Val c);
Val	NatCmp	(Val a, Val b);

Val	fastbitsz	(Val a);
Val	directbits	(Val a);
Val	fastbytsz	(Val a);


u64 strlen (const char *s) {
	int i=0;
	while (*s) i++, s++;
	return i;
}

Val str_ (u64 n, const char *s) {
	u64 words = (n+7)/8;
	u64 *p = (u64*) zcall1(reserve, words);
	for (int i=0; i<n; i++) ((char*)p)[i] = s[i];
	return zcall1(claim, words);
}

Val str (const char *s) {
	return str_(strlen(s), s);
}

////////////////////////////////////////////////////////////////////////////////

void rts_init    (void);
void arity_tests (void);
void repl        (u64 argArray);

u64* syscall_mmap(u64*, u64, u64, u64, int64_t, u64*);

void _putchar(char character) {
	syscall_write(2, &character, 1);
}

void v (Val v) {
       printf("(%lx)", v);
       print_value(v,1);
}

void sstk ()     { silly_print_stack();    }
void stk ()      { print_stack();    }
void s   (int n) { debug_stack_("STACK", n); }

////////////////////////////////////////////////////////////////////////////////

// Simplified version of buddy.c from the buddy allocator repository (neo9
// snapshot based).
//
// This is a simplification of the full threadsafe, non-stop the world
// collecting buddy allocator. We're punting on all collection and parallelism
// to first get the minimal malloc/free interface integrated. For large
// sections, I've left big ugly markers. For few lines of code which exist
// inside of existing functions, I've commented them out with /* THREAD: ... */

// Structure of a list embedded in the raw nodes.
typedef struct list_t {
  struct list_t* prev;
  struct list_t* next;
} list_t;

extern void list_init(list_t* list);
extern void list_push(list_t* list, list_t* entry);
extern void list_remove(list_t* entry);
extern list_t* list_pop(list_t* list);

extern void* ptr_for_node(size_t index, size_t bucket);
extern size_t node_for_ptr(void* ptr, size_t bucket);
extern size_t bucket_for_request(size_t request);

extern int aligned_bit_set(int bit, uint8_t *words);
extern int aligned_bit_get(int bit, uint8_t *words);
extern void project_or(u32* out, u64* in, u64 count);

extern void nodeset(int index, int tristate);
extern int nodeget(int index);


// Equivalent structure layout
typedef struct {
  u32* mutex_pointer; // qword
  u32 sequence;       // dword
  u32 padding;        // unused
} cond_t;

extern void mutex_init(u32* mutex);
extern void mutex_lock(u32* mutex);
extern void mutex_unlock(u32* mutex);
extern void condition_init(cond_t* cv, u32* mutex);
extern void condition_signal(cond_t* cv);
extern void condition_broadcast(cond_t* cv);
extern void condition_wait(cond_t* cv);
extern void rwlock_init(void* lock);
extern void rwlock_read_lock(void* lock);
extern void rwlock_read_unlock(void* lock);
extern void rwlock_write_lock(void* lock);
extern void rwlock_write_unlock(void* lock);
extern void barrier_set(int32_t* barrier, int32_t count);
extern void barrier_done(int32_t* barrier);
extern void barrier_wait(int32_t* barrier);

// Thread context structure
typedef struct {
    int32_t tid;                     // Thread ID (kernel-assigned)
    int32_t tid_padding;
    uint32_t exit_flag;          // Synchronization: 0 = running, 1 = exited
    uint32_t padding;
    void *(*fn)(void *);         // Function to execute
    void *arg;                   // Argument for the thread function
    void *result;                // Return value from the thread function
    void *stack;                 // Allocated stack memory
    size_t stack_size;           // Size of the stack
} minimal_thread_t;

int thread_create(minimal_thread_t* thread, void* (*thread_work)(void*), void* value);
void thread_join(minimal_thread_t* thread, void** result);

minimal_thread_t collector;

void* collector_thread_impl(void*);

#define HEADER_SIZE 2 // one sizeof(u64) word.

/* 12 4096 */
/* 11 2048 */
/* 10 1024 */
/* 9  512 */
/* 8  256 */
/* 7  128 */
/* 6  64 */
/* 5  32 */

// 1 page, 4096 byte minimum allocation.

/* #define MIN_ALLOC_LOG2 12ULL */
/* #define MIN_ALLOC (1ULL << MIN_ALLOC_LOG2) */

// For now, hard code 16gb.
//
// Maybe eventually, we want the max size to be configurable with an RTS flag?
// Right now, all the math is static and done at compile time, but in the long
// term, you could make this configurable; it's just more complexity during
// startup./* #define MAX_ALLOC_LOG2 34ULL */
/* #define MAX_ALLOC (1ULL << MAX_ALLOC_LOG2) */

// 32 4gb
// 33 8gb
// 34 16gb
// 35 32gb

#define UNALLOCATED 0
#define INNER_NODE 1
#define ALLOC_GC 2
#define ALLOC_MANUAL 3

const char* alloc_state_to_str(int x) {
	if (x == UNALLOCATED)
		return "UNALLOCATED";
	else if (x == INNER_NODE)
		return "INNER_NODE";
	else if (x == ALLOC_GC)
		return "ALLOC_GC";
	else if (x == ALLOC_MANUAL)
		return "ALLOC_MANUAL";
	else
		return "<INVALID>";
}

extern void* buddy_mmap_ptr;
extern list_t* buckets_ptr;
extern u8* node_state_ptr;
extern u32* bucket_locks_ptr;
extern u64 explore_max_idx;
extern u8* explore_tree_ptr;
extern u8* sweep_tree_ptr;
extern u8* new_allocation_ptr;
extern u64 bitmap_bytes;
extern int64_t sweep_level;
extern int* sweep_node_ptr;
extern u8* sweep_action_ptr;

// Beginning of arena.
extern void* base_ptr;
extern void* base_end_ptr;
extern u64 min_alloc_log2;
extern u64 min_alloc;
extern u64 max_alloc_log2;
extern u64 max_alloc;
extern u64 bucket_count;

void validate_buddy_structure();
extern void recursively_prune(int i, int bucket);
void nonrecur_mark_index(int i, int bucket, uint8_t *words);

inline static int toparent(int i) {
  return (i - 1) / 2;
}

inline static int tosibling(int i) {
  return ((i - 1) ^ 1) + 1;
}

// Returns true if the pointer is inside the buddy tree boundaries.
inline static bool in_buddy_tree(void* ptr) {
  return ptr >= base_ptr && ptr < base_ptr + max_alloc;
}

extern u64 collector_lock[7];

static inline size_t words_from_bits(u64 bitsz) {
  // Round up a bit-count to a whole number of u64 words.
  return (bitsz + 63) >> 6;
}

static int atoi(const char *str) {
    // Check for NULL or empty string
    if (str == NULL || *str == '\0') {
        return -1;
    }

    int result = 0;

    while (*str != '\0') {
        // Check if character is an ASCII digit (0-9)
        if (*str < '0' || *str > '9') {
            return -1;
        }

        int digit = *str - '0';

        // Check for overflow before multiplying
        // INT_MAX is 2147483647, so check if result * 10 + digit would overflow
        if (result > (2147483647 - digit) / 10) {
            return -1;
        }

        result = result * 10 + digit;
        str++;
    }

    return result;
}

void u64memset(u64* dest, u64 val, size_t n) {
  for (size_t i = 0; i < n; ++i) {
    dest[i] = val;
  }
}


// Concurrent garbage collection works in multiple phases.
enum CollectorState {
  // During malloc, just acquire the lock for the duration of touching the
  // freelists; do no extra work for marking.
  SLEEPING,

  // The collector has incremented the epoch number and is waiting for
  // submissions from the different malloc threads of their live sets. The
  // roots should be submitted to the `explore_tree`.
  WAITING_FOR_ROOTS,

  // The collector is currently sweeping. It's either a) projecting the sparse
  // marks of the first layer of the explore_tree into a full tree, b) walking
  // the explore tree to mark it and all dependencies into `sweep_tree`, c)
  // walking `sweep_tree` to find branches to prune.
  SWEEPING
};
// Current state of the collector. Should only be modified by the collector;
// protected with the sweep_lock.
enum CollectorState collector_state = SLEEPING;

// The "collection epoch", the number of times the collection has been run. To
// run, the collector will atomically increment this number, and will wait
// until each malloc context submits roots.
u64 collection_epoch = 0;

// The "requested epoch", the number of times the user has requested that GC2
// be run. This is lock controlled.
u64 requested_epoch = 0;

// Mutex and condition variable to control access to `requested_epoch`.
u32 requested_epoch_mutex = 0;
cond_t requested_epoch_cvar;

// (Hard coded to one until we have real thread support.)
u32 number_of_live_threads = 1;

// Barrier of how many more threads need to submit data.
int32_t remaining_submissions = 0;

// Epoch for the single main execution epoch. This eventually has to go away
// and be a per-thread structure.
u64 main_epoch = 0;


// In addition to a location, we also store an "action" per stack area to guide
// the traversal since we'll enter and exit nodes in different states.
enum SweepAction {
  // Entering a node from below.
  CURRENT = 0,

  // Action placed on the stack when traversing left. When entering a LEFT
  // node, you're coming back down the stack on the left and have to try going
  // RIGHT.
  LEFT = 1,

  // Action placed on the stack when traversing right. When entering a RIGHT
  // node, you're coming back down the stack on the right.
  RIGHT = 2
};
// enum SweepAction sweep_action[BUCKET_COUNT];

extern const char* getenveq(const char*);

u32 mutex_printf = 0;

int mprintf(const char* format, ...) {
  va_list args;
  int result;

  mutex_lock(&mutex_printf);

  va_start(args, format);
  result = vprintf(format, args);
  va_end(args);

  mutex_unlock(&mutex_printf);

  return result;
}

void init_buddy(int in_min_alloc_log2, int in_max_alloc_log2) {
  // Calculate the memory arena constants.
  min_alloc_log2 = in_min_alloc_log2;
  min_alloc = 1ULL << min_alloc_log2;
  max_alloc_log2 = in_max_alloc_log2;
  max_alloc = 1ULL << max_alloc_log2;
  bucket_count = max_alloc_log2 - min_alloc_log2 + 1;

  // The buddy allocation is
  u64 buckets_memsize = round_up_align(bucket_count * 16, 4096);
  u64 node_state_memsize = round_up_align((1ULL << bucket_count) / 4, 4096);
  u64 bucket_locks_memsize = round_up_align(bucket_count * 4, 4096);
  // sizeof(byte)
  u64 explore_tree_memsize = round_up_align((1ULL << bucket_count) / 8, 4096);
  u64 sweep_tree_memsize = explore_tree_memsize;
  u64 new_allocation_memsize = explore_tree_memsize;
  // sizeof(int)
  u64 sweep_node_memsize = round_up_align(bucket_count * 4, 4096);
  // TODO: The above isn't enough to move everything into a single mmapped
  // region. We ALSO need pages for the sweep data.
  u64 sweep_action_memsize = round_up_align(bucket_count, 4096);

  const u64 rw = 3; // PROT_READ | PROT_WRITE
  const u64 base_flags = 16418; // MAP_PRIVATE | MAP_ANON | MAP_NORESERVE
  const u64 total_size = buckets_memsize + node_state_memsize +
			 bucket_locks_memsize + explore_tree_memsize +
                         sweep_tree_memsize + new_allocation_memsize +
			 sweep_node_memsize + sweep_action_memsize + max_alloc;

  buddy_mmap_ptr = syscall_mmap(0, total_size, rw, base_flags, -1, 0);
  buckets_ptr = buddy_mmap_ptr;
  node_state_ptr = (void*)buckets_ptr + buckets_memsize;
  bucket_locks_ptr = (void*)node_state_ptr + node_state_memsize;
  explore_tree_ptr = (void*)bucket_locks_ptr + bucket_locks_memsize;
  sweep_tree_ptr = (void*)explore_tree_ptr + explore_tree_memsize;
  new_allocation_ptr = (void*)sweep_tree_ptr + sweep_tree_memsize;
  sweep_node_ptr = (void*)new_allocation_ptr + new_allocation_memsize;
  sweep_action_ptr = (void*)sweep_node_ptr + sweep_node_memsize;
  base_ptr = (void*)sweep_action_ptr + sweep_action_memsize;
  base_end_ptr = (void*)base_ptr + max_alloc;

  // Store the total number of bytes that the three bitmaps use so we can
  // quickly madvise(DONT_NEED) whenever we're finished with a collection.
  bitmap_bytes = explore_tree_memsize + sweep_tree_memsize +
		 new_allocation_memsize;

  rwlock_init(&collector_lock);

  // Initialize all bucket levels.
  for (int i = 0; i < bucket_count; ++i) {
    list_init(&buckets_ptr[i]);
    mutex_init(&bucket_locks_ptr[i]);
  }

  // Initialize the topmost bucket to contain the entirety of the memory as an
  // unallocated region.
  list_push(&buckets_ptr[0], (list_t *)base_ptr);
  nodeset(0, UNALLOCATED);

  // Initialize the mutex and condition variable used for signaling that GC2
  // should be run.
  mutex_init(&requested_epoch_mutex);
  condition_init(&requested_epoch_cvar, &requested_epoch_mutex);

  // Start the collector thread.
  thread_create(&collector, &collector_thread_impl, 0);

  // Calculate and set checking chances from environment variables.
  const char* checkchancestr = getenveq("PLANVM_CHECKCHANCE=");
  if (checkchancestr) {
	  int32_t raw_chance = atoi(checkchancestr);
	  if (raw_chance == -1 || raw_chance > 100) {
		  die("Invalid PLANVM_CHECKCHANCE value");
	  }
	  printf("All check percentages set to 1 in %d\n", raw_chance);
	  gc1_check_chance = raw_chance;
	  gc2_check_chance = raw_chance;
  }

  checkchancestr = getenveq("PLANVM_GC1_CHECKCHANCE=");
  if (checkchancestr) {
	  int32_t raw_chance = atoi(checkchancestr);
	  if (raw_chance == -1) {
		  die("Invalid PLANVM_GC1_CHECKCHANCE value");
	  }
	  printf("Check gc1 chance set to 1 in %d\n", raw_chance);
	  gc1_check_chance = raw_chance;
  }

  checkchancestr = getenveq("PLANVM_GC2_CHECKCHANCE=");
  if (checkchancestr) {
	  int32_t raw_chance = atoi(checkchancestr);
	  if (raw_chance == -1) {
		  die("Invalid PLANVM_GC2_CHECKCHANCE value");
	  }
	  printf("Check gc2 chance set to 1 in %d\n", raw_chance);
	  gc2_check_chance = raw_chance;
  }

  checkchancestr = getenveq("PLANVM_GC2_ON_GC1_CHANCE=");
  if (checkchancestr) {
	  int32_t raw_chance = atoi(checkchancestr);
	  if (raw_chance == -1) {
		  die("Invalid PLANVM_GC2_ON_GC1_CHANCE value");
	  }
	  printf("gc2 on gc1 chance set to 1 in %d\n", raw_chance);
	  gc2_on_gc1_chance = raw_chance;
  }

}


void single_thread_mark();

//
// buddy_malloc() returns a partially initialized result: while it will put the
// right gc tag and the size in the first word header, any other details about
// the returned span are *uninitialized*. The contract here is that the caller
// will have the returned memory initialized before the next root submission,
// which without very special handling, usually means by the time buddy_malloc
// is called next.
//
// request: number of u64 words
// tag: three bit tag to attach to the allocation
u64* buddy_malloc(size_t request, size_t tag) {
  size_t original_bucket, bucket, lowest_bucket;

  if (request + HEADER_SIZE > max_alloc / 8) {
    __builtin_trap();
  }

  bucket = bucket_for_request(request + HEADER_SIZE);
  original_bucket = bucket;

  // Acquire a reader lock to pause sweep. Multiple mallocs can hold the reader
  // lock at the same time.
  rwlock_read_lock(&collector_lock);

  if (should_check(gc2_check_chance)) {
    validate_buddy_structure();
  }

  // Acquire the lock for this bucket level. Reminder: the larger bucket
  // numbers correspond to smaller allocations.
  mutex_lock(&bucket_locks_ptr[bucket]);

  // Looking at the free lists, walk upwards to try to find the first one
  // bucket with a free pointer.
  u64* ptr = (u64*)list_pop(&buckets_ptr[bucket]);
  while (!ptr) {
    if (bucket == 0) {
      // There is no free space anywhere.
      for (int b = 0; b <= original_bucket; ++b) {
        mutex_unlock(&bucket_locks_ptr[b]);
      }
      __builtin_trap();
    }

    bucket--;

    mutex_lock(&bucket_locks_ptr[bucket]);
    ptr = (u64*)list_pop(&buckets_ptr[bucket]);
  }

  lowest_bucket = bucket;

  // Take the node off the free pointer list.
  size_t i = node_for_ptr(ptr, bucket);

  // If our allocation is smaller than half the node size, recursively split it
  // down and put the unused buddies in the free lists of the corresponding
  // bucket.
  while (bucket < original_bucket) {
    nodeset(i, INNER_NODE);

    i = i * 2 + 1;
    bucket++;
    list_push(&buckets_ptr[bucket], (list_t *)ptr_for_node(i + 1, bucket));
    nodeset(i + 1, UNALLOCATED);
  }

  nodeset(i, tag >= 7 ? ALLOC_MANUAL : ALLOC_GC);

  for (int b = lowest_bucket; b <= original_bucket; ++b) {
    mutex_unlock(&bucket_locks_ptr[b]);
  }

  // If the collector is active at all, it is malloc's responsibility to mark
  // its new pointer to prevent it being collected during sweep.
  enum CollectorState cur_collector_state = atomic_load(&collector_state);
  if (cur_collector_state != SLEEPING) {
	  nonrecur_mark_index(i, bucket, new_allocation_ptr);
  }

  // tag is actually 8 bits / a byte
  request <<= 14; // Convert size to bits and shift left three bits
  request |= tag;
  // Redirection header is 0
  *(u64 *)ptr = 0;
  // GC header is set to our modified request.
  *((u64*)ptr + 1) = request;

  if (should_check(gc2_check_chance)) {
	  validate_buddy_structure();
  }

  u64 *result = (u64*)ptr + HEADER_SIZE;

  rwlock_read_unlock(&collector_lock);

  return result;
}

void buddy_free(u64* ptr) {
  // Rebuild the request size from the header.
  u64 header = *((u64*)ptr - 1);
  size_t request = words_from_bits(header >> 8) + HEADER_SIZE;

  ptr -= HEADER_SIZE;

  size_t bucket = bucket_for_request(request);
  size_t i = node_for_ptr(ptr, bucket);
  size_t original_bucket = bucket;

  rwlock_read_lock(&collector_lock);

  if (should_check(gc2_check_chance)) {
	  // Prevalidate that we have the tree in a valid state before we start
	  // mucking with it.
	  validate_buddy_structure();
  }

  if (collector_state != SWEEPING) {
    // This is the fast path, which we can only run if the collector is not
    // sweeping; we don't have to do anything special and we just explicitly
    // free this segment while walking up the tree taking as few locks as
    // needed.

    // Acquire the lock for this bucket level.
    mutex_lock(&bucket_locks_ptr[original_bucket]);

    // Set ourselves to be unallocated.
    nodeset(i, UNALLOCATED);

    // Each iteration of the loop is done to check the split state as it goes
    // up.
    bool done = false;
    while (i != 0 && !done) {
      // Check the allocation status of the other. If it's UNALLOCATED, we
      // remove it to merge.
      size_t other_i = ((i - 1) ^ 1) + 1;
      int other = nodeget(other_i);
      if (other == UNALLOCATED) {
        list_remove((list_t *)ptr_for_node(other_i, bucket));

        i = (i - 1) / 2;
        nodeset(i, UNALLOCATED);

        bucket--;

        // Acquire the parent lock
        mutex_lock(&bucket_locks_ptr[bucket]);
      } else {
        done = true;
      }
    }

    // If we're checking validity at all, write BADBEEF to the beginnings of
    // the memory span.
    //
    // TODO: Remember to add a case like this in the write locked side when
    // we get there.
    //
    // TODO: Consider factoring this, the future write locked and the
    // recursively_prune_c version into one function.
    if (gc2_check_chance || gc1_check_chance) {
      u64* end = ptr_for_node(i + 1, original_bucket);
      u64 wordsz = end - ptr;
      u64 towrite = wordsz;
      if (towrite > 64) {
        towrite = 64;
      }
      u64memset(ptr, BADBEEF, towrite);
    }

    // Push the last remaining item into its freelist.
    list_push(&buckets_ptr[bucket], (list_t *)ptr_for_node(i, bucket));

    // Unlock locks.
    for (int b = bucket; b <= original_bucket; ++b) {
      mutex_unlock(&bucket_locks_ptr[b]);
    }

    // Did we screw this up?
    if (should_check(gc2_check_chance)) {
      validate_buddy_structure();
    }

    rwlock_read_unlock(&collector_lock);
  } else {
    // The system is currently sweeping. We'll have to upgrade to a hard write
    // lock to potentially modify the sweeping state, which could be anywhere
    // in the tree at this point.
    rwlock_read_unlock(&collector_lock);
    rwlock_write_lock(&collector_lock);

    // TODO: We need to BADBEEF out the freed memory on this path.
    // mprintf("slow path freeing %p\n", ptr);

    // The slow path is like the fast path except we also have to validate that
    // at each level, the current node does not interact with the current sweep
    // state. Because we're under a heavy write lock, we don't have to worry
    // about any individual locks.

    int state = nodeget(i);
    switch (state) {
    case UNALLOCATED:
	    // If we're already unallocated, then the sweeper has already freed
	    // this node.
	    break;

    case ALLOC_GC:
	    // It is always an error to try to manually free a GC object.
	    die("Manually freeing a GC object.");
	    break;

    case ALLOC_MANUAL:
	    // Set ourselves to be unallocated.
	    nodeset(i, UNALLOCATED);

	    // Each iteration of the loop is done to check for duplicates.
	    bool done = false;
	    while (i != 0 && !done) {
		    // Check if we're still sweeping (we could have finished
		    // between releasing the read lock and acquiring the write
		    // lock). If we are, check if node `i` is being visited by
		    // the sweeper at this moment. If it is, we have to reset
		    // the sweeping downwards.
		    if (sweep_level != -1 && sweep_node_ptr[bucket] == i) {
			    sweep_level = bucket - 1;
		    }

		    // Check the allocation status of the other. If it's
		    // UNALLOCATED, we remove it to merge.
		    size_t other_i = ((i - 1) ^ 1) + 1;
		    int other = nodeget(other_i);
		    if (other == UNALLOCATED) {
			    list_remove((list_t *)ptr_for_node(other_i, bucket));
			    i = (i - 1) / 2;
			    nodeset(i, UNALLOCATED);

			    bucket--;

		    } else {
			    done = true;
		    }
	    }

	    // Push the last remaining item into its freelist.
	    list_push(&buckets_ptr[bucket], (list_t *)ptr_for_node(i, bucket));
	    break;
    case INNER_NODE:
	    die("Encountered inner node during free");
    }

    // Validate that we put the tree in a valid state.
    if (should_check(gc2_check_chance)) {
      validate_buddy_structure();
    }

    rwlock_write_unlock(&collector_lock);
  }
}



// [DEBUG] Recursively validate th
void validate_node_state(int i, int bucket) {
  int t = nodeget(i);
  if (t == UNALLOCATED) {
    // Validate that the next/prev pointers are valid.
    list_t* node = ptr_for_node(i, bucket);
    if (node->prev == (list_t*)BADBEEF) {
      printf("node with bad prev: %p\n", node);
      die("BADBEEF found for UNALLOCATED prev pointer");
    }
    if (node->prev->next == (list_t*)BADBEEF) {
      die("BADBEEF found for UNALLOCATED prev->next pointer");
    }

    if (node->prev->next != node) {
      mprintf("Invalid prev pointer for index=%i, bucket=%d\n", i, bucket);
      mprintf("node=%p, node->prev=%p, node->prev->next=%p\n",
             node, node->prev, node->prev->next);
      die("Invalid prev pointer for unallocated\n");
    } else if (node->next->prev != node) {
      mprintf("Invalid next pointer for index=%i, bucket=%d\n", i, bucket);
      die("Invalid next pointer for unallocated\n");
    }

    // Verify that this unallocated node is reachable from the right bucket.
    bool found = false;
    list_t* head = &buckets_ptr[bucket];
    for (list_t* cur = head->next; cur != head; cur = cur->next) {
	    if (cur == node) {
		    found = true;
		    break;
	    }
    }
    if (!found) {
	    die("UNALLOCATED node placed in wrong bucket.");
    }
  } else if (t == ALLOC_GC || t == ALLOC_MANUAL) {
    // Allocated nodes are fine.
    u64* header = ptr_for_node(i, bucket);
    header++;
    ENSURE_NOBEEF(*header);
    u64 bitsz = *header >> 8;
    u64 nodesz = words_from_bits(bitsz) + HEADER_SIZE;

    size_t actualbucket = bucket_for_request(nodesz);
    if (actualbucket != bucket) {
      printf("readbucket: nodesz: %lu, actual bucket: %d, expected bucket: %d\n"
	    , nodesz, actualbucket, bucket);
      die("Different real bucket");
    }
  } else if (t == INNER_NODE) {
    // Validate first that both children are not UNALLOCATED. If that's true,
    // then there's a pretty big error.
    int left = nodeget(2 * i + 1);
    int right = nodeget(2 * i + 2);
    if (left == UNALLOCATED && right == UNALLOCATED) {
      die("Inner node with two Unallocated children\n");
    }

    validate_node_state(2*i + 1, bucket + 1);
    validate_node_state(2*i + 2, bucket + 1);
  } else {
    die("INVALID NODE STATE\n");
  }
}

// Validates bucket pointers separately from the node state.
//
// Makes sure that every bucket freelist is a ring list that points back to its
// head, also checking that each double link is valid.
void bucket_validate() {
  for (int bucket = 0; bucket < bucket_count; ++bucket) {
    list_t* head = &buckets_ptr[bucket];
    for (list_t* cur = head->next; cur != head; cur = cur->next) {
      ENSURE_NOBEEF((u64)cur->prev);
      ENSURE_NOBEEF((u64)cur->next);

      if (cur->prev->next != cur) {
        printf("cur=%p, cur->prev=%p, cur->prev->next=%p\n",
		cur, cur->prev, cur->prev->next);
        die("Invalid prev pointer for unallocated\n");
      }

      size_t nodei = node_for_ptr(cur, bucket);
      if (nodeget(nodei) != UNALLOCATED) {
	printf("Item in bucket allocated\n");
	die("Item in bucket allocated");
      }

      while (nodei != 0) {
	nodei = toparent(nodei);
	int ty = nodeget(nodei);
	if (ty != INNER_NODE) {
	  printf("Item in bucket is on orphaned branch (%s)\n",
		 alloc_state_to_str(ty));
	  die("Item in bucket is on orphaned branch");
	}
      }
    }
  }
}

void validate_buddy_structure() {
	bucket_validate();
	validate_node_state(0, 0);
}

/* -------------------------------------------------------------------- */

void validate_pin_contents(Val toplevel, Val item, u64* start, u64* end) {
  char *y = NULL;

  if ((item >> 63) == 0) return; // direct

  u64* ptr = PTR(item);
  if (ptr < start || ptr > end) {
    BAD("CHECKED: item in pin outside pin segment");
  }

  return;

fail:
  printf("Pin=%p, Item=%p, Start=%p, End=%p\n", toplevel, item, start, end);
  fail(y);
}

void pinvalidate(Val item) {
  refchk(item, "pinvalidate");

  // Validate this is a pin.
  if (item >> 56 != 0xC4) {
    die("Not a pin reference!\n");
  }

  u64* start = PTR(item);
  u64 bitsz  = heapnode_bitsz(item);
  u64 sz     = (bitsz+63) / 64;
  u64* end   = start + sz;

  // Recursively check each item in the pin to make sure it's memory is
  // entirely bounded by [start, end].

  validate_pin_contents(item, *start, start, end);
}

/* -------------------------------------------------------------------- */

void recur_mark_index(int i, int bucket);

void atomic_max(u64* location, u64 value);

// Exploration

// Shallow mark
//
// This mark is used to shallowly mark a pointer quickly in the `explore_tree`
// while submitting roots. It only marks the smallest allocation for that
// pointer, so it may not even mark the entire real allocation pointed to.
void shallow_mark_ptr_in_explore_tree(void* ptr) {
	if (in_buddy_tree(ptr)) {
		int bucket = bucket_count - 1;
		size_t mark_index = node_for_ptr(ptr, bucket);
		aligned_bit_set(mark_index, explore_tree_ptr);
		atomic_max(&explore_max_idx, mark_index);
	}
}

// Projects the bits which represent the root pointers submitted by the threads
// into a full explore tree, that'll be used for dependency exploration.
void project_explore_tree() {
  for (int i = bucket_count - 1; i > 5; i--) {
    // Maximum index at this level
    int shift = bucket_count - 1 - i;
    u64 max_idx_at_level = explore_max_idx >> shift;

    // Number of 64-bit words to process (round up)
    u64 len = (max_idx_at_level / 64) + 1;

    /* mprintf("projecting layer %d (len=%d)\n", i, len); */

    // For levels i >= 6, the starting byte offset is:
    //    offset(i) = 1 << (i - 3)
    // and for the parent level (i-1):
    //    offset(i-1) = 1 << ((i - 1) - 3)
    uint8_t *level_i_ptr = explore_tree_ptr + (1 << (i - 3));
    uint8_t *level_i_minus_1_ptr = explore_tree_ptr + (1 << ((i - 1) - 3));

    // Cast to the appropriate pointer types.
    u64 *input = (u64 *) level_i_ptr;
    u32 *output = (u32 *) level_i_minus_1_ptr;

    project_or(output, input, len);
  }

  for (int level = 5; level >= 0; level--) {
    int count = 1 << level;
    int level_start = (1 << level) - 1;
    /* mprintf("projecting layer %d (start=%d, count=%d)\n", */
    /*         level, level_start, count); */

    for (int j = 0; j < count; j++) {
      int parent = level_start + j;
      int left_child = 2 * parent + 1;
      int right_child = 2 * parent + 2;

      int left_val  = aligned_bit_get(left_child, explore_tree_ptr);
      int right_val = aligned_bit_get(right_child, explore_tree_ptr);
      int combined  = left_val | right_val;

      if (combined)
        aligned_bit_set(parent, explore_tree_ptr);
    }
  }
}

void explore_empty(int i, int bucket) {
	if (bucket == (bucket_count - 1)) {
		return;
	}

	int state = nodeget(i);
	if (state != UNALLOCATED) {
		die("Orphaned unallocated in exploration");
	}

      explore_empty(2 * i + 1, bucket + 1);
      explore_empty(2 * i + 2, bucket + 1);
}

// Exploration: we walk across the explore tree, searching for marked
// allocations. When we find one, mark it and all its dependencies recursively
// in the mark_bitmap.
void explore(int i, int bucket) {
  int keep = aligned_bit_get(i, explore_tree_ptr);

  /* mprintf("explore(%d, %d) = %d\n", i, bucket, keep); */
  if (keep) {
    int state = nodeget(i);

    // This node is marked as having data we want to preserve. If the node is
    // an inner node, recur downwards to see if its children need to be freed.
    if (state == INNER_NODE) {
      explore(2 * i + 1, bucket + 1);
      explore(2 * i + 2, bucket + 1);
    } else if (state == ALLOC_GC) {
      recur_mark_index(i, bucket);
    } else if (state == ALLOC_MANUAL) {
      // This is always an error because the explore tree was marked to
      // keep this item.
      die("Found manual alloc in exploration.");
    }
    // TODO: Enable this behind a user controllable debug flag. This is a sane
    // check, but it's kind of heavyweight to do all the time.
    /* else if (state == UNALLOCATED) { */
    /*   explore_empty(2 * i + 1, bucket + 1); */
    /*   explore_empty(2 * i + 2, bucket + 1); */
    /* } */
  }
}

/* -------------------------------------------------------------------- */

// Given an index location, mark it and all nodes for keep in the sweep tree,
// but do not look at the node at all or recur into it.
//
// When processing a megapin, we nonrecur on pins because the recuring work has
// already been done for us in the creation of the megapin.
void nonrecur_mark_index(int i, int bucket, uint8_t *words) {
  while (bucket >= 0) {
    int was_marked = aligned_bit_set(i, words);
    if (was_marked) {
      // If it's already marked, we have nothing else to do.
      break;
    }

    i = toparent(i);
    bucket--;
  }
}

// Given an index location, mark it and all nodes for keep in the sweep tree
// and then recur iff the index points to a
//
// This is different from the old version because it actually lets you side
// step redundant pointer operations: the sweep tree doesn't need to be
// complete from the top down like the explore tree does.
void recur_mark_index(int i, int bucket) {
  int recur_i = -1;
  int recur_bucket = -1;

  /* printf("recur_mark_index(%d, %d)\n", i, bucket); */

  while (bucket >= 0) {
    int was_marked = aligned_bit_set(i, sweep_tree_ptr);
    if (was_marked) {
      // If it's already marked, we have nothing else to do.
      break;
    }

    // If we aren't marked and we haven't already seen an ALLOCATED node and
    // we're an ALLOCATED node in the tree, we've found the real pointer with
    // size for the first time and need to recur into it.
    int nodetype = nodeget(i);
    if (nodetype == ALLOC_MANUAL) {
      die("Attempting to recur mark a manual node.");
    } else if (recur_i == -1 && nodetype == ALLOC_GC) {
      /* printf("Found allocation record at bucket %d...\n", bucket); */
      recur_i = i;
      recur_bucket = bucket;
    }

    i = toparent(i);
    bucket--;
  }

  if (recur_i != -1) {
    u64* ptr = ptr_for_node(recur_i, recur_bucket);

    // Move forward past redirection header.
    ptr++;

    // Get the tag from the ptr. We have to read a bit about the type of the
    // allocation from the heap tag because we don't necessarily have a tagged
    // pointer, and even if we do, we could have a tagged pointer into the
    // inside of a pin that isn't correct.
    u64 tag = *ptr & 0xff;

    if (tag == 0x1) {
      ptr++;

      // This is a pin, and we have to recur through every external reference.
      size_t pinsz = ptr[2];
      ENSURE_NOBEEF(pinsz);
      for (size_t i = 0; i < pinsz; ++i) {
        Val pinval  = ptr[6 + i];

        // Look at the size and calculate what bucket level this pin is
        // operating at.
        u64 bitsz   = get_bitsz(pinval);
        u64 sz      = (bitsz+63) / 64;

        // Since these pointers are pointers to pins, we know that we're
        // pointing to the start of a range so we don't have to recur from the
        // absolute bottom, treating these like they're an arbitrary pointer.
        size_t pinbucket = bucket_for_request(sz + HEADER_SIZE);
        u64* pinptr = PTR(pinval);
	if (in_buddy_tree(pinptr)) {
	  size_t pini = node_for_ptr(pinptr, pinbucket);
	  /* printf("Marking %p (naked pin)...\n", pinptr); */
	  recur_mark_index(pini, pinbucket);
	}
      }
    } else if (tag == 0x4) {
      // This is a raw closure sitting on the buddy heap. These can be
      // generated by the profiling system before delivery to the plan process.
      // We must recur through its pointers to keep everything live.
      u64 record = *ptr;
      u64 bitsz = record >> 8;
      u64 nodesz = words_from_bits(bitsz);
      ptr++;

      for (size_t i = 0; i < nodesz; ++i) {
	Val v = ptr[i];
	if (!is_direct(v)) {
	  u64* p = PTR(v);
	  if (in_buddy_tree(p)) {
	    size_t pinbucket = bucket_for_request(nodesz + HEADER_SIZE);
	    size_t pini = node_for_ptr(p, pinbucket);
	    recur_mark_index(pini, pinbucket);
	  }
	}
      }
    } else if (tag == 0x6) {
      ptr++;

      // This is a megapin and we want to nonrecur through all pins, but recur
      // through all megapins.

      size_t pinsz = ptr[2];
      ENSURE_NOBEEF(pinsz);
      for (size_t i = 0; i < pinsz; ++i) {
        // In a megapin, there are lengths for all pins and megapin entries,
        // and that means we can consult that information to not have to
        // dereference each arbitrary random pin.
        Val pinval = ptr[6 + i];
        size_t wordsz = ptr[6 + pinsz + i];

        size_t pinbucket = bucket_for_request(wordsz + HEADER_SIZE);
        u64* pinptr = PTR(pinval);
	if (in_buddy_tree(pinptr)) {
	  size_t pini = node_for_ptr(pinptr, pinbucket);

	  // The point of megapins is that we don't have to recur into each pin
	  // in the megapin.
	  /* printf("Marking %p (pin in megapin)...\n", pinptr); */
	  nonrecur_mark_index(pini, pinbucket, sweep_tree_ptr);
	}
      }

      size_t megapin_offset = 6 + pinsz * 2;
      pinsz = ptr[megapin_offset];
      ENSURE_NOBEEF(pinsz);
      megapin_offset++;
      for (size_t i = 0; i < pinsz; ++i) {
        // Repeat the same thing for all the megapins.
        Val pinval = ptr[megapin_offset + i];
        size_t wordsz = ptr[megapin_offset + pinsz + i];

        size_t pinbucket = bucket_for_request(wordsz + HEADER_SIZE);
        u64* pinptr = PTR(pinval);
	if (in_buddy_tree(pinptr)) {
	  size_t pini = node_for_ptr(pinptr, pinbucket);

	  // Recur at the megapin boundary.
	  /* printf("Marking %p (megapin in megapin)...\n", pinptr); */
	  recur_mark_index(pini, pinbucket);
	}
      }
    }
  }
}

// Recursively walk down the tree starting from bucket i, resetting the entire
// substructure. At the end of this, {i, bucket} and every child node of it
// will be UNALLOCATED in the node_state and will not appear in any freelists
// for allocation.
void recursively_prune_c(int i, int bucket) {
  int t = nodeget(i);
  nodeset(i, UNALLOCATED);
  if (t == UNALLOCATED) {
    list_t* l = ptr_for_node(i, bucket);
    /* mprintf("recursively_pruning unallocated %p\n", l); */
    // We're unallocated, meaning we have an item in a free list.
    list_remove(l);
  } else if (t == ALLOC_MANUAL) {
    die("Trying to prune a manually allocated node!");
  } else if (t == ALLOC_GC) {
    // We don't actually have to do anything in the allocated branch case
    // because the node isn't in a free list.

    if (gc2_check_chance || gc1_check_chance) {
      u64* start = ptr_for_node(i, bucket);
      u64* end = ptr_for_node(i + 1, bucket);

      // If any debug flags are set, make sure that there's still a valid header
      // here.
      u64 record = start[1];
      u64 bitsz = record >> 8;
      u64 header_wordsz = words_from_bits(bitsz);
      u64 wordsz = end - start;
      if (header_wordsz > wordsz) {
	      die("Header is larger than possible.");
      }

      // If we're debugging validity at all, fill the beginning of the
      // allocation with BADBEEF to check for bad pointers in
      // validate_node_state. Limit this to the first 64 u64 words because
      // otherwise this can get a bit expensive to leave on all the time.
      //
      // 64 words will always cover the pin header, notably the pin_count
      // (which we check all over the place because it's near free) and most of
      // the subpins in most pins.
      u64 towrite = wordsz;
      if (towrite > 64) {
        towrite = 64;
      }
      /* mprintf("Invalidating %p (%d, %d)...\n", start, i, bucket); */
      u64memset(start, BADBEEF, towrite);
    }
  } else if (t == INNER_NODE) {
    // We are an inner node. That means we aren't in the free list ourselves,
    // but some of our two children will be.
    recursively_prune_c(2*i + 1, bucket + 1);
    recursively_prune_c(2*i + 2, bucket + 1);
  }
}

// Sweep process
//
// The sweeping process considers a single node at a time so that it can take
// turns progressing with any malloc() or free() calls from the other threads.
//
// We perform sweeping with an explicit stack so that the sweep state when
// paused, the actual state of the sweep can be controlled or modified. free()
// can need to change the current state of the sweeper if it's attempting to
// explicitly free an object during sweep. All of the following structures are
// controlled by the `collector_lock`.
void sweep_step(u64* live_object_count) {
  // At every level, we unlock our sweep progress to give other threads a
  // chance to malloc or free.
  //
  // Mallocs can perform additional additive marking, but that's fine because
  // they won't be removing marks. If we've moved past the area where new
  // marks are performed during our BFS, that's fine since we won't have to
  // explore them.
  rwlock_write_lock(&collector_lock);

  if (should_check(gc2_check_chance)) {
    validate_buddy_structure();
  }

  int i = sweep_node_ptr[sweep_level];

  /* printf("Sweep level=%d, i=%d, action=%d\n", sweep_level, i, */
  /*        sweep_action_ptr[sweep_level]); */

  switch (sweep_action_ptr[sweep_level]) {
    case CURRENT: {
      int keep = aligned_bit_get(i, sweep_tree_ptr) ||
		 aligned_bit_get(i, new_allocation_ptr);
      int state = nodeget(i);

      /* printf("sweep(%d, %d) = {keep=%d, node_state=%d, ptr=%p}\n", */
      /*        i, sweep_level, keep, state, */
      /*        ptr_for_node(i, sweep_level)); */

      if (keep) {
        if (state == INNER_NODE) {
          /* printf("state == INNER NODE\n"); */
          // We are descending left.
          sweep_action_ptr[sweep_level] = LEFT;

          sweep_level++;
          sweep_node_ptr[sweep_level] = 2 * i + 1;
          sweep_action_ptr[sweep_level] = CURRENT;
        } else {
          /* printf("(other keep)\n"); */
          // If we're ALLOCATED, that's expected.
          //
          // If we are marked for keep and are UNALLOCATED, that implies
          // we're in the rare case where we have entered an explicitly
          // free()ed segment which was marked.
	  (*live_object_count)++;
          sweep_level--;
        }
      } else if (state != UNALLOCATED) {
        /* u64* start = ptr_for_node(i, sweep_level); */
        /* printf("Freeing everything under %d/%d (start ptr=%p)...\n", */
        /*        i, sweep_level, */
        /*        start); */

        /* printf("else if state != UNALLOCATED\n"); */
        // This node (or one or more childrean of it) is allocated, but it's
        // not in the mark bitmap, so free the entire tree. We must do this
        // as a single atomic operation behind the lock: this operation can
        // touch multiple freelist buckets and the state bitmap.
        recursively_prune_c(i, sweep_level);

        // The node is unallocated but is not in the bucket list. We must also
        // perform the same upwards consolidation here because there might be a
        // divergence between the keep state in the sweep_tree and an
        // explicitly free()d region which was marked.
        bool done = false;
        while (i != 0 && !done) {
          size_t other_i = tosibling(i);
          int other = nodeget(other_i);
          if (other == UNALLOCATED) {
            list_remove((list_t *)ptr_for_node(other_i, sweep_level));
            i = toparent(i);
            nodeset(i, UNALLOCATED);

            // We must also move back downwards in the sweep tree as we merge.
            sweep_level--;
          } else {
            done = true;
          }
        }

        // Push the last remaining item into its freelist.
        list_push(&buckets_ptr[sweep_level],
                  (list_t*)ptr_for_node(i, sweep_level));

        // Return down one node.
        sweep_level--;
      } else {
        /* printf("else\n"); */

        // Return down one node.
        sweep_level--;
      }

      break;
    }

    case LEFT: {
      // We just came down from exploring left. So it's now time to go
      // explore right.
      sweep_action_ptr[sweep_level] = RIGHT;

      sweep_level++;
      sweep_node_ptr[sweep_level] = 2 * i + 2;
      sweep_action_ptr[sweep_level] = CURRENT;
      break;
    }

    case RIGHT: {
      // We just came down from exploring right. So it's now time to go down
      // or end.
      sweep_level--;
      break;
    }
  }

  if (should_check(gc2_check_chance)) {
    validate_buddy_structure();
  }

  rwlock_write_unlock(&collector_lock);
}

// Given a fully saturated mark tree bitmap, prune all branches that aren't
// marked. This is designed to take turns and only progress while malloc isn't
// running.
void sweep() {
  rwlock_write_lock(&collector_lock);
  {
    sweep_level = 0;
    sweep_node_ptr[sweep_level] = 0;
    sweep_action_ptr[sweep_level] = CURRENT;
  }
  rwlock_write_unlock(&collector_lock);

  u64 live_obj_count = 0;
  while (sweep_level != -1) {
    sweep_step(&live_obj_count);
  }
  /* mprintf("live object count after sweep: %d\n", live_obj_count); */
}

// When a plan thread wants to signal to the runtime to the runtime environment
// that it wants to make GC2 run.
void request_gc2() {
	// A user thread is requesting that now is a good time to run gc2.
	mutex_lock(&requested_epoch_mutex);
	requested_epoch++;
	condition_signal(&requested_epoch_cvar);
	mutex_unlock(&requested_epoch_mutex);
}

inline u32 current_collection_epoch() {
	return atomic_load(&collection_epoch);
}

void* collector_thread_impl(void*) {
	u32 next_epoch;
	for (;;) {
		// Wait for some other thread to increment the number of epochs
		// by one or more.
		mutex_lock(&requested_epoch_mutex);
		while (requested_epoch == atomic_load(&collection_epoch)) {
			condition_wait(&requested_epoch_cvar);
		}
		next_epoch = requested_epoch;
		mutex_unlock(&requested_epoch_mutex);

		/* mprintf("[GC2] Beginning epoch %d...\n", next_epoch); */

		// Signal the beginning of collection and wait for each thread
		// to attempt to allocate and 
		rwlock_write_lock(&collector_lock);
		atomic_store(&collection_epoch, next_epoch);
		/* atomic_fetch_add(&collection_epoch, 1); */
		atomic_store(&collector_state, WAITING_FOR_ROOTS);
		barrier_set(&remaining_submissions, number_of_live_threads);
		rwlock_write_unlock(&collector_lock);

		/* mprintf("[GC2] Waiting...\n"); */

		// Wait on remaining_submissions to go to zero. Each thread
		// will check if it has to submit allocations on each explicit
		// pin malloc or on its nursery generation sweep.
		barrier_wait(&remaining_submissions);

		/* mprintf("[GC2] Setting Sweep...\n"); */

		// Move to the exploration and sweeping phase.
		rwlock_write_lock(&collector_lock);
		atomic_store(&collector_state, SWEEPING);
		rwlock_write_unlock(&collector_lock);

		// If any bottom bucket got marked, explore.
		if (explore_max_idx) {
			// Project the mark tree. Each thread submitted roots
			// by marking the most fine grain buckets; this
			// projects marking down to the roots.
			//
			// This is safe to do unlocked because mutator threads
			// only touch this while WAITING_FOR_ROOTS.
			project_explore_tree();

			// BFS across the projected explore tree to find all
			// allocations, and for each, do a recursive attempt to
			// explore in the mark tree.
			//
			// This is safe to do unlocked because mutator threads
			// never touch the sweep tree and any gc allocations
			// are purely additive to the set.
			explore(0, 0);
		}

		// Sweep across the complete set of dependencies.
		sweep();

		/* mprintf("[GC2] Ending...\n"); */

		// We are done and go back to sleep.
		rwlock_write_lock(&collector_lock);
		atomic_store(&collector_state, SLEEPING);
		rwlock_write_unlock(&collector_lock);

		// Clear the sweep tree bitmap and the explore tree bitmap. The
		// other threads only touch this buffer while WAITING_FOR_ROOTS
		// so we can clear it while waiting for the next cycle.
		explore_max_idx = 0;
		syscall_madvise(explore_tree_ptr, bitmap_bytes, MADV_DONTNEED);
	}

	return NULL;
}

extern void zero_remaining_profile();

void submit_thread_marks() {
  enum CollectorState cur_collector_state = atomic_load(&collector_state);
  if (cur_collector_state == WAITING_FOR_ROOTS) {
    u64 cur_epoch = atomic_load(&collection_epoch);

    if (main_epoch < cur_epoch) {
      single_thread_mark();
      main_epoch = cur_epoch;
    }
  }
}

// If this item is some sort of pin and is on the buddy heap, check to see if
// it has a set redirection header. If it does, use that redirection header.
void attempt_redirect(Val* valloc) {
	Val v = *valloc;
	if (!ispin(v)) {
		return;
	}
	u64* p = PTR(v);
	if (in_buddy_tree(p)) {
		u64 redirect = p[-2];
		if (redirect != 0) {
			if (redirect == (u64)-1) {
				die("TODO: Handle wait for remote sync");
			}

			u64 header = (v >> 48) << 48;
			u64 computed = redirect | header;
			/* printf("rewrote %p as %p\n", v, computed); */
			*valloc = computed;
		}
	}
}

void single_thread_mark() {
	// Validate that our heap_next is sane
	if (heap_next < heap_addr || heap_next >= heap_addr + mapd_size) {
		die("Invalid heap_next in single_thread_mark()");
	}

	/* mprintf("submitting roots\n"); */

	if (should_check(gc1_check_chance)) {
		memcheck_forced("marking");
	}

	// Mark the heap nonrecur as a "new" allocation. This prevents it from
	// being explored during the exploration phase, while still holding it
	// live.
	int bucket = bucket_count - 1;
	size_t mark_index = node_for_ptr(heap_addr, bucket);
	nonrecur_mark_index(mark_index, bucket, new_allocation_ptr);

	if (profile_base) {
		// The is currently a profile frame; it's almost certainly
		// incomplete with garbage data based [profile_next]. Pre-zero
		// it out.
		zero_remaining_profile();
		shallow_mark_ptr_in_explore_tree(profile_base);
	}

	// We want to then walk across all stack items AND all items on the
	// cheney heap looking just for references into the pin heap and mark
	// them.
	for (Val* scur = stack_last; scur >= sp; scur--) {
		Val v = *scur;
		if ((v >> 63) == 0) continue;
		attempt_redirect(scur);
		u64* ptr = PTR(*scur);
		// Be careful to not mark the current heap.
		if (ptr < heap_addr || ptr >= heap_addr + mapd_size) {
			shallow_mark_ptr_in_explore_tree(ptr);
		}
	}

	u64* hcur = heap_addr;
	while (hcur < heap_next) {
		u64 tag   = *hcur & 0xff;
		u64 bitsz = *hcur >> 8;
		u64 sz    = (bitsz+63) / 64;

		if (tag != 0x0) {
			// This heap node is a law, a clz or a thunk.
			for (size_t i = 0; i < sz; ++i) {
				Val v = hcur[1 + i];
				if ((v >> 63) == 0) continue; // direct
				attempt_redirect(&hcur[1 + i]);
				u64* ptr = PTR(v);
				if (ptr < heap_addr ||
				    ptr >= heap_addr + mapd_size) {
					shallow_mark_ptr_in_explore_tree(ptr);
				}
			}
		}

		hcur += sz;
		hcur++;
	}

	// Signal to the other thread that we have submitted.
	barrier_done(&remaining_submissions);
}

/* -------------------------------------------------------------------- */

// There are times where even interacting with the buddy allocator's bucket
// math can be a slowdown. For those times, we have a very simple bump
// allocator here which just hands out structs of a given size from pages
// fetched from the allocator. All these objects are freed together at the end
// with `bump_alloc_free()`.


#define BUMP_ALIGN 16

static inline unsigned char* slab_payload(bump_slab_t* s) {
  size_t hdr = round_up_align(sizeof(*s), BUMP_ALIGN);
  return ((unsigned char*)s) + hdr;
}

void bump_alloc_init(bump_alloc_t* a, int size_of_struct) {
  a->stride = round_up_align((size_t)size_of_struct, BUMP_ALIGN);
  a->head   = NULL;
}

static bump_slab_t* new_slab(size_t payload_cap) {
  size_t HDR = round_up_align(sizeof(bump_slab_t), BUMP_ALIGN);
  bump_slab_t* s = (bump_slab_t*)buddy_malloc(HDR + payload_cap, 0x9);
  s->next = NULL;
  s->used = 0;
  s->cap  = payload_cap;
  return s;
}

void* bump_alloc_alloc(bump_alloc_t* a) {
  if (!a || !a->stride) return NULL;
  bump_slab_t* s = a->head;
  if (!s || s->used + a->stride > s->cap) {
    // e.g. target ~ (4096-8) payload; clamp to at least one object
    size_t want = min_alloc - (HEADER_SIZE*8);
    size_t cap  = (want / a->stride) ? (want / a->stride) * a->stride : a->stride;
    bump_slab_t* n = new_slab(cap);
    n->next = s;
    a->head = s = n;
  }
  unsigned char* base = slab_payload(s);
  void* p = base + s->used;
  s->used += a->stride;
  return p;
}

void bump_alloc_free(bump_alloc_t* a) {
  if (!a) return;
  for (bump_slab_t* s = a->head; s;) {
    bump_slab_t* next = s->next;
    buddy_free((u64*)s);
    s = next;
  }
  a->head = NULL;
}

// The stack and loom already setup in asm before this is called.
int xplan_main (int argc, char **argv) {
	g_color = 1;

	const char* cmd = getenveq("PLANCMD=");
	if (cmd) {
		if (streq(cmd, "license")) {
			printf("PLAN native runtime, Version r0\n");
			printf("Copyright (C) 2026 xoCore Technologies");
		}
	}

	*(--sp) = seedfile(argv[1]);

	switch (argc) {
	case 1:
		return 1;
	case 2:
		repl(0);
		return 0;
	default:
		for (int i=argc-1; i>1; i--) push_strnat(argv[i]);
		push(0);
		u64 argVec = zcall1(mkclosure, argc-2);
		repl(argVec);
		return 0;
	}
}

/* -------------------------------------------------------------------- */

// Helper function to reset the entire buddy heap for the next test. We clearn
// the sweep tree and then perform a full sweep.
void free_entire_buddy_tree() {
	u64memset((u64*)sweep_tree_ptr, 0, (1 << bucket_count) / 64);
	sweep();
}

void test_buddy_explicit_free() {
	free_entire_buddy_tree();

	u64* a = buddy_malloc(2048, 0x009);
	u64* b = buddy_malloc(32, 0x009);
	u64* c = buddy_malloc(4096, 0x009);
	u64* d = buddy_malloc(2048, 0x009);

	validate_buddy_structure();

	// Explicitly free these in a slightly different order. The tree should
	// always be valid.
	buddy_free(c);
	validate_buddy_structure();

	buddy_free(a);
	validate_buddy_structure();

	buddy_free(b);
	validate_buddy_structure();

	buddy_free(d);
	validate_buddy_structure();
}
