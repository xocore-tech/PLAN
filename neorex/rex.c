// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
/*
    ### Input

    TODO: Consider having single-line blocks that end with a rune continue
          on to the next line.

    TODO: Consider supporting this input: ''Ugly Strings''

    TODO: Consider supporting this input ''Ugly Strings
                                           ''

    ### Testing and Cleanup:

    TODO: Do careful testing of the handling of heirs in layout mode.

    TODO: Write cleaner code for loading string literals.

    S-expression style pretty-printing.

    (? f [x y] x)

    (f 3 4 5 6)

    (f 3 4 5 6
      (^ a b c d e f g)
      7 8 9 10
      (- a b c d e f g))

    (f 3 4 5 'slug
    )

    (f 3 4 5 '''
             ugly
             ''')

    (f 3 4 5 "trad
              string")

    (f 3 4 5 'quip(foo)bar)

    Decision criteria:

    -   Is the output of printing the rex thing normal and have small output?
*/

#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// Utils ///////////////////////////////////////////////////////////////////////

#define DEBUG 0

#define debugf(...) if (DEBUG) fprintf(stderr, __VA_ARGS__);

void _Noreturn die (char *reason) {
    fprintf(stderr, "\n\nCRASH! <<<%s>>>\n\n", reason);
    exit(1);
}


// CLI Options /////////////////////////////////////////////////////////////////

typedef enum {
    CMD_LEX, CMD_NEST, CMD_SPLIT, CMD_QUIP, CMD_PARSE, CMD_CMP
} Cmd;

typedef struct { Cmd cmd; bool color; char *r1, *r2; } Config;

Config conf;


// Lexemes /////////////////////////////////////////////////////////////////////

#undef EOF

typedef enum token_type {
    BAD, EOL, EOB, EOF,
    SEMI, WYTE,
    BEGIN, END,
    RUNE, WORD, TRAD, QUIP, UGLY, SLUG
} TokenType;

typedef struct { TokenType ty; char *buf; int col, sz; } Token;

static inline Token TOK(TokenType ty, char *buf, int sz, int col) {
    return (Token){.ty=ty, .buf=buf, .sz=sz, .col=col};
}


// Rex /////////////////////////////////////////////////////////////////////////

// word       WORD
// 'quip      QUIP
// "trad"     TRAD
// ''ugly''   UGLY
// ' slug     SLUG
// (. 3 4)    BASE
// :x         PREFIX WORD
// :3.4       PREFIX TIGHT
// :(3 . 4)   PREFIX INFIX
// :(. 3 4)   PREFIX BASE

typedef enum rex_type {
    REX_WORD, REX_TRAD, REX_QUIP, REX_UGLY, REX_SLUG,
    REX_HEIR,
    CLEAR_PREFIX, CLEAR_INFIX,
    PAREN_PREFIX, PAREN_INFIX,
    CURLY_PREFIX, CURLY_INFIX,
    BRACK_PREFIX, BRACK_INFIX,
    TIGHT_PREFIX, TIGHT_INFIX,
    REX_BAD,
} RexType;

typedef struct rex_fmt {
    int  wide;   // wide output size (0 means "too big")
    bool unwrap; // can we unwrap the parens
} RexFmt ;

typedef struct rex {
    enum rex_type t;
    char         *txt;
    int           ts;
    int           ss;
    RexFmt        fmt;
    struct rex   *rs[];
} Rex;


/// Constructing Rex ///////////////////////////////////////////////////////////

static Rex *rexNZ(enum rex_type ty, char *txt, int ts, int sons) {
    Rex *rex = malloc(sizeof(Rex) + sons*sizeof(Rex*));
    rex->t          = ty;
    rex->txt        = txt;
    rex->ts         = ts;
    rex->ss         = sons;
    rex->fmt.wide   = 0;
    rex->fmt.unwrap = 0;
    return rex;
}

static Rex *rexH(Rex *hd, Rex *tl) {
    Rex *rex  = rexNZ(REX_HEIR, NULL, 0, 2);
    rex->rs[0] = hd;
    rex->rs[1] = tl;
    return rex;
}

static Rex *rexN(enum rex_type ty, char *rune, int sons) {
    return rexNZ(ty, rune, strlen(rune), sons);
}

static Rex *rex1(enum rex_type ty, char *rune, Rex *son) {
    Rex *x = rexN(ty, rune, 1);
    x->rs[0] = son;
    return x;
}

static inline int max (int a, int b) { return (a>b) ? a : b; }
static inline int min (int a, int b) { return (a<b) ? a : b; }

Rex *leaf_rex(Token tok) {
    int rty;

    switch (tok.ty) {
    case BAD:    rty=REX_BAD;  break;
    case WORD:   rty=REX_WORD; break;
    case TRAD:   rty=REX_TRAD; break;
    case QUIP:   rty=REX_QUIP; break;
    case UGLY:   rty=REX_UGLY; break;
    case SLUG:   rty=REX_SLUG; break;
    default:     die("leaf_rex: not a leaf");
    }

    int   sz  = tok.sz;
    char *p   = tok.buf;
    char *end = tok.buf+sz;
    char *out = calloc(sz+1, 1);

    switch (tok.ty) {
    case TRAD: goto trad;
    case QUIP: goto quip;
    case SLUG: goto slug;
    case UGLY: goto ugly;
    default:   goto copy;
    }

    slug: {
        sz -= 2; p += 2;
        int i=0, o=0, prefix=(tok.col + 1);
        for (; i<sz; i++,o++) {
            char c = out[o] = p[i];
            if (c == '\n') i += prefix;
        }
        sz=o;
        goto end;
    }


    // TODO: factor our the commonalities of the TRAD/QUIP/UGLY loaders.
  trad:
    {
        sz -= 2; p++; end--;
        int i=0, o=0, dent=tok.col;
        for (; i<sz; i++,o++) {
            char c = out[o] = p[i];
            if (c == '"') i++;
            if (c == '\n')
                for (int j=0; j<dent && p[i+1]==' '; j++,i++);
        }
        sz=o;
        goto end;
    }

  quip:
    {
        sz--; p++;
        int i=0, o=0, dent=tok.col-1;
        for (; i<sz; i++,o++)
            if ((out[o]=p[i]) == '\n')
                for (int j=0; j<dent && p[i+1]==' '; j++,i++);
        sz=o;
        goto end;
    }

  ugly:
    {
        int i, o, dsz=0, dent=tok.col-1;
        while (p[dsz] == '\'') dsz++;
        p+=(dsz+1), sz-=(dsz*2 + 2 + dent);
        for (i=dent,o=0; i<sz; i++,o++)
            if ((out[o]=p[i]) == '\n')
                for (int j=0; j<dent && p[i+1]==' '; j++,i++);
        sz=o;
        goto end;
    }

  copy:
    memmove(out, p, sz);
    debugf("LEAF:|%s|\n", out);

  end:
    return rexNZ(rty, out, sz, 0);
}


// Rune Precedence /////////////////////////////////////////////////////////////

static const char *runeseq = ",:#$`~@?\\|^&=!<>+-*/%!.";

static int runeprec (char c) {
    int j=0;
    while (runeseq[j] && c != runeseq[j]) j++;
    return j;
}

static uint64_t packrune (const char *str, int sz) {
    if (sz > 13) die("can't pack wide rune");

    uint64_t place  = 1;
    uint64_t result = 0;
    for (int i=0; i<13; i++) {
        int code = i<sz ? runeprec(str[i]) : 23;
        result += place*code;
        place *= 24;
    }
    return result;
}

static int runecmp (const char *a, const char *b) {
    uint64_t aw = packrune(a, strlen(a));
    uint64_t bw = packrune(b, strlen(b));

    if (aw < bw) return -1;
    if (aw > bw) return 1;
    return 0;
}

static int runecmp_ (const void *a, const void *b) {
    return runecmp(*(const char**)a, *(const char**)b);
}

void unpack_rune (char *out, uint64_t rune) { // only needed for testing.
    for (int i=0; i<12; i++) {
        int code = rune % 24;
        out[i] = runeseq[code];
        rune = rune / 24;
    }
}

static bool isrune (char c) {
    for (int i=0; i<23; i++) if (c == runeseq[i]) return 1;
    return 0;
}



// Formatting //////////////////////////////////////////////////////////////////

/*
    Base Printer Forms

        x   -- word
        q   -- quip
        s   -- slug
        t   -- trad
        u   -- ugly
        x·y -- heir
        a+b -- infix
        +a  -- prefix
        ()  -- wrap

    When do heir elements need to be wrapped?

        These lexical comabinations can't be safely juxtaposed:

            x·x
            +·+
            ""·""
            ''x''·''x''
            'q·*        (except 'q'q and 'q''u'' and 'q' slug)

        These tree-types can't be safely juxtaposed without getting
        a different tree shape.

            a·+x    (heir becomes infix)
            a·x+y   (heir attaches to x, not x+y)
            a·(b·c) (heir order gets inverted)
            +·*     (becomes prefix BUT THIS CASE NEVER HAPPENS)
            *·+     (gets separated BUT THIS CASE NEVER HAPPENS)

        This doesn't seem like it covers everything, what are the
        other cases?

            q+   -- this one is okay.
            x+3  -- this one is okay.
            q+3  -- this just becomes a big quip.
            xq+3 -- this is a problem too.

    Which side of an heir gets the wrapping?

        If the head is a quip, always wrap the head.
        If only one side is multi-line, wrap that one.
        If only one side is long, wrap that one.
        Otherwise, wrap the right one.

    What if we ask the opposite question?


When is it safe to unwrap a node?

    First, here is a concice notation for the node types.

        x q s t u ab a+b +a (a) (+ a b) (a + b)

    And these mean:

        |  w WORD  |  ab      JUXT "juxtaposed with"  |
        |  t TEXT  |  a+b     ROPE "tight infix"      |
        |  q QUIP  |  +a      TACK "tight prefix"     |
        |  u UGLY  |  (a + b) IFIX "nest infix"       |
        |  s SLUG  |  (+ a b) NEST "nest prefix"      |

	Rules for unwrapping within juxt form.

        ; Always unwrap isolated nodes (except for the nest forms)

        w t q u s ab a+b +a

        ; These types are paren-safe.  They can be unwrapped if juxtaposed
        ; with a wrapped form.

        w() t() --- u() --- ab()
        ()w ()t ()q ()u ()s

        ; This combinations are fully safe.  If juxtaposed, both sides
        ; may be unwrapped.

            / -w -t -q -u -s
        w-  | -- wt wq wu ws
        t-  | tw -- tq tu ts
        q-  | -- -- qq qu qs
        u-  | uw uw -- -- --
        s-  | -- -- -- -- --

        Finally, the (c) in ab(c) can be unwrapped if bc could be
        unwrapped




	Here's a compressed version of the unwrapping rules for JUXT forms:

	First, if the head of the JUXT is an JUXT (ab)c, this unwraps
	left if bc unwraps left, otherwise neither side unwraps.

	Otherwise, lookup the combination in this table.

           | -w -t -q -u -s -_
        w- | || && && && && <<
        t- | && || && && && <<
        q- | >> >> && && && --
        u- | && && || || || <<
        s- | >> >> >> >> >> --
        _- | -- -- -- -- -- --

    >> means unwrap the RHS, << the LHS, && means unwrap both, || means
    that either can be unwrapped but not both, and a blank means that
    no unwrapping is possible.

    For example, if you see (foo)(bar), you lookup ww and determine
    that must onwrap only one of the two.  But for (foo)("bar"), both
    are unwrapped.  If you see (+3)(3 + 4), that falls under __, and
    neither side unwraps.


    (a' slug
    ))( 'slug
      )


    ( ' slug
    )a"t"

    And (ab)c unwraps both ways if bc unwraps both ways.

        ab('q)

        a"t"c

        (a"t")('q)
        (ab)(cd) => ab(cd)

    ''hi''
    ''hi''('q)
    (''hi'')' slug

    Note that these rules are only about unwrapping within juxt forms,
    not within other types of forms.

    These rules do not talk about cases like this:

        (-3)(.4)+(.5)(.6)

    Or this:

        -(-3)

    How about in prefix forms?  Here, only the tail may need to be
    wrapped.

        These unwrap

            +w +t +q +u +s +3.4 +ab

        TODO: does +ab always unwrap?  If a starts with a rune, then it
        would still be wrapped as +(a)b, right?

        These remain wrapped:

            +(+3) +(+ 3 4) +(3 + 4)

    How about tight infix forms:

        Tight-infix unwraps within tight infix if the precidence is lower.

        tight-prefix never unwraps within tight infix.

        unwraps:

            w+w+w
            t+t+t
            u+u+u
            ab+ab+ab

        does not unwrap:

            +'foo
            'q+(non-quip)

        x q s t u ab a+b +a (a) (+ a b) (a + b)

        These unwrap:

            w+w "t"+"t" 'q+'q 'q+''ugly'' 'q+' slug ...TODO

        These remain wrapped:

            ('q)+w (s)+w
*/


/*
           | -w -t -q -u -s -_
        w- | || && && && && <<
        t- | && || && && && <<
        q- | >> >> && && && --
        u- | && && || || || <<
        s- | >> >> >> >> >> --
        _- | -- -- -- -- -- --
*/

static const char *heir_unwrap_table =
   //wtqus(ab)
    "|&&&&<" // w-
    "&|&&&<" // t-
    ">>&&&-" // q-
    "&&|||<" // u-
    ">>>>-<" // s-
    "------";// (ab)-

static FILE *rf = NULL;
static int   rd = 0;
void prex (Rex *r);
void pwrapped (Rex *r);

static char heir_unwrap_dir(Rex *hd, Rex *tl) {
    Rex *top = hd;
    bool cache = top->fmt.unwrap;
    top->fmt.unwrap = true;
    while (hd->fmt.unwrap && hd->t == REX_HEIR) { hd = hd->rs[1]; }
    top->fmt.unwrap = cache;
    int hi = min(hd->t, 5);
    int ti = min(tl->t, 5);
    int i = hi*6 + ti;
    return heir_unwrap_table[i];
}

Rex *trailing (Rex *x) {
  loop:
    if (x->fmt.unwrap && x->t == TIGHT_INFIX) { x = x->rs[x->ss-1]; goto loop; }
    if (x->fmt.unwrap && x->t == REX_HEIR)    { x = x->rs[1];       goto loop; }
    return x;
}

Rex *leading (Rex *x) {
  loop:
    if (x->fmt.unwrap && x->t == TIGHT_INFIX) { x = x->rs[0]; goto loop; }
    if (x->fmt.unwrap && x->t == REX_HEIR)    { x = x->rs[0]; goto loop; }
    return x;
}

static bool trailing_rune (Rex *x) {
    x = trailing(x);
    return ( x->fmt.unwrap &&
             x->t == REX_QUIP &&
             isrune(x->txt[x->ts - 1])); }

static bool leading_tick  (Rex *x) {
    x = leading(x);
    return ( x->fmt.unwrap &&
             ( x->t == REX_QUIP ||
               x->t == REX_SLUG ||
               x->t == REX_UGLY )); }

static bool trailing_quip (Rex *x) { return (trailing(x)->t == REX_QUIP); }
static bool trailing_slug (Rex *x) { return (trailing(x)->t == REX_SLUG); }

static void frex(Rex *r) {
    Rex **sons = r->rs;
    int ss = r->ss;
    for (int i=0; i<r->ss; i++) frex(sons[i]);

    switch (r->t) {
    case CURLY_PREFIX:
    case BRACK_PREFIX:
    case CLEAR_PREFIX:
    case PAREN_PREFIX:
    case CURLY_INFIX:
    case BRACK_INFIX:
    case CLEAR_INFIX:
    case PAREN_INFIX: {
        int w = 3 + r->ts;  // parens + space + rune
        for (int i=0; i<ss; i++) {
            Rex *s = sons[i];
            s->fmt.unwrap = 1; // everything can be unwrapped in PAREN_PREFIX mode.
            if (s->t == REX_SLUG) s->fmt.unwrap = 0;
            w += 1 + s->fmt.wide;
            if (s->fmt.wide == 0) { w+=40; }
        }
        r->fmt.wide = (w>40) ? 0 : w;
        break;
    }

    case TIGHT_INFIX: {
        uint64_t or = packrune(r->txt, r->ts);
        if (ss < 2) { r->t = PAREN_PREFIX; frex(r); }
        int w=0;
        Rex *next = NULL;
        for (int i=(ss-1); i>=0; i--) {
            Rex *s = sons[i];
            { w += s->fmt.wide ? s->fmt.wide : 40;
              if (next) w += r->ts;
            }
            bool u = true;
            if (!s->fmt.wide)                                         u=0;
            if (s->t == TIGHT_INFIX && packrune(s->txt, s->ts) <= or) u=0;
            if (s->t == TIGHT_PREFIX)                                 u=0;
            if (next && trailing_rune(s))                             u=0;
            if (next && trailing_slug(s))                             u=0;
            if (next && trailing_quip(s) && !leading_tick(next))      u=0;
            s->fmt.unwrap = u;
            if (!u) w += 2;
            next = s;
        }
        r->fmt.wide = (w>40) ? 0 : w;
        break;
    }

    case TIGHT_PREFIX: {
        if (ss != 1) { r->t=PAREN_PREFIX; return frex(r); }

        int wd = sons[0]->fmt.wide;
        if (wd) wd += strlen(r->txt);
        r->fmt.wide = wd;

        sons[0]->fmt.unwrap = (sons[0]->t != TIGHT_PREFIX);

        break;
    }

    case REX_WORD:
        r->fmt.wide = r->ts;
        break;

    case REX_TRAD:
        r->fmt.wide = r->ts; // TODO: escaping
        break;

    case REX_HEIR:
        // TODO: add a special-case where prefix forms are rendered as {}
        // or [].

        Rex *hd = sons[0];
        Rex *tl = sons[1];
        char c;
        switch (c = heir_unwrap_dir(hd, tl)) {
        case '<': hd->fmt.unwrap = 1; break;
        case '>': tl->fmt.unwrap = 1; break;
        case '&': hd->fmt.unwrap = tl->fmt.unwrap = 1; break;
        case '|': hd->fmt.unwrap = 1; break;
        case '-': break;
        default: debugf("BAD unwrap direction: %c\n", c); die("no more!");
        }

        {
            int wd = 0;
            int hw = hd->fmt.wide, tw = tl->fmt.wide;
            if (hw && tw) {
                wd = hw+tw;
                if (! hd->fmt.unwrap) wd+=2;
                if (! tl->fmt.unwrap) wd+=2;
            }
            if (wd>40) wd=0;
            r->fmt.wide = wd;
            break;
        }

    // TODO: Tight Infix
    // TODO: Tight Prefix
    default:
    }
}

// Printing ////////////////////////////////////////////////////////////////////

static void red      (FILE *f) { if (conf.color) fprintf(f, "\033[1;31m");     }
static void blue     (FILE *f) { if (conf.color) fprintf(f, "\033[1;34m");     }
static void cyan     (FILE *f) { if (conf.color) fprintf(f, "\033[1;36m");     }
static void yellow   (FILE *f) { if (conf.color) fprintf(f, "\033[1;33m");     }
static void gold     (FILE *f) { if (conf.color) fprintf(f, "\033[38;5;178m"); }
static void magenta  (FILE *f) { if (conf.color) fprintf(f, "\033[1;35m");     }
static void gray     (FILE *f) { if (conf.color) fprintf(f, "\033[1;90m");     }
static void graybg   (FILE *f) { if (conf.color) fprintf(f, "\033[100m");      }
static void green    (FILE *f) { if (conf.color) fprintf(f, "\033[1;32m");     }
static void bold     (FILE *f) { if (conf.color) fprintf(f, "\033[1m");        }
static void reset    (FILE *f) { if (conf.color) fprintf(f, "\033[0m");        }

static int w_col = 0;
static int w_depth = 0;

static void align () {
    for (; w_col<w_depth; w_col++) fputc(' ', rf);
}

static void wchar (char c)  { align(); fputc(c, rf); w_col++;    }
static void wstr  (char *s) { align(); while (*s) wchar(*(s++)); }
static void wgap  (void)    { align(); wchar(' ');               }
static void wline ()        { wchar('\n'); w_col=0;              }

static void inline print_open_paren (void) {
    blue(rf); wchar('('); reset(rf);
}

static void inline print_open_curl (void) {
    blue(rf); wchar('{'); reset(rf);
}

static void inline print_open_bracket (void) {
    blue(rf); wchar('['); reset(rf);
}

static void inline wrune (char *txt, int sz) {
    align();
    bold(rf); gold(rf); fwrite(txt, 1, sz, rf); reset(rf);
    w_col += sz;
}

static void inline print_word (char *txt, int sz) {
    align();
    fwrite(txt, 1, sz, rf);
    w_col += sz;
}

static void inline print_quip (char *txt, int sz) {
    int d0 = w_depth;
    w_depth = max(w_depth, w_col);
    cyan(rf);
    if (!sz) wstr("(')");
    else {
        wchar('\'');
        for (int i=0; i<sz; i++) {
            if (txt[i] == '\n') wline();
            else wchar(txt[i]);
        }
    }
    reset(rf);
    w_depth = d0;
}

static void inline print_close_paren (void) {
    blue(rf); wchar(')'); reset(rf);
}

static void inline print_close_bracket (void) {
    blue(rf); wchar(']'); reset(rf);
}

static void inline print_close_curl (void) {
    blue(rf); wchar('}'); reset(rf);
}

enum cluster_elem_type { CLEM_RUNE, CLEM_REX };

typedef struct cluster_elem {
    enum cluster_elem_type ty;
    union { Rex *rex; char *txt; };
} Clem;

static void pwrap_wide (char *az, int sz, Clem *cs) {
    if (az) { blue(rf); wchar(az[0]); reset(rf); }

    for (int i=0; i<sz; i++) {
        Clem c = cs[i];
        if (i) wgap();
        if (c.ty == CLEM_RUNE) wrune(c.txt, strlen(c.txt));
        else pwrapped(c.rex);
    }

    if (az) { blue(rf); wchar(az[1]); reset(rf); }
}

static void pwrap_tall (char *az, int sz, Clem *cs) {
    int d0 = w_depth;
    int d1 = max(w_col, d0);
    w_depth = d1;

    if (az) { blue(rf); wchar(az[0]); reset(rf); }

    bool special=true; // TODO: I forget wtf is this

    for (int i=0; i < sz; i++) {
        Clem c = cs[i];

        if (i==0) {
            if (c.ty == CLEM_RUNE) {
                int rsz = strlen(c.txt);
                wrune(c.txt, rsz); wgap(); // just in case
                w_depth = d1 + (rsz + 2);
                continue;
            } else {
                w_depth = d1+2;
            }
        }

        if (c.ty == CLEM_REX) {
            if (special) { special=0; pwrapped(c.rex); }
            else { wline(); pwrapped(c.rex); }
            continue;
        }

        if (i) wgap();
        else special=1;

        wrune(c.txt, strlen(c.txt)); wgap();
    }

    if (az) {
        w_depth = d1;
        wline();
        blue(rf); wchar(az[1]); reset(rf);
    }
    w_depth = d0;
}


/*
    (| leaf leaf leaf
      (| rex)
      leaf leaf leaf
      (| rex))

    (| a b c
      (d
        )(? (f x) x)
      leaf)
*/

/*
    printing a heir?

        (
        print head (with d = d+1)
        newline
        )
        print tail (with d = d+1)

    printing rex?

      if output col is too small?  indent

    printing a node?

      print '('
      if rune, print it

        for each son:
            if isnode?
                newline
                d = d0+2;
                print the node

            if isheir?
                newline
                print the heir

            if isleaf?
                last things was a leaf? space, otherwise newline
                print the leaf

    done?
        print ')'
*/

#define streq(a,b) (!strcmp(a,b))

static void pwrap(int w, char *az, int nc, Clem *cs) {
    if (w) pwrap_wide(az, nc, cs);
    else pwrap_tall(az, nc, cs);
}

static void prefix_wrapped (int w, char *az, char *rune, int sons, Rex **ss) {
    int nc = sons + (rune ? 1 : 0);
    Clem cs[nc];
    int o=0;

    if (rune)
        cs[o++] = (Clem){.ty = CLEM_RUNE, .txt = rune};

    for (int i=0; i<sons; i++)
        cs[o++] = (Clem){.ty = CLEM_REX, .rex = ss[i]};

    pwrap(w, az, nc, cs);
}

void nest_infix_tall (char *az, char *rune, int sons, Rex **ss) {
    int d0 = w_depth;
    int d1 = w_depth = max(w_depth, w_col);

    { blue(rf); wchar(az[0]); reset(rf); }

    int rsz   = strlen(rune);
    int delem = d1 + rsz + 1;
    int drune = d1;

    // Save space by dedenting the wide runes if possible.
    if (rsz > 1 && w_depth > rsz+1) {
        drune -= rsz-1;
        delem -= rsz-1;
    }

    for (int i=0; i < sons; i++) {
        if (i) { w_depth=drune; wline(); wrune(rune, rsz); }
        w_depth=delem; pwrapped(ss[i]);
    }

    if (sons==1) {
        w_depth=drune;
        wline();
        wrune(rune, rsz);
    }

    wline();
    w_depth=d1; { blue(rf); wchar(az[1]); reset(rf); }
    w_depth=d0;
}

void nest_infix (char *az, RexFmt fmt, char *rune, int sons, Rex **ss) {
    int w = fmt.wide;

    if (sons == 0)
        return prefix_wrapped(w, az, rune, sons, ss);

    if (!w) return nest_infix_tall(az, rune, sons, ss);

    Clem cs[sons*2];
    int o=0;

    for (int i=0; i<sons; i++) {
        cs[o++] = (Clem){.ty = CLEM_REX, .rex = ss[i]};
        if (i == 0 || i+1 < sons)
            cs[o++] = (Clem){.ty = CLEM_RUNE, .txt = rune};
    }

    pwrap(w, az, o, cs);
}

void pwrapped (Rex *r) {
    switch (r->t) {
    case CLEAR_PREFIX:
    case PAREN_PREFIX:
    case CLEAR_INFIX:
    case PAREN_INFIX:
        prex(r); // main printer already paren-wraps.
        return;
    default:
        break;
    }

    if (r->fmt.unwrap) {
        prex(r);
    } else if (r->fmt.wide) {
        print_open_paren();
        prex(r);
        print_close_paren();
    } else {
        print_open_paren();
        wgap();
        prex(r);
        wline();
        print_close_paren();
    }
}

int ugly_delim_size (int sz, char *b) {
    int width=1, count=0;

    for (int i=0; i<sz; i++) {
        if (b[i] == '\'') { count++; continue; }
        if (count) {
            width = max(count, width);
            count = 0;
        }
    }

    return max(count, width) + 1;
}

void prex (Rex *r) {
    if (!r) return;

    switch (r->t) {
    case REX_HEIR: {
        int d0 = w_depth;
        w_depth = max(d0, w_col);
        pwrapped(r->rs[0]);
        w_depth++;
        pwrapped(r->rs[1]);
        w_depth = d0;
        return;
    }

    case TIGHT_PREFIX:
        wrune(r->txt, r->ts);
        pwrapped(r->rs[0]);
        return;

    case CURLY_INFIX:
        nest_infix("{}", r->fmt, r->txt, r->ss, r->rs);
        return;

    case BRACK_INFIX:
        nest_infix("[]", r->fmt, r->txt, r->ss, r->rs);
        return;

    case PAREN_INFIX: case CLEAR_INFIX:
        nest_infix("()", r->fmt, r->txt, r->ss, r->rs);
        return;

    case TIGHT_INFIX: {
        for (int i=0; i<r->ss; i++) {
            pwrapped(r->rs[i]);
            if (i+1 < r->ss) wrune(r->txt, r->ts);
        }
        return;
    }

    case PAREN_PREFIX: case CLEAR_PREFIX: {
        char *rune = r->txt;
        if (r->ss != 1 && streq(rune, "`")) rune=NULL;
        prefix_wrapped(r->fmt.wide, "()", rune, r->ss, r->rs);
        return;
    }

    case BRACK_PREFIX: {
        char *rune = r->txt;
        if (streq(rune, "`")) rune=NULL;
        prefix_wrapped(r->fmt.wide, "[]", rune, r->ss, r->rs);
        return;
    }

    case CURLY_PREFIX: {
        char *rune = r->txt;
        if (streq(rune, "`")) rune=NULL;
        prefix_wrapped(r->fmt.wide, "{}", rune, r->ss, r->rs);
        return;
    }

    case REX_WORD:
        print_word(r->txt, r->ts);
        return;

    case REX_QUIP:
        print_quip(r->txt, r->ts);
        return;

    case REX_BAD:
        red(rf);
        wstr("BAD:");
        reset(rf);
        goto rexstr;

    case REX_SLUG: {
        yellow(rf);
        int d=w_depth;
        w_depth = max(d, w_col);
        int i=0;
        int remain = r->ts;

      line:
        if (!remain) { wchar('\''); wline(); return; }

        wchar('\'');
        if (r->txt[i] != '\n') wchar(' ');

        while (remain) {
            if (r->txt[i] == '\n') { wline(); i++; remain--; goto line; }
            wchar(r->txt[i]);
            i++, remain--;
        }
        reset(rf);
        w_depth=d;
        return;
    }

    case REX_TRAD:
    rexstr: {
        int d0 = w_depth;
        magenta(rf);
        wchar('"');
        w_depth = w_col;
        for (int i=0; i < r->ts; i++) {
            char c = r->txt[i];
            switch (c) {
            case '"':  wstr("\"\""); break;
            case '\n': wline();      break;
            default:   wchar(c);     break;
            }
        }
        wchar('"');
        reset(rf);
        w_depth = d0;
        return;
    }

    case REX_UGLY: {
        int d0 = w_depth;
        w_depth = max(w_depth, w_col);
        yellow(rf);
        int dw = ugly_delim_size(r->ts, r->txt);
        for (int i=0; i<dw; i++) { wchar('\''); } wline();
        for (int i=0; i<r->ts; i++) {
            if (r->txt[i] == '\n') wline();
            else wchar(r->txt[i]);
        }
        wline(); for (int i=0; i<dw; i++) { wchar('\''); }
        reset(rf);
        w_depth = d0;
        return;
    }

    default:
        wstr("<bad-rex-tag>");
    }
}

static inline void print_rex(FILE *f, Rex *r) {
    rf=stderr;
    frex(r);
    w_depth=4; w_col=0; rd=0; rf=f;
    prex(r);
}


// Constructing Infix/Prefix/Shut Forms ////////////////////////////////////////

char cabtxt[2] = { '_', 0 };

Token cab_token = (Token){ .ty=WORD, .buf=cabtxt, .sz=1, .col=0, };

Rex cab_rex = (Rex){ .t=REX_WORD, .txt=cabtxt, .ts=1, .ss=0 };

Clem cab_elem = (Clem){ .ty=CLEM_REX, .rex=&cab_rex };

static Rex *infix_recur
    (enum rex_type rty, int nRune, char **runes, Clem *buf, int off, int sz)
{
    if (sz == 1) return buf[off].rex;

    if (nRune == 0) {
        Rex *res = rexN(CLEAR_PREFIX, "`", sz);
        for (int i=0; i<sz; i++) res->rs[i] = buf[i+off].rex;
        return res;
    }

    Rex *kids[sz];
    int nKid = 0;

    for (int i=0; sz>0; i++) {
        // find the next matching rune (or ix=sz)
        int ix = 0;
        for (; ix<sz; ix++) {
            Clem c = buf[off+ix];
            if (c.ty != CLEM_RUNE) continue;
            if (0 != strcmp(runes[0], c.txt)) continue;
            break;
        }

        // Process the section until then (dropping this rune);
        kids[nKid++] = infix_recur(rty, nRune-1, runes+1, buf, off, ix);

        // Repeat the process on everything after that rune.
        off += (ix+1);
        sz -= (ix+1);
    }

    if (nKid==1 && sz) return kids[0];

    Rex *r = rexN(rty, runes[0], nKid);
    for (int i=0; i<nKid; i++) r->rs[i] = kids[i];
    return r;
}

static Rex *infix_rex (enum rex_type rty, Clem *input, int sz) {

    // Collect all of the runes.

    char *runes[128] = {0};
    int nRune=0;

    for (int i=0; i<sz; i++) {
        if (input[i].ty != CLEM_RUNE) continue;
        runes[nRune++] = input[i].txt;
    }

    // Sort the runes by precidence and deduplicate.

    if (nRune) {
        qsort(runes, nRune, sizeof(char*), runecmp_);

        int uniq = 1;
        for (int i=1; i<nRune; i++) {
            if (0 != strcmp(runes[i], runes[i-1])) {
                runes[uniq++] = runes[i];
            }
        }
        nRune = uniq;
    }

    // Perform the actual recursive infix logic.

    return infix_recur(rty, nRune, runes, input, 0, sz);
}

RexType infix_color (char nestTy) {
    switch (nestTy) {
    case '(': return PAREN_INFIX;
    case '[': return BRACK_INFIX;
    case '{': return CURLY_INFIX;
    default:  die("impossible: bad nest tag");
    }
}

RexType prefix_color (char nestTy) {
    switch (nestTy) {
    case '(': return PAREN_PREFIX;
    case '[': return BRACK_PREFIX;
    case '{': return CURLY_PREFIX;
    default:  die("impossible: bad nest tag");
    }
}

Rex *color (char nestTy, Rex *p) {
    switch (p->t) {
    case CLEAR_PREFIX: p->t = prefix_color(nestTy); return p;
    case CLEAR_INFIX:  p->t = infix_color(nestTy);  return p;
    default:
        switch (nestTy) {
        case '(': return p;
        case '[': return rex1(BRACK_PREFIX, "`", p);
        case '{': return rex1(CURLY_PREFIX, "`", p);
        default:  die("bad nest");
        }
    }
}

static Rex *nest_rex_inner(Clem *es, int sz) {
    if (!sz) return rexN(CLEAR_PREFIX, "`", 0);

    if (es->ty == CLEM_RUNE) die("nest cannot begin with a rune");

    if (sz == 1) return es->rex;

    return infix_rex(CLEAR_INFIX, es, sz);
}

static Rex *nest_rex (char nestTy, Clem *es, int sz) {
    Rex *rex = nest_rex_inner(es, sz);

    return color(nestTy, rex);
}

static Rex *clump_rex (Clem *es, int sz) {
    if (sz < 1) die("impossible: empty clump");
    if (sz == 1) return es[0].rex;

    if (es[0].ty == CLEM_RUNE) {
        Rex *son = infix_rex(TIGHT_INFIX, es+1, sz-1);
        return rex1(TIGHT_PREFIX, es[0].txt, son);
    }

    return infix_rex(TIGHT_INFIX, es, sz);
}


/// Parsing Machine ////////////////////////////////////////////////////////////

enum cluster_ctx_type { CTX_NEST, CTX_CLUMP, CTX_LAYOUT };

typedef struct cluster_ctx {
    enum cluster_ctx_type ty;
    int pos;
    int sz;
    union { char nest; bool has_heir; };
} CCtx;

void add_rex_to_clump (int, Rex*);

static struct cluster_ctx ctx_stk[1024] = {
    (CCtx){ .ty=CTX_NEST, .pos=0, .sz=0, .nest='(' }
};

static struct cluster_elem elm_stk[1024] = {0};

static int elm_stk_sz=0, ctx_stk_sz=1;

#define TOP_CTX ctx_stk[ctx_stk_sz-1]
#define TOP_ELE elm_stk[elm_stk_sz-1]

// push an element to the element stack and grow the top-most context.
void raw_push_elem (Clem e) {
    elm_stk[elm_stk_sz++] = e;
    TOP_CTX.sz++;
}

static inline CCtx raw_pop_ctx  (void) {
    return ctx_stk[--ctx_stk_sz];
}

static inline void raw_push_ctx (CCtx c) {
    ctx_stk[ctx_stk_sz++] = c;
}

static void debug_stack (void) {
    Clem *elm = elm_stk;
    CCtx *ctx = ctx_stk;

    debugf("STK:<<< ");

    for (int i=0; i<ctx_stk_sz; i++,ctx++) {

        debugf("[%d]", ctx->pos);

        switch (ctx->ty) {
        case CTX_NEST:
            switch (ctx->nest) {
            case '(': debugf("paren("); break;
            case '[': debugf("brack("); break;
            case '{': debugf("curly("); break;
            }
            break;
        case CTX_CLUMP:
            debugf("clump(");
            break;
        case CTX_LAYOUT:
            debugf("layout(");
            break;
        default:
            debugf("\nctx:(sz=%d,off=%ld),elm:(sz=%d,off=%ld)\n", ctx_stk_sz, ctx - ctx_stk, elm_stk_sz, elm-elm_stk);
            die("wtf??");
        }

        for (int j=0; j<ctx->sz; j++, elm++) {
            switch(elm->ty) {
            case CLEM_RUNE: debugf("%s", elm->txt); break;
            case CLEM_REX:  print_rex(stderr, elm->rex); break;
            default:        die("wut?");
            }
            if (j+1 < ctx->sz) debugf(" ― ");
        }

        debugf(") ");
    }

    debugf(" >>>");
}

// Parsing Operations //////////////////////////////////////////////////////////

#define STACK if (DEBUG) { debugf("\t\t"); debug_stack(); debugf("\n"); }

static void finalize_clump (void) {
    // debugf("    finalize_clump\n");
    CCtx ctx = raw_pop_ctx();
    int sz   = ctx.sz;
    Clem *es = &elm_stk[elm_stk_sz-sz];
    Rex *rex = clump_rex(es, sz);

    TOP_CTX.sz++;
    elm_stk_sz -= sz;
    elm_stk[elm_stk_sz++] = (Clem){.ty=CLEM_REX, .rex=rex};
}

static void finalize_nest (void) {
    STACK;
    debugf("\tfinalize_nest()\n");
    CCtx ctx = TOP_CTX;
    int sz = ctx.sz;
    Clem *es = &elm_stk[elm_stk_sz - sz];
    Rex *rex = nest_rex(ctx.nest, es, sz);
    elm_stk_sz -= sz;
    ctx_stk_sz -= 1;
    add_rex_to_clump(ctx.pos, rex);
}

static void finalize_layout (void) {
    STACK;
    debugf("\tfinalize_layout()\n");
    CCtx ctx   = TOP_CTX;
    int sz     = ctx.sz;
    Clem *es   = &elm_stk[elm_stk_sz - sz];
    char *rune = es[0].txt;

    Rex *rex = NULL;

    if (ctx.has_heir) { // TODO: broken
        int nSons = sz-2;
        rex = rexN(CLEAR_PREFIX, rune, nSons);
        for (int i=0; i<nSons; i++) rex->rs[i] = es[i+1].rex;
        rex = rexH(rex, es[sz-1].rex);
    } else {
        int nSons = sz-1;
        rex = rexN(CLEAR_PREFIX, rune, nSons);
        for (int i=0; i<nSons; i++) rex->rs[i] = es[i+1].rex;
    }

    STACK;
    debugf("  deleting top context\n");
    ctx_stk_sz--;
    elm_stk_sz -= sz;
    STACK;
    if (DEBUG) { debugf("  about to push rex = "); print_rex(stderr, rex); debugf("\n"); }
    raw_push_elem((Clem){.ty=CLEM_REX, .rex=rex});

    if (TOP_CTX.ty == CTX_LAYOUT) {
        TOP_CTX.has_heir = (ctx.pos == TOP_CTX.pos);
    }
}

static void close_ctx (void) {
    switch (TOP_CTX.ty) {
    case CTX_NEST:   finalize_nest();   break;
    case CTX_CLUMP:  finalize_clump();  break;
    case CTX_LAYOUT: finalize_layout(); break;
    }
}

void close_ctx_if_clump (void) { if (TOP_CTX.ty==CTX_CLUMP) close_ctx(); }
void close_ctx_if_nest  (void) { if (TOP_CTX.ty==CTX_NEST)  close_ctx(); }

void layout (int col) {
    STACK;
    debugf("\tlayout(%d)\n", col);
    while (TOP_CTX.ty == CTX_LAYOUT && TOP_CTX.pos > col) finalize_layout();
}

void open_clump (int col) {
    STACK;
    debugf("\topen_clump(%d)\n", col);
    layout(col);
    if (TOP_CTX.ty == CTX_CLUMP) return;
    raw_push_ctx((CCtx){ .ty=CTX_CLUMP, .sz=0, .pos=col });
}

void open_nest (int col, char c) {
    STACK;
    debugf("\topen_nest(%d, '%c')\n", col, c);
    open_clump(col);
    raw_push_ctx((CCtx){.ty=CTX_NEST, .pos=col, .nest=c, .sz=0});
}

void add_rex_to_clump (int col, Rex *rex) {
    STACK;
    if (DEBUG) { debugf("\tadd_rex_to_clump(%d, ", col); print_rex(stderr, rex); debugf(")\n"); }
    open_clump(col);
    Clem elm = TOP_ELE;

    if (TOP_CTX.sz && elm.ty == CLEM_REX) {
        TOP_ELE.rex = rexH(elm.rex, rex);
    } else {
		raw_push_elem((Clem){ .ty=CLEM_REX, .rex=rex });
    }
}

void push_clumped_rune (int col, char *txt) {
    STACK;
    debugf("\tpush_clumped_rune(%d, %s)\n", col, txt);
    open_clump(col);
    raw_push_elem((Clem){ .ty=CLEM_RUNE, .txt=txt });
}

void push_free_rune (int col, char *txt) {
    STACK;
    debugf("\tpush_free_rune(%d, %s)\n", col, txt);
    close_ctx_if_clump();

    int rpos = (col - 1) + strlen(txt);

    layout(rpos);

    CCtx top = TOP_CTX;

    if (top.ty==CTX_LAYOUT)
        raw_push_ctx((CCtx){.ty=CTX_LAYOUT, .pos=rpos, .sz=0, .has_heir=0});

    else if (top.ty==CTX_NEST && !top.sz) // empty nest
        raw_push_ctx((CCtx){.ty=CTX_LAYOUT, .pos=rpos, .sz=0, .has_heir=0});

    else if (top.ty==CTX_NEST && TOP_ELE.ty == CLEM_RUNE) // rune + rune
        raw_push_ctx((CCtx){.ty=CTX_LAYOUT, .pos=rpos, .sz=0, .has_heir=0});

    raw_push_elem((Clem){.ty=CLEM_RUNE, .txt=txt});
}

static void finalize_context (void) {
    if (ctx_stk_sz != 1) die("ctx_stk_sz must be 1\n");

    Rex *rex = nest_rex('(', elm_stk, elm_stk_sz);

    if (elm_stk_sz) {
        print_rex(stdout, rex);
        printf("\n");
    }

    if (conf.color) {
        graybg(stdout);
        printf(" ");
        reset(stdout);
    }

    printf("\n");

    ctx_stk_sz = 1;
    ctx_stk[0].sz = elm_stk_sz = 0;
}

static void parse(Token tok) {
    static char *rune_txt = NULL;
    static int   rune_col = 0;

    // If this token immediately follows a rune, then we need to decide
    // if the token and the rune clump.  Runes clump with leaves and
    // BEGIN tokens.
    //
    // If the rune does clump, then we add the rune to the current clump
    // (or create a new one for the rune) and then process the current
    // token in the usual way.
    //
    // If the token does *not* clump, then what we do depends on the
    // type of context we are in.
    //
    // -   If we are in a clump, then we close the clump before
    //     proceeding.
    //
    // -   If we are in a nest, then we simply add the rune to the end
    //     of the nest context.
    //
    // -   Otherwise, we are in a LAYOUT context, in which case the rune
    //     is a part of a new LAYOUT context.

    if (rune_txt) {
        switch (tok.ty) {
        case BEGIN: case WORD: case TRAD: case QUIP: case UGLY: case SLUG:
            push_clumped_rune(rune_col, rune_txt);
            break;
        default:
            push_free_rune(rune_col, rune_txt);
        }
        rune_txt=NULL;
    }

    switch (tok.ty) {
    case END: {
        layout(0); // close all layout within the nest.
        close_ctx_if_clump();
        layout(0); // close all layout within the nest.
        close_ctx_if_nest();
        return;
      }

    case BEGIN:
        open_nest(tok.col, tok.buf[0]);
        return;

    case SEMI: case EOL: case WYTE:
        close_ctx_if_clump(); return;

    case EOF:
    case EOB:
        while (ctx_stk_sz>1) close_ctx();
        finalize_context();
        return;

    case RUNE: {
        rune_txt = calloc(tok.sz+1, 1);
        memcpy(rune_txt, tok.buf, tok.sz);
        rune_col = tok.col;
        return;
        // We wont know what to do with this until we see the next token,
        // so just stash it for now.
    }

    case BAD:
    case WORD:
    case TRAD:
    case QUIP:
    case UGLY:
    case SLUG:
        layout(tok.col);
        STACK;
        tok.buf[tok.sz] = 0;
        debugf("\tpush_leaf(\"%s\")\n", tok.sz ? tok.buf : "");
        Rex *rex = leaf_rex(tok);
        add_rex_to_clump(tok.col, rex);
        return;

    default:
        die("impossible: bad token");
    }
}


// Print Tokens with Syntax Highlighting ///////////////////////////////////////

static const bool hl_showspace = 0;

static void puttok(Token t) {
    FILE *f = stdout;

    switch (t.ty) {
    case BEGIN: case END: blue(f); break;
    case RUNE:            bold(f); gold(f); break;
    case BAD:             graybg(f); red(f); break;
    case SEMI:
        gray(f); break;
    case EOL:
        if (hl_showspace) { red(f); fprintf(f, "|EOL"); }
        break;
    case WYTE:
        if (hl_showspace) graybg(f);
        break;
    case WORD: case EOF: // no highlight
        break;
    case EOB:
        graybg(f);
        for (int i=0; i<80; i++) printf(" ");
        reset(f);
        printf("\n");
        return;
    case TRAD: case UGLY:
        green(f); break;
    case QUIP:
        bold(f); cyan(f); { if (t.sz==1) graybg(f); } break;
    case SLUG:
        magenta(f); break;
    default:
        fprintf(f, "bad token(%d)", t.ty);
        die("bad token");
    }

    fwrite(t.buf, 1, t.sz, f); reset(f);
    reset(f);
}


// Convert quips to strings. ///////////////////////////////////////////////////

static void quipemit (Token t) {
    if (conf.cmd == CMD_QUIP) puttok(t); else parse(t);
}

static void quipjoin (Token t) {
    static char buf[65536]={0};
    static char rbuf[64]={0};
    static int n=0, sz=0, c=0;
    static bool poison=0;
    static Token rune;

  again:
    if (!c && t.ty==QUIP) goto begin;
    if (!c)               goto pass_over;
    if (t.ty==EOF)        goto finalize;
    if (n)                goto consume;
    else                  goto check_terminal;

  pass_over:
    quipemit(t);
    return;

  check_terminal:
    switch (t.ty) {
    case END: case WYTE: case SEMI:
    case EOL: case QUIP: case UGLY: case SLUG:
        goto finalize;

    case RUNE: goto rune;
    default:   goto consume;
    }

  rune:
    memcpy(rbuf, t.buf, t.sz);
    rune = t;
    rune.buf = rbuf;
    rbuf[t.sz] = 0;
    return;

  finalize:
    if (rune.col && sz==1) {
        memcpy(buf+sz, rbuf, rune.sz);
        sz += rune.sz;
        rune.col = 0;
    }
    quipemit(TOK((poison?BAD:QUIP), buf, sz, c));
    if (rune.col) { quipemit(rune); rune.col=0; }
    n=c=0;
    goto again;

  begin:
    n = sz = poison = 0;
    c = t.col;
    goto consume;

  consume:
    if (t.ty != EOL && t.ty != WYTE && t.col < c) poison=1;

    if (t.ty == BEGIN) n++;
    if (t.ty == END)   n--;

    if (rune.col) {
        memcpy(buf+sz, rbuf, rune.sz);
        sz += rune.sz;
        rune.col = 0;
    }

    memcpy(buf+sz, t.buf, t.sz);
    sz += t.sz;
    return;
}


/// Splitting Blocks ///////////////////////////////////////////////////////////

enum bsplit_mode { OUTSIDE, LEADING_RUNE, SINGLE_LN, BLK };

Token eob = (Token){ .ty=EOB, .buf=NULL, .sz=0, .col=0 };

static void splitemit(Token t) {
    if (conf.cmd == CMD_SPLIT) puttok(t); else quipjoin(t);
}

static inline bool leafy (Token tok) {
    switch (tok.ty) {
    case BEGIN: case WORD: case QUIP: case TRAD: case UGLY: case SLUG: return 1;
    default:                                                           return 0;
    }
}

static void bsplit (Token tok) {
    static enum bsplit_mode mode = OUTSIDE;
    static int  eol              = 0;
    static int  nest             = 0;
    static int  rune_wid         = 0;
    static int  rune_col         = 0;
    static char rune_txt[128]    = {0};
    bool        end              = false;

    if (tok.ty == BEGIN) nest++;
    if (tok.ty == END)   nest--;

    eol = (tok.ty == EOL) ? eol+1 : 0;

    switch (mode) {
    case OUTSIDE:
        if (RUNE==tok.ty) {
            mode=LEADING_RUNE, rune_wid=tok.sz, rune_col=tok.col;
            memcpy(rune_txt, tok.buf, tok.sz);
            return; // we will emit later, once we see the next token.
        } else {
            mode = leafy(tok) ? SINGLE_LN : OUTSIDE;
            break;
        }

    case SINGLE_LN:
        if (nest==0 && eol==1) end=true;
        break;

    case LEADING_RUNE:
        splitemit(TOK(RUNE, rune_txt, rune_wid, rune_col));
        mode = leafy(tok) ? SINGLE_LN : BLK;
        break;

    case BLK:
        if (nest==0 && eol==2) end=true;
        break;
    }

    splitemit(tok);
    if (end) { splitemit(eob); mode=OUTSIDE; }
}


// Match BEGIN and END tokens //////////////////////////////////////////////////

static void nestjoin (Token t) {
    static char s[128] = {0};
    static int n = 0;

    if (t.ty == BEGIN) {
        switch (t.buf[0]) {
        case '(': s[n++] = ')'; break;
        case '[': s[n++] = ']'; break;
        case '{': s[n++] = '}'; break;
        default: die("bad BEGIN token");
        }
    }

    if (t.ty == END) {
        if (n && s[n-1]==t.buf[0]) n--;
        else t.ty=BAD;
    }

    if (conf.cmd == CMD_NEST) puttok(t); else bsplit(t);
}


// Lexer ///////////////////////////////////////////////////////////////////////

typedef enum lex_mode {
    BASE_MODE,  // Dispatch based on first character.
    WYTE_MODE,  // | +|
    RUNE_MODE,  // |{:runechar:}+|
    WORD_MODE,  // |{:wordchar:}+|
    NOTE_MODE,  // |;.*$|
    TRAD_MODE,  // |("[^"]*")+|
    TICK_MODE,  // |'|
    UGLY_START, // |''+|
    UGLY_MODE,  // |''+\n.*\n *''+|
    SLUG_TICK,  // |'( .*)?$|
    SLUG_TEXT,  // |'( .*)?$|
    SLUG_LOOK,  // |'( .*)?$|
} LexMode;

typedef struct lex_st LexSt;

struct lex_st {
    LexMode mode;       // Which state the lexer is in.
    bool    again;      // Please re-input the previous character.
    int     col, tcol;  // column of current character and current token.
    char    nest[256];  // stack of nesting terminator chars.
    int     nestsz;     // nesting depth.
    char    buf[65536]; // character buffer for current token.
    int     bufsz;      // character-width of current token.
    int     usz, urem;  // ugly quote-width, ugly terminator seen-count.
    bool    tterm;      // waiting to see if trad-string ends.
    Token   qrune;      // Terminal rune candidate.
    bool    qterm;      // Quip is finished.
};

static bool isword(char c) { return (isalnum(c) || c == '_'); }

static void lexemit(Token t) {
    if (conf.cmd == CMD_LEX) puttok(t); else nestjoin(t);
}

#define emit0(t) { lexemit(TOK(t, buf, bufsz,   tcol)); \
                   bufsz=0;                             \
                   mode=BASE_MODE;                      \
                   return; }                            \

#define emit1(t) { lexemit(TOK(t, buf, bufsz-1, tcol)); \
                   buf[0] = c;                          \
                   bufsz=1;                             \
                   mode=BASE_MODE;                      \
                   goto basemode; }

static char newline[2] = { '\n', 0 };
static Token eol_tok = {.ty=EOL, .buf=newline, .sz=1, .col=0};

void lex (int c) {
    static LexMode mode=BASE_MODE;
    static int     col=0, tcol=0; // input column, token column
    static char    buf[65536];    // token text
    static int     bufsz=0;       // Width of token text
    static int     usz=0, urem=0; // Ugly-string deliminater width+remaining
    static int     ss=0;          // Slug indenting-space count.
    static bool    tterm=0;       // Trad-string lookahead.
    static bool    poison=0;      // Dedent poision for strings.

    col = (c=='\n') ? 0 : col + 1;
    buf[bufsz++]=c;

    if (c == 256) {
        if (mode != BASE_MODE) { mode=BASE_MODE; emit0(BAD); }
        else { emit0(EOF); }
    }

    switch (mode) {
    case BASE_MODE: basemode:
        poison=tterm=0, tcol=col;

        switch (c) {
        case '(': case '[': case '{': emit0(BEGIN);
        case ']': case ')': case '}': emit0(END);
        case '\'':                    mode=TICK_MODE; return;
        case '"':                     mode=TRAD_MODE; return;
        case ';':                     mode=NOTE_MODE; return;
        case '\n':                    emit0(EOL);
        default:
            if (c == ' ')  { mode=WYTE_MODE; goto wytemode; }
            if (isrune(c)) { mode=RUNE_MODE; goto runemode; }
            if (isword(c)) { mode=WORD_MODE; goto wordmode; }
            emit0(BAD);
        }

    case NOTE_MODE:           if (c != '\n') return; else emit1(SEMI);
    case WYTE_MODE: wytemode: if (c == ' ')  return; else emit1(WYTE);
    case WORD_MODE: wordmode: if (isword(c)) return; else emit1(WORD);
    case RUNE_MODE: runemode: if (isrune(c)) return; else emit1(RUNE);

    case SLUG_TEXT: slugtext:
        if (c == '\n') mode=SLUG_LOOK, ss=0;
        return;

    case SLUG_LOOK:
        switch (c) {
        case ' ':  ss++; return;
        case '\'': if (col == tcol) { mode=SLUG_TICK; return; }
        default:   break;
        }

        lexemit(TOK(SLUG, buf, bufsz-(ss+2), tcol));
        lexemit(eol_tok);
        if (ss) { memset(buf, ' ', ss); lexemit(TOK(WYTE, buf, ss, 1)); }
        { buf[0]=c, bufsz=1, mode=BASE_MODE; goto basemode; }

    case SLUG_TICK:
        if (c == ' ' || c == '\n') { mode=SLUG_TEXT; goto slugtext; }
        { mode=TICK_MODE; goto tickmode; }

    case UGLY_START:
        if (c == '\'') { usz++; return; }
        if (c != '\n') { poison=1; }
        mode=UGLY_MODE;
        return;

    case UGLY_MODE:
        if (col<tcol && c!=' ' && c!='\n') { poison=1;         }
        if (c != '\'')                     { urem=usz; return; }
        if (--urem)                        { return;           }
        if (col+1 != tcol+usz)             { poison=1;         }
        emit0(poison?BAD:UGLY);

    case TRAD_MODE: {
        if (col<=tcol && c!=' ' && c!='\n') { poison=1;        }
        if (c == '"' && tterm)              { tterm=0; return; }
        if (c == '"')                       { tterm=1; return; }
        if (!tterm) return;
        emit1(poison?BAD:TRAD);
      }

    case TICK_MODE: tickmode:
        switch (c) {
        case ' ':  case '\n': mode=SLUG_TEXT; goto slugtext;
        case '\'':            usz=2; mode=UGLY_START; return;
        default:              emit1(QUIP);
        }
    }
}


// CLI Tool ////////////////////////////////////////////////////////////////////

static inline _Noreturn void usage (void) {
    printf("rex [--no-color]\n");
    printf("rex (lex | nest | slug | split | quip | parse) [--no-color]\n");
    printf("rex cmd rune rune\n");
    exit(2);
}

static inline Config argparse (int argc, char **argv) {
    if (argc==1) return (Config){CMD_PARSE, true, 0, 0};

    if (!strcmp(argv[1], "--no-color")) {
        if (argc != 2) usage();
        return (Config){CMD_PARSE, false, 0, 0};
    }

    if (!strcmp(argv[1], "cmp")) {
        if (argc != 4) usage();
        return (Config){CMD_CMP, false, argv[2], argv[3]};
    }

    Config c = {CMD_PARSE, true, 0, 0};

    if (!strcmp(argv[1], "lex"))        c.cmd=CMD_LEX;
    else if (!strcmp(argv[1], "nest"))  c.cmd=CMD_NEST;
    else if (!strcmp(argv[1], "split")) c.cmd=CMD_SPLIT;
    else if (!strcmp(argv[1], "quip"))  c.cmd=CMD_QUIP;
    else if (!strcmp(argv[1], "parse")) c.cmd=CMD_PARSE;
    else usage();

    if (argc == 3) {
        if (strcmp(argv[2], "--no-color")) usage();
        c.color=0;
    }

    return c;
}

int main (int argc, char **argv) {
    conf = argparse(argc, argv);

    if (conf.cmd == CMD_CMP) {
        char *arune = conf.r1;
        char *brune = conf.r2;
        int ord = runecmp(arune, brune);
        char o = '=';
        if (ord<0) o='<';
        if (ord>0) o='>';
        printf("(%s %c %s)\n", arune, o, brune);
        return 0;
    }

    while (1) {
        char c = getchar();
        if (!feof(stdin)) { lex(c); continue; }
        lex('\n');
        lex(256);
        return 0;
    }
}
