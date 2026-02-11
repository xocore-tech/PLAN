// rbt_range.c — singly-threaded red‑black tree for [start,end) ranges
// public domain / cc0. no warranties. iterative only (no recursion).
// focuses on INSERT + forward iteration; deletion can be added later.

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
//#include <assert.h>
#include "printf.h"

#include "rbtree_range.h"

// compare note: we order by (start,end). ranges are [start,end), end>start.
// addresses stored as uintptr_t to avoid UB from void* arithmetic/compare.

void rb_init(rb_tree *t) {
    t->root = NULL; t->first = NULL; t->size = 0;
}

static inline int rb_cmp(uintptr_t a_start, uintptr_t a_end,
                         uintptr_t b_start, uintptr_t b_end) {
    if (a_start < b_start) return -1;
    if (a_start > b_start) return +1;
    if (a_end   < b_end)   return -1;
    if (a_end   > b_end)   return +1;
    return 0;
}

/* static rb_node* rb_leftmost(rb_node *x) { */
/*     if (!x) return NULL; */
/*     while (x->left) x = x->left; */
/*     return x; */
/* } */

static void rotate_left(rb_tree *t, rb_node *x) {
    rb_node *y = x->right;
    // assert(y);
    x->right = y->left;
    if (y->left) y->left->parent = x;
    y->parent = x->parent;
    if (!x->parent) t->root = y;
    else if (x == x->parent->left) x->parent->left = y;
    else x->parent->right = y;
    y->left = x;
    x->parent = y;
}

static void rotate_right(rb_tree *t, rb_node *x) {
    rb_node *y = x->left;
    // assert(y);
    x->left = y->right;
    if (y->right) y->right->parent = x;
    y->parent = x->parent;
    if (!x->parent) t->root = y;
    else if (x == x->parent->right) x->parent->right = y;
    else x->parent->left = y;
    y->right = x;
    x->parent = y;
}

// fixup after insert (CLRS). iterative.
static void insert_fixup(rb_tree *t, rb_node *z) {
    while (z->parent && z->parent->color == RB_RED) {
        rb_node *p = z->parent;
        rb_node *g = p->parent;
        if (!g) break; // parent is root; safe guard
        if (p == g->left) {
            rb_node *u = g->right; // uncle
            if (u && u->color == RB_RED) {
                p->color = RB_BLACK;
                u->color = RB_BLACK;
                g->color = RB_RED;
                z = g;
            } else {
                if (z == p->right) {
                    z = p;
                    rotate_left(t, z);
                    p = z->parent; g = p ? p->parent : NULL;
                }
                p->color = RB_BLACK;
                if (g) {
                    g->color = RB_RED;
                    rotate_right(t, g);
                }
            }
        } else {
            rb_node *u = g->left; // uncle
            if (u && u->color == RB_RED) {
                p->color = RB_BLACK;
                u->color = RB_BLACK;
                g->color = RB_RED;
                z = g;
            } else {
                if (z == p->left) {
                    z = p;
                    rotate_right(t, z);
                    p = z->parent; g = p ? p->parent : NULL;
                }
                p->color = RB_BLACK;
                if (g) {
                    g->color = RB_RED;
                    rotate_left(t, g);
                }
            }
        }
    }
    if (t->root) t->root->color = RB_BLACK;
}

// public: create a node (caller can also embed rb_node in a larger struct)
void rb_node_init(rb_node *n, uintptr_t start, uintptr_t end) {
    n->start = start; n->end = end;
    n->left = n->right = n->parent = n->next = NULL;
    n->color = RB_RED;
}

// find place to insert; also discover inorder predecessor/successor for threading
// returns false if an identical key already exists (no insert performed).
bool rb_insert(rb_tree *t, rb_node *z) {
    rb_node *y = NULL;      // parent runner
    rb_node *x = t->root;   // search cursor
    rb_node *pred = NULL;   // inorder predecessor
    rb_node *succ = NULL;   // inorder successor

    while (x) {
        y = x;
        int c = rb_cmp(z->start, z->end, x->start, x->end);
        if (c < 0) {
            succ = x;            // going left → current x is a successor candidate
            x = x->left;
        } else if (c > 0) {
            pred = x;            // going right → current x is a predecessor candidate
            x = x->right;
        } else {
            return false; // disallow exact duplicate key
        }
    }

    // link z under y
    z->parent = y;
    if (!y) t->root = z;
    else if (rb_cmp(z->start, z->end, y->start, y->end) < 0) y->left = z;
    else y->right = z;

    // set thread pointers
    z->next = succ;
    if (pred) pred->next = z; else t->first = z;

    // standard rb fixup
    insert_fixup(t, z);

    // update tree->first if leftmost rotated away (defensive):
    if (t->first) {
        // ensure first is genuinely leftmost by walking parents if needed
        rb_node *f = t->first;
        while (f->left) f = f->left;
        t->first = f;
    }

    t->size++;
    return true;
}

// search helpers (log n): lower_bound for start address
rb_node* rb_lower_bound_start(const rb_tree *t, uintptr_t key_start) {
    rb_node *x = t->root, *best = NULL;
    while (x) {
        if (key_start <= x->start) { best = x; x = x->left; }
        else x = x->right;
    }
    return best;
}

// find the node whose range contains addr, or NULL if none. (no coalescing assumed)
rb_node* rb_find_containing(const rb_tree *t, uintptr_t addr) {
    rb_node *x = t->root;
    while (x) {
        if (addr < x->start) x = x->left;
        else if (addr >= x->end) x = x->right;
        else return x; // start <= addr < end
    }
    return NULL;
}

// sanity check: validate red-black invariants and threading (O(n)). heavy; use in debug.
#ifdef RB_DEBUG
#include <stdio.h>
static int check_black_height(rb_node *x) {
    if (!x) return 1;
    int lh = check_black_height(x->left);
    int rh = check_black_height(x->right);
    assert(lh == rh);
    assert(x->color == RB_BLACK || x->color == RB_RED);
    if (x->color == RB_RED) {
        assert(!x->left  || x->left->color  == RB_BLACK);
        assert(!x->right || x->right->color == RB_BLACK);
    }
    return lh + (x->color == RB_BLACK);
}

static void check_threading(const rb_tree *t) {
    const rb_node *n = rb_iter_begin(t);
    const rb_node *prev = NULL;
    uintptr_t last = 0; (void)last;
    while (n) {
        if (prev) {
            assert(prev->next == n);
            assert(rb_cmp(prev->start, prev->end, n->start, n->end) < 0);
        }
        prev = n; n = n->next;
    }
}

static void rb_validate(const rb_tree *t) {
    if (!t->root) return;
    assert(t->root->color == RB_BLACK);
    (void)check_black_height(t->root);
    check_threading(t);
}
#endif

// ==========================
// deletion (iterative) + threading maintenance
// ==========================

static rb_node* rb_tree_min(rb_node *x) {
  while (x && x->left) {
    x = x->left;
  }
  return x;
}

static rb_node* rb_tree_max(rb_node *x) {
  while (x && x->right) {
    x = x->right;
  }
  return x;
}

static rb_node* rb_predecessor(rb_node *x) {
    if (!x) return NULL;
    if (x->left) return rb_tree_max(x->left);
    rb_node *p = x->parent;
    while (p && x == p->left) { x = p; p = p->parent; }
    return p;
}

static void rb_transplant(rb_tree *t, rb_node *u, rb_node *v) {
    if (!u->parent) t->root = v;
    else if (u == u->parent->left) u->parent->left = v;
    else u->parent->right = v;
    if (v) v->parent = u->parent;
}

static void delete_fixup(rb_tree *t, rb_node *x, rb_node *x_parent) {
    while ((x != t->root) && (!x || x->color == RB_BLACK)) {
        if (x == (x_parent ? x_parent->left : NULL)) {
            rb_node *w = x_parent ? x_parent->right : NULL; // sibling
            if (w && w->color == RB_RED) {
                w->color = RB_BLACK;
                x_parent->color = RB_RED;
                rotate_left(t, x_parent);
                w = x_parent ? x_parent->right : NULL;
            }
            if ((!w) || ((!w->left || w->left->color == RB_BLACK) && (!w->right || w->right->color == RB_BLACK))) {
                if (w) w->color = RB_RED;
                x = x_parent;
                x_parent = x_parent ? x_parent->parent : NULL;
            } else {
                if (!w->right || w->right->color == RB_BLACK) {
                    if (w->left) w->left->color = RB_BLACK;
                    if (w) { w->color = RB_RED; rotate_right(t, w); }
                    w = x_parent ? x_parent->right : NULL;
                }
                if (w) w->color = x_parent ? x_parent->color : RB_BLACK;
                if (x_parent) x_parent->color = RB_BLACK;
                if (w && w->right) w->right->color = RB_BLACK;
                if (x_parent) rotate_left(t, x_parent);
                x = t->root; x_parent = NULL;
            }
        } else {
            rb_node *w = x_parent ? x_parent->left : NULL;
            if (w && w->color == RB_RED) {
                w->color = RB_BLACK;
                x_parent->color = RB_RED;
                rotate_right(t, x_parent);
                w = x_parent ? x_parent->left : NULL;
            }
            if ((!w) || ((!w->right || w->right->color == RB_BLACK) && (!w->left || w->left->color == RB_BLACK))) {
                if (w) w->color = RB_RED;
                x = x_parent;
                x_parent = x_parent ? x_parent->parent : NULL;
            } else {
                if (!w->left || w->left->color == RB_BLACK) {
                    if (w->right) w->right->color = RB_BLACK;
                    if (w) { w->color = RB_RED; rotate_left(t, w); }
                    w = x_parent ? x_parent->left : NULL;
                }
                if (w) w->color = x_parent ? x_parent->color : RB_BLACK;
                if (x_parent) x_parent->color = RB_BLACK;
                if (w && w->left) w->left->color = RB_BLACK;
                if (x_parent) rotate_right(t, x_parent);
                x = t->root; x_parent = NULL;
            }
        }
    }
    if (x) x->color = RB_BLACK;
}

// delete a node already in the tree; maintains threading (n->next) and first.
void rb_delete(rb_tree *t, rb_node *z) {
    if (!z) return;

    // save neighbors for threading repair
    rb_node *pred_z = rb_predecessor(z);
    rb_node *succ_z = z->next;

    rb_node *y = z;                     // node actually removed from the tree
    rb_color y_orig_color = y->color;
    rb_node *x = NULL;                  // child that moves into y's place
    rb_node *x_parent = NULL;           // parent of x during fixup

    if (!z->left) {
        x = z->right; x_parent = z->parent;
        rb_transplant(t, z, z->right);
    } else if (!z->right) {
        x = z->left; x_parent = z->parent;
        rb_transplant(t, z, z->left);
    } else {
        y = rb_tree_min(z->right);      // successor of z
        y_orig_color = y->color;
        x = y->right;                    // y has no left child
        x_parent = y->parent;
        // threading neighbor on successor side BEFORE structure changes
        rb_node *succ_y = y->next;      

        if (y->parent == z) {
            x_parent = y;               // x may be NULL; its parent is y after transplant
        } else {
            rb_transplant(t, y, y->right);
            y->right = z->right; y->right->parent = y;
        }
        rb_transplant(t, z, y);
        y->left = z->left; y->left->parent = y;
        y->color = z->color;

        // repair threads around z/y region: pred(z) -> y -> succ(y)
        if (pred_z) pred_z->next = y; else t->first = y;
        y->next = succ_y;

        // now proceed to fix colors starting from x and x_parent.
    }

    if (y == z) {
        // simple case (<=1 child): splice pred -> succ
        if (pred_z) pred_z->next = succ_z; else t->first = succ_z;
    }

    if (y_orig_color == RB_BLACK) {
        delete_fixup(t, x, x_parent);
    }

    t->size--;
}

// ==========================
// coalescing insert (merges adjacent/overlapping ranges)
// ==========================

static inline bool ranges_touch_or_overlap(uintptr_t a0, uintptr_t a1, uintptr_t b0, uintptr_t b1) {
    // intervals are [start,end). overlap if a0 < b1 && b0 < a1; adjacency if a1 == b0 || b1 == a0
    bool overlap = (a0 < b1) && (b0 < a1);
    bool adjacent = (a1 == b0) || (b1 == a0);
    return overlap || adjacent;
}

static rb_node* rb_max_leq_start(const rb_tree *t, uintptr_t key_start) {
    rb_node *x = t->root, *best = NULL;
    while (x) {
        if (x->start <= key_start) { best = x; x = x->right; }
        else x = x->left;
    }
    return best;
}

// insert with coalescing. deletes neighbors that merge, expands z, then inserts.
bool rb_insert_or_coalesce(rb_tree *t, rb_node *z) {
    uintptr_t ns = z->start, ne = z->end;
    // assert(ne > ns);

    // merge forward with successors whose starts are <= current ne and touch/overlap
    for (rb_node *n = rb_lower_bound_start(t, ns); n && ranges_touch_or_overlap(ns, ne, n->start, n->end); n = rb_lower_bound_start(t, ns)) {
        if (n->start < ns) ns = n->start;
        if (n->end   > ne) ne = n->end;
        rb_delete(t, n);
    }

    // merge backward with predecessors whose end >= current ns and touch/overlap
    for (rb_node *p = rb_max_leq_start(t, ns);
         p && ranges_touch_or_overlap(ns, ne, p->start, p->end);
         p = rb_max_leq_start(t, ns)) {
        if (p->start < ns) ns = p->start;
        if (p->end   > ne) ne = p->end;
        rb_delete(t, p);
    }

    rb_node_init(z, ns, ne);
    return rb_insert(t, z);
}

void rb_print_inorder(const rb_tree *t, const char *label) {
    printf("%s (size=%zu)\n", label, t->size);
    for (rb_node *it = rb_iter_begin(t); it; it = rb_iter_next(it)) {
        printf("    [%zx,%zx)\n", (size_t)it->start, (size_t)it->end);
    }
}

// ==========================
// demo main()
// ==========================
#ifdef RB_DEMO
#include <stdio.h>

int main(void) {
    rb_tree t; rb_init(&t);

    // raw inserts (no coalescing) to show baseline ordering
    rb_node a,b,c,d,e,f,g,h,i;
    rb_node_init(&a, 30,40);
    rb_node_init(&b, 10,11);
    rb_node_init(&c, 50,60);
    rb_node_init(&d, 20,25);
    rb_node_init(&e,  0, 5);
    rb_node_init(&f, 70,80);
    rb_node_init(&g, 15,17);
    rb_node_init(&h, 26,29);
    rb_node_init(&i, 90,95);

    rb_insert(&t, &a);
    rb_insert(&t, &b);
    rb_insert(&t, &c);
    rb_insert(&t, &d);
    rb_insert(&t, &e);
    rb_insert(&t, &f);
    rb_insert(&t, &g);
    rb_insert(&t, &h);
    rb_insert(&t, &i);

#ifdef RB_DEBUG
    rb_validate(&t);
#endif

    dump(&t, "after plain inserts");

    // now demonstrate coalescing via rb_insert_or_coalesce
    // cases covered:
    // 1) overlap left neighbor: [8,13) merges with [10,11] and [15,17] only partially (touch later)
    // 2) adjacency: [17,20) touches previous -> merge
    // 3) bridge pred+succ: inserting [25,50) connects [20,25]..[26,29]..[30,40]..[50,60)
    // 4) disjoint stays separate: [82,85)

    rb_node m1,m2,m3,m4;

    rb_node_init(&m1, 8,13);  // overlaps [10,11], extends left side
    rb_insert_or_coalesce(&t, &m1);
    dump(&t, "after insert_or_coalesce([8,13))");

    rb_node_init(&m2, 17,20); // adjacent to [15,17) -> merges into [8,20)
    rb_insert_or_coalesce(&t, &m2);
    dump(&t, "after insert_or_coalesce([17,20))");

    rb_node_init(&m3, 25,50); // bridges [20,25) .. [26,29) .. [30,40) .. [50,60)
    rb_insert_or_coalesce(&t, &m3);
    dump(&t, "after insert_or_coalesce([25,50))  // expect one big [8,60) block on the low side");

    rb_node_init(&m4, 82,85); // disjoint, should remain separate between [70,80) and [90,95)
    rb_insert_or_coalesce(&t, &m4);
    dump(&t, "after insert_or_coalesce([82,85))");

#ifdef RB_DEBUG
    rb_validate(&t);
#endif

    return 0;
}
#endif
