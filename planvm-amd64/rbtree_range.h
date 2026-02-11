#ifndef RBTREE_RANGE_H
#define RBTREE_RANGE_H

typedef struct rb_node rb_node;

typedef enum { RB_RED = 0, RB_BLACK = 1 } rb_color;

struct rb_node {
    uintptr_t start;
    uintptr_t end;   // invariant: end > start

    rb_node *left;
    rb_node *right;
    rb_node *parent;

    rb_node *next;   // inorder successor thread (NULL for max element)

    rb_color color;
};

typedef struct {
    rb_node *root;
    rb_node *first;   // leftmost (min) node to start forward iteration
    size_t   size;
} rb_tree;

void rb_init(rb_tree *t);
void rb_node_init(rb_node *n, uintptr_t start, uintptr_t end);

bool rb_insert(rb_tree *t, rb_node *z);
rb_node* rb_lower_bound_start(const rb_tree *t, uintptr_t key_start);
rb_node* rb_find_containing(const rb_tree *t, uintptr_t addr);
void rb_delete(rb_tree *t, rb_node *z);
bool rb_insert_or_coalesce(rb_tree *t, rb_node *z);

void rb_print_inorder(const rb_tree *t, const char *label);

// iteration api
static inline rb_node* rb_iter_begin(const rb_tree *t) { return t->first; }
static inline rb_node* rb_iter_next(const rb_node *n) { return n ? n->next : NULL; }

#endif
