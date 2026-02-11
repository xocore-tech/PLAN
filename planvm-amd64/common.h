#ifndef COMMON_H
#define COMMON_H

#include <stddef.h>

typedef struct bump_slab_t {
  struct bump_slab_t* next;
  size_t used;   // bytes used in payload
  size_t cap;    // bytes capacity in payload
} bump_slab_t;

typedef struct bump_alloc_t {
  bump_slab_t* head;
  size_t stride;   // per-object, already A-aligned
  size_t align;    // required alignment (power of two)
} bump_alloc_t;

void bump_alloc_init(bump_alloc_t* a, int size);
void* bump_alloc_alloc(bump_alloc_t* a);
void bump_alloc_free(bump_alloc_t* a);

#endif
