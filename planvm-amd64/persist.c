// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
// -*- tab-width: 8 -*-

#include <nmmintrin.h>
#include <errno.h>
#include "printf.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdnoreturn.h>

#include "common.h"
#include "rbtree_range.h"

/* -------------------------------------------------------------------- */

#define BADBEEF 0xBADBEEFBADBEEF
#define ENSURE_NOBEEF(x) \
	if (x == BADBEEF) { \
	    die("Found pointer to freed memory"); \
	}

// TODO: Duplicated from rpn.c. I'd try to normalize with `common.h`, but that
// appears to be a dead file unreferenced anywhere.

typedef uint64_t u64;
typedef uint64_t Val;
typedef uint32_t u32;
typedef uint8_t  u8;

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

inline static u64 round_down_align(u64 base, u64 alignment) {
  return base & ~(alignment - 1);
}

inline static u64 round_up_align(u64 base, u64 alignment) {
  return ((base + alignment - 1) / alignment) * alignment;
}

int syscall_close(int fd);

int syscall_fstat(int fd, struct stat *st);
int syscall_fsync(int fd);
int syscall_msync(u64* ptr, size_t len, int flags);
int syscall_madvise(u64* ptr, size_t len, int advice);
int syscall_ftruncate(int fd, off_t len);
int syscall_fallocate(int fd, int mode, off_t offset, off_t len);
ssize_t syscall_pread64(int fd, char* buf, size_t count, off_t pos);
ssize_t syscall_pwrite64(int fd, char* buf, size_t count, off_t pos);

u64* syscall_mmap(u64*, u64, u64, u64, int64_t, off_t);

void noreturn die   (const char*);

/* -------------------------------------------------------------------- */

extern u64* buddy_malloc(size_t request, size_t tag);
extern void buddy_free(u64* ptr);

/* -------------------------------------------------------------------- */

typedef struct { uint64_t key, val; } entry_t;
typedef struct {
	uint64_t lg;
	uint64_t mask;
	uint64_t count;
	entry_t tbl[];    // flexible array
} ht_t;

extern ht_t* ht_create(uint64_t lg);
extern u64 ht_has(ht_t* tbl, Val item);

// c wrapper around ht_put, now that ht_put's calling convention is kind of
// non-standard.
extern void c_ht_put(ht_t** tbl, Val item, u64 data);
extern Val c_ht_get(ht_t** tbl, Val item);

extern bool ismegapin(Val i);

extern uint32_t calculate_crc32c(const char* data, uint64_t size);

u64 natural_crc32(u64 nat);
u64 crc32_checksum_for(Val item);

struct superblock;
bool read_superblock(int fd, off_t offset, struct superblock *sb);
void write_superblock(int fd, off_t offset, struct superblock *sb);
u64* allocate_persistence_space_for(size_t sz);
void find_active_superblock(struct superblock sb1,
                            bool have_sb1,
                            off_t sb1_offset,
                            struct superblock sb2,
                            bool have_sb2,
                            off_t sb2_offset);


bool mark_item_in_tree(Val item, rb_tree* tree, bump_alloc_t* a);
void scrub_item(Val item, u64 expected, rb_tree* tree, bump_alloc_t* a);

size_t filesize = 0;

// The size (in bytes) of everything from the front of the file up to the start
// of the data area.
size_t headersize = 0;

u64* persistence_start = 0;
u64* persistence_cur = 0;
u64* persistence_end = 0;


#define MAP_SHARED 0x01
#define MAP_FIXED  0x10

int persistence_fd = -1;

extern off_t elf_binary_end();

u64 base_addr = 0x500000;

#include "planvm-amd64.incc.c"

void update_superblock(Val item) {
  active_sb.sequence = active_sb.sequence + 1;
  if ((item >> 63) == 0) {
    active_sb.val_checksum = natural_crc32(item);
  } else {
    active_sb.val_checksum = PTR(item)[-2];
  }
  active_sb.val = item;
  active_sb.write_offset = persistence_cur - persistence_start;

  write_superblock(persistence_fd, inactive_offset, &active_sb);
}

// OK, but we are going to be doing fancy things: we're going to be trying to
// ensure we maximize block reuse. And we're going to do that by separating
// items onto different pages. We will have four regions:
//
// - a series of small blocks with megapins where all the megapins do not cross
//   page boundaries
//
// - big megapins (defined as >= 2/3 of a block large) get their own block(s).
//
// - a series of small blocks with pins where pins don't cross pages.
//
// - big pins
//
// This separation is an attempt to 

u64 large_item_words = 410;  /* `page_size_words * 0.8` rounded up. */

// Previously, we were able to just switch based on whether things were a
// megapin or not. Now location_table has to be a bit more complex:
// location_table maps to a 62-bit integer wordsz length + 2 least significant
// bit type tag, saving whether the location is a small_items_offset, a big
// megapins offset or a big pins offset.

const u64 SMOL_ITEM = 0;
const u64 BIG_ITEM = 1;

// We pack two u32s crc32s into a single u64.
inline static u64 wordsForCRC32s(u64 num_crcs) {
  return (num_crcs + 1) / 2;
}

// Non-recursive assignment of an item
void assign_item_persistence(Val item,
                             size_t* smol_items_offset,
                             size_t* big_items_offset,
                             ht_t** small_page_table,
                             ht_t** location_table) {
  /* printf("assign_item_persistence(%lx)\n", item); */

  u64 bitsz = get_bitsz(item);
  u64 wordsz = (bitsz + 63) / 64;
  u64* p = PTR(item);
  u64 pin_count = p[2];
  ENSURE_NOBEEF(pin_count);

  // Add space for the additional crc32 information. The '2' is 1 for the
  // front header crc32, 1 for the normal heap header.
  u64 crc_count = pin_count;
  if (ismegapin(item)) {
    u64 base = 6 + pin_count * 2;
    u64 megacount = p[base];
    crc_count += megacount;
  }
  wordsz += 2 + wordsForCRC32s(crc_count);

  /* printf(" - wordsz: %ld (large item size %ld)\n", wordsz, large_item_words); */

  // If this item is a chonky boi, we give it a number of contiguous pages in
  // the big items region starting from the first item, making sure it's
  // aligned with a page, which signals it owns the pages.
  if (wordsz >= large_item_words) {
    u64 item_offset = round_up_align(wordsz, page_size_words);

    // Record the location of the megapin.
    c_ht_put(location_table, item, (*big_items_offset << 2) | BIG_ITEM);

    // Offset by the number of words here.
    *big_items_offset += item_offset;
  } else {
    // This item is smol and can fit on a single page, but check if it will
    // fit on the current page.
    u64 cur_page_off = round_down_align(*smol_items_offset, page_size_words);
    u64 end_page_off =
        round_down_align(*smol_items_offset + wordsz, page_size_words);
    if (end_page_off != cur_page_off) {
      // This smol item doesn't fit on the current page. So what we do is we
      // first advance to the next page because smol items can't straddle
      // page boundaries.
      *smol_items_offset = end_page_off;
      cur_page_off = end_page_off;
    }

    // Check if we already have a small item header on the current page.
    u64 small_page_size;
    u64 idx = ht_has(*small_page_table, cur_page_off + 1);
    if (idx != 0xffffffffffffffff) {
      small_page_size = c_ht_get(small_page_table, idx);
    } else {
      // Initialize to size to one to count for the header.
      small_page_size = 1;
      (*smol_items_offset)++; // For the header word.
    }

    // The current total size of the small page is the size of the first
    // item we just placed.
    u64 total = small_page_size + wordsz;
    c_ht_put(small_page_table, cur_page_off + 1, total);
    /* printf(" - page size for %ld is now %ld\n", cur_page_off, total); */

    c_ht_put(location_table, item, (*smol_items_offset << 2) | SMOL_ITEM);
    *smol_items_offset += wordsz;
  }
}

// Go through all megapins and assign them to.
void assign_megapin_persistence(Val item,
                                size_t* smol_items_offset,
                                size_t* big_items_offset,
                                ht_t** small_page_table,
                                ht_t** location_table,
                                ht_t** record_table) {
  if ((item >> 63) == 0) return; // direct

  // Check to see if the item is outside the persistence heap area. If it's on
  // the persistence heap, we don't need to do anything.
  u64* p = PTR(item);
  if (p >= persistence_start && p <= persistence_end) {
    return;
  }

  // Check to see if the item has a redirection header. If it does, it and all
  // its child items are actually on disk.
  u64 redirection = p[-2];
  if (redirection != 0) {
    if (redirection == (u64)-1) {
      die("TODO: Handle wait for remote sync");
    }

    // Check if we've already recorded the redirection.
    u64 idx = ht_has(*record_table, (u64)p);
    if (idx != 0xffffffffffffffff) {
      return;
    }

    c_ht_put(record_table, (u64)p, redirection);
    return;
  }

  /* printf("assign_megapin_persistence(%lx)\n", item); */

  u64 pin_count = p[2];
  ENSURE_NOBEEF(pin_count);
  if (ismegapin(item)) {
    // Check if `item` is an already traversed megapin that we've recorded.
    u64 idx = ht_has(*location_table, item);
    if (idx != 0xffffffffffffffff) {
      return;
    }

    assign_item_persistence(item,
                            smol_items_offset, big_items_offset,
                            small_page_table, location_table);

    // Ignore pins in the megapin and just recur through child megapins.
    u64 base = 6 + pin_count * 2;
    u64 megacount = p[base];
    base++;
    for (size_t i = 0; i < megacount; ++i) {
      assign_megapin_persistence(p[base + i],
                                 smol_items_offset, big_items_offset,
                                 small_page_table, location_table,
                                 record_table);
    }
  } else {
    // Don't record anything about this pin, but do recursively traverse since
    // there might be a megapin among our unpersisted children.
    for (size_t i = 0; i < pin_count; ++i) {
      assign_megapin_persistence(p[6 + i], smol_items_offset, big_items_offset,
                                 small_page_table, location_table,
                                 record_table);
    }
  }
}

void assign_pin_persistence(Val item,
                            size_t* smol_items_offset,
                            size_t* big_items_offset,
                            ht_t** small_page_table,
                            ht_t** location_table,
                            ht_t** record_table) {
  // OK, so we're now in the second phase here: we want to.
  if ((item >> 63) == 0) return; // direct

  // Check to see if the item is outside the persistence heap area. If it's on
  // the persistence heap, we don't need to do anything.
  u64* p = PTR(item);
  if (p >= persistence_start && p <= persistence_end) {
    return;
  }

  // Check to see if the item has a redirection header. If it does, it and all
  // its child items are actually on disk.
  u64 redirection = p[-2];
  if (redirection != 0) {
    if (redirection == (u64)-1) {
      die("TODO: Handle wait for remote sync");
    }

    // Check if we've already recorded the redirection.
    u64 idx = ht_has(*record_table, (u64)p);
    if (idx != 0xffffffffffffffff) {
      return;
    }

    c_ht_put(record_table, (u64)p, redirection);
    return;
  }

  /* printf("assign_pin_persistence(%lx) [cur big size=%lu]\n", */
  /*        item, *big_items_offset); */

  /* u64 bitsz = get_bitsz(item); */
  /* u64 wordsz = (bitsz + 63) / 64; */
  u64 pin_count = p[2];
  ENSURE_NOBEEF(pin_count);
  if (ismegapin(item)) {
    /* printf("megapin has %lu pins\n", pin_count); */

    /* g_item_debug = true; */

    // Don't record the megapin, but do record each pin in the megapin which
    // isn't already on disk and allocated.
    for (size_t i = 0; i < pin_count; ++i) {
      Val subitem = p[6 + i];
      u64* sptr = PTR(subitem);
      if (sptr >= persistence_start && sptr <= persistence_end) {
        continue;
      }

      u64 idx = ht_has(*location_table, subitem);
      if (idx != 0xffffffffffffffff) {
        continue;
      }

      assign_item_persistence(subitem,
                              smol_items_offset, big_items_offset,
                              small_page_table, location_table);
    }

    // Recur on the megapin items
    u64 base = 6 + pin_count * 2;
    u64 megacount = p[base];
    base++;
    for (size_t i = 0; i < megacount; ++i) {
      assign_pin_persistence(p[base + i],
                             smol_items_offset, big_items_offset,
                             small_page_table, location_table, record_table);
    }
  } else {
    u64 idx = ht_has(*location_table, item);
    if (idx != 0xffffffffffffffff) {
      return;
    }

    assign_item_persistence(item,
                            smol_items_offset, big_items_offset,
                            small_page_table, location_table);

    for (size_t i = 0; i < pin_count; ++i) {
      assign_pin_persistence(p[6 + i], smol_items_offset, big_items_offset,
                             small_page_table, location_table, record_table);
    }
  }
}

u64 get_dst_ptr_for_val(Val src, ht_t** record_table) {
  u64* p = PTR(src);
  u64 idx = ht_has(*record_table, (u64)p);
  if (idx == 0xffffffffffffffff) {
    // TODO: It's normal to not have recorded refs, but we might want a
    // debugging check that `p` is already pointing into the persistence heap.
    return (u64)p;
  }
  return c_ht_get(record_table, idx);
}

// Copy a single direct or indirect value to dst.
__int64_t copy_val(__int64_t crc, Val src, u64* dst, ptrdiff_t offset,
                   ht_t** record_table) {
  u64 val;

  if ((src >> 63) == 0) {
    // item is direct, just copy the val
    val = src;
  } else if ((src >> 56) == 0xC4) {
    // item is some sort of pin, translate it from the record table.
    u64 outptr = get_dst_ptr_for_val(src, record_table);
    // The item is on the persistence heap, so has crc32 values, so make sure
    // it's tagged properly as such.
    u64 tag = (src >> 48 | 0b10) << 48;
    val = tag | outptr;
  } else {
    // item is a pointer to inside the pin, just offset.
    val = src + (offset * 8);
  }

  /* printf("src = %lx, dst[%p] = %lx\n", src, dst, val); */

  crc = _mm_crc32_u64(crc, val);

  *dst = val;
  return crc;
}

// Copies a heap record inside a pin while threading through the crc32
// calculation.
__int64_t copy_heap_in_pin(__int64_t crc, u64* src, u64* dst, ptrdiff_t offset,
                           ht_t** record_table) {
  // Copy the heap tag verbatim.
  crc = _mm_crc32_u64(crc, src[-1]);
  dst[-1] = src[-1];

  // Read the heap tag type.
  u64 tag = src[-1] & 0b111;
  /* printf("tag: %lx\n", tag); */

  u64 heapbits = src[-1] >> 8;
  u64 heapwords = (heapbits + 63) / 64;

  switch (tag) {
    case 0x0:
      for (size_t i = 0; i < heapwords; ++i) {
        crc = _mm_crc32_u64(crc, src[i]);
        dst[i] = src[i];
      }
      break;

    case 0x2:
    case 0x3:
    case 0x4:
    case 0x5:
      for (size_t i = 0; i < heapwords; ++i) {
        crc = copy_val(crc, src[i], dst + i, offset, record_table);
      }
      break;

    default:
      die("todo: unhandled type to copy");
  }

  return crc;
}

// Given a source address, copy the pin/megapin pointed to by src into dst and
// rebuild the item at dst with an equivalent heap header and crc32 structure.
//
// src: src pointer to item
// dst: dst pointer to beginning of record; dst[0] = crc slot, dst[1] = heap tag
void copy_pin(bool megapin, u64* src, u64* dst, ht_t** record_table) {
  // TODO: Do `mov rax, 0xFFFFFFFF` to put a INT32_MIN into the 64-bit register
  // when translating for real crc32. This is only a signed int because of the
  // intrinsic's type signature.
  __int64_t crc = 0xFFFFFFFF;

  u64 pin_count = src[2];
  u64 megapin_count = megapin ? src[6 + 2 * pin_count] : 0;
  u64 crc_wordsz = wordsForCRC32s(pin_count + megapin_count);

  // Add the size of the crc32 words to the existing header.
  u64 new_header = src[-1] + (crc_wordsz << 14);
  dst[1] = new_header;
  crc = _mm_crc32_u64(crc, new_header);

  size_t tocopy = 6 + pin_count;
  if (megapin) {
    tocopy += pin_count + 1 + 2 * megapin_count;
  }

  // We have to calculate the offset that we add to the non-pin tagged
  // pointers. This offset is applied to all non-pin pointers because all
  // tagged pointers inside a pin should point inside the pin.
  //
  // This calculation is complicated by the headers of the new pin being larger
  // than the previous pin: you have to offset from the beginning of the data
  // sections instead of the.
  u64* srcdata = src + tocopy;
  u64* dstdata = dst + 2 + tocopy + crc_wordsz;
  ptrdiff_t offset = dstdata - srcdata;

  // Copy just the headers before the pin references.
  for (size_t i = 0; i < 6; ++i) {
    crc = copy_val(crc, src[i], &dst[2 + i], offset, record_table);
  }

  // Copy all the pins.
  for (size_t i = 0; i < pin_count; ++i) {
    crc = copy_val(crc, src[6 + i], &dst[2 + 6 + i], offset, record_table);
  }

  if (megapin) {
    // When copying a megapin, we can't just copy the pin lengths from the
    // source pin because they probably don't match the lengths of the dest
    // pins.
    for (size_t i = 0; i < pin_count; ++i) {
      u64* dstpin = (u64*)get_dst_ptr_for_val(src[6 + i], record_table);
      u64 len = dstpin[-1] >> 14;
      dst[2 + 6 + pin_count + i] = len;
      crc = _mm_crc32_u64(crc, len);
    }

    // Copy the megapin count over
    dst[2 + 6 + 2 * pin_count] = megapin_count;
    crc = _mm_crc32_u64(crc, megapin_count);

    // Copy the megapins
    for (size_t i = 0; i < megapin_count; ++i) {
      u64 mpoff = 6 + 2 * pin_count + 1 + i;
      crc = copy_val(crc, src[mpoff], &dst[2 + mpoff], offset, record_table);
    }

    // Lookup the new megapin lengths

    // Copy the megapin lengths:
    for (size_t i = 0; i < megapin_count; ++i) {
      u64 srcoff = 6 + 2 * pin_count + 1 + i;
      u64* dstpin = (u64*)get_dst_ptr_for_val(src[srcoff], record_table);
      u64 len = dstpin[-1] >> 14;

      u64 dstoff = 6 + 2 * pin_count + 1 + megapin_count + i;
      dst[2 + dstoff] = len;
      crc = _mm_crc32_u64(crc, len);
    }
  }

  u64 off = 2 + 6 + pin_count;
  if (megapin) {
    off += 1 + pin_count + 2 * megapin_count;
  }
  u64* crcdst = dst + off;
  u32* crcs   = (u32*)crcdst;

  for (size_t i = 0; i < pin_count; ++i) {
    u64 srcoff = 6 + i;
    u64* dstpin = (u64*)get_dst_ptr_for_val(src[srcoff], record_table);
    u32 val = (u32)dstpin[-2];
    crcs[i] = val;
  }
  crcs += pin_count;
  for (size_t i = 0; i < megapin_count; ++i) {
    u64 srcoff = 6 + 2 * pin_count + 1 + i;
    u64* dstpin = (u64*)get_dst_ptr_for_val(src[srcoff], record_table);
    u32 val = (u32)dstpin[-2];
    crcs[i] = val;
  }

  for (size_t i = 0; i < crc_wordsz; ++i ) {
    crc = _mm_crc32_u64(crc, crcdst[i]);
  }

  // TODO: Assert that we aren't copying a pin with existing crc32 records.

  // If the pin has additional memory here, we need to treat that as a set of
  // heap records and translate the contents of each record to the new
  u64 bitsz = src[-1] >> 8;
  u64 pinwords = (bitsz + 63) / 64;
  pinwords -= tocopy;
  u64* payload_src = src + tocopy;
  u64* payload_dst = dst + 2 + tocopy + crc_wordsz;
  while (pinwords > 0) {
    u64 heapbits = *payload_src >> 8;
    u64 heapwords = (heapbits + 63) / 64;
    payload_src++;
    payload_dst++;
    pinwords--;

    crc = copy_heap_in_pin(crc, payload_src, payload_dst, offset, record_table);

    payload_src += heapwords;
    payload_dst += heapwords;
    pinwords -= heapwords;
  }

  // Commit to the hash.
  crc ^= 0xFFFFFFFF;
  dst[0] = (u64)crc;

  // For recording, we must record pointing to the new payload past the header.
  dst += 2;
  c_ht_put(record_table, (u64)src, (u64)dst);
}

void traverse_and_persist(u64* ptr,
                          Val item,
                          size_t smol_size,
                          ht_t** location_table,
                          ht_t** record_table) {
  if ((item >> 63) == 0) return; // direct

  // Check to see if the item is inside the persistence heap area. If it's on
  // the persistence heap, we don't need to do anything.
  u64* p = PTR(item);
  if (p >= persistence_start && p <= persistence_end) {
    return;
  }

  // Don't duplicate write DAG objects.
  u64 x = ht_has(*record_table, (u64)p);
  if (x != 0xffffffffffffffff) { // TODO: Just use -1 in the assembly.
    return;
  }

  // Depth-first traversal first, to ensure everything this item depends on is
  // already on disk, with a calculated crc32 so we can look that up when we
  // copy the crc32 for that item.
  bool mp = ismegapin(item);
  if (mp) {
    // Recur on the pins in the megapins, to traverse them in depth first
    // order.
    u64 pin_count = p[2];
    ENSURE_NOBEEF(pin_count);
    for (size_t i = 0; i < pin_count; ++i) {
      traverse_and_persist(ptr, p[6 + i], smol_size,
                           location_table, record_table);
    }

    u64 megapin_offset = 6 + 2 * pin_count;
    u64 megapin_count = p[megapin_offset];
    for (size_t i = 0; i < megapin_count; ++i) {
      traverse_and_persist(ptr, p[megapin_offset + i], smol_size,
                           location_table, record_table);
    }
  } else {
    u64 pin_count = p[2];
    ENSURE_NOBEEF(pin_count);
    for (size_t i = 0; i < pin_count; ++i) {
      traverse_and_persist(ptr, p[6 + i], smol_size,
                           location_table, record_table);
    }
  }

  u64 locationidx = ht_has(*location_table, item);
  if (locationidx == 0xffffffffffffffff) {
    die("missing target destination location");
  }
  u64 locationval = c_ht_get(location_table, locationidx);
  u64 location_tag = locationval & 0b11;
  u64 location_off = locationval >> 2;

  u64* dst;
  if (location_tag == SMOL_ITEM /* 0 */) {
    dst = ptr + location_off;
  } else {
    dst = ptr + smol_size + location_off;
  }
  /* printf("locationval: %ld, dst: %p\n", locationval, dst); */

  // Recheck that p hasn't been corrupted in the mean time.
  ENSURE_NOBEEF(p[2]);

  copy_pin(mp, p, dst, record_table);
}

// Writes the unpersisted parts of `item` to disk, without changing any
// superblock metadata.
void write_item(Val item,
                ht_t** location_table /* inout */,
                size_t* smol_size /* out */,
                u64** memdst /* out */,
                u64* memsz /* out */,
                Val* outitem /* out */) {
  // TODO: check the degenerate case where item is already on the persistence
  // heap.

  ht_t* record_table = ht_create(5);

  // Do a round of fake assigning for the new method:
  size_t big_size = 0;
  // small_page_table maps (page aligned start + 1) => size; increment because
  // 0 is a valid page aligned offset.
  ht_t* small_page_table = ht_create(5);
  assign_megapin_persistence(item, smol_size, &big_size, &small_page_table,
                             location_table, &record_table);
  /* printf("After megapin scan: Smol size: %ld, Big: %ld\n", */
  /*        *smol_size, big_size); */
  assign_pin_persistence(item, smol_size, &big_size, &small_page_table,
                         location_table, &record_table);
  /* printf("After pin scan: Smol size: %ld, Big: %ld\n", *smol_size, big_size); */

  // We have to round the smol_size upwards to the next page
  *smol_size = round_up_align(*smol_size, page_size_words);
  *memsz = *smol_size + big_size;
  /* printf("Allocation amount: %ld\n", *memsz); */
  u64* newptr = allocate_persistence_space_for(*memsz);

  // Iterate over the small page table and write all the small page
  // headers. Remember: key is offsetted by 1 for hashtable reasons because 0
  // is a valid offset.
  for (size_t i = 0; i < small_page_table->mask + 1; ++i) {
    // Reminder: use ht_key/ht_val in the translation.
    if (small_page_table->tbl[i].key != 0) {
      u64 key = small_page_table->tbl[i].key - 1;
      u64 val = small_page_table->tbl[i].val;
      newptr[key] = val;
    }
  }
  buddy_free((u64*)small_page_table);

  traverse_and_persist(newptr, item, *smol_size,
                       location_table, &record_table);

  /* printf("pointer: %p\n", newptr); */

  // Return a tagged reference to the first item in the newly allocated stack.
  u64* itemptr = (u64*)get_dst_ptr_for_val(item, &record_table);
  u64 mask;
  if (ismegapin(item)) {
    mask = 0xC403000000000000;
  } else {
    mask = 0xC402000000000000;
  }
  *outitem = mask | (u64)(itemptr);

  *memdst = newptr;

  buddy_free((u64*)record_table);
}

// Persist unpersisted parts of `item` to disk.
//
// - If an item's pointer is page aligned, it takes up the full or multiple
//   pages depending on the size of its heap header. Items that are more than
//   half a page in size should always get their own page, and just eat the
//   internal fragmentation.
//
// - If an item's pointer is anywhere else in the page, it's a) smaller than a
//   page, and b) one of multiple items on a page, where the first u64 on a
//   page is a reference to the first of the end of the free space.
//
Val persist_item(Val item) {
  u64* p = PTR(item);
  ENSURE_NOBEEF(*p);

  ht_t* location_table = ht_create(5);
  size_t smol_size = 0;
  u64* memdst = 0;
  u64 memsz = 0;
  Val outitem = 0;
  write_item(item, &location_table, &smol_size, &memdst, &memsz, &outitem);

  ENSURE_NOBEEF(*p);

  // Force an msync and a persistence flip here.
  msync_region(memdst, memsz);
  update_superblock(outitem);

  ENSURE_NOBEEF(*p);

  // Now that the item is on disk, iterate across the record table that maps
  // buddy pins to their location on disk and write the target back into the
  // relocation header.
  for (size_t i = 0; i < location_table->mask + 1; ++i) {
    if (location_table->tbl[i].key != 0) {
      Val item = location_table->tbl[i].key;
      u64 locationval = location_table->tbl[i].val;
      u64 location_tag = locationval & 0b11;
      u64 location_off = locationval >> 2;

      u64* dst;
      if (location_tag == SMOL_ITEM /* 0 */) {
        dst = memdst + location_off;
      } else {
        dst = memdst + smol_size + location_off;
      }
      // Point to the first item.
      dst += 2;

      u64* srcptr = PTR(item);
      if (srcptr[-2] != 0 ) {
        die("Nonzero redirection header?");
      }
      srcptr[-2] = (u64)dst;
    }
  }

  buddy_free((u64*)location_table);

  return outitem;
}

/* -------------------------------------------------------------------- */

u64	zcall1	(u64(*)(u64), u64);
Val MkPin(Val);

Val precommit_shim(Val item) {
  /* printf("precommit_shim(%lx)\n", item); */
  if (persistence_fd == -1) {
    // In temporary XPLAN mode, don't commit because there's no place to
    // commit to.
    return item;
  } else {
    return persist_item(item);
  }
}

Val commit_shim(Val item) {
  if (persistence_fd == -1) {
    return item;
  } else {
    // We always wrap the item in a toplevel pin to force evaluation of
    // thunks, which the persistence system can't deal with, and to create
    // a pin structure for dependency information.
    Val x = zcall1(MkPin, item);
    Val persisted = persist_item(x);
    update_superblock(persisted);
    Val unpinned = PTR(persisted)[0];
    return unpinned;
  }
}

/* -------------------------------------------------------------------- */

void calculate_item_range(Val item, u64 wordsz,
                          uintptr_t* start, uintptr_t* end) {
  u64* ptr = PTR(item);

  // Every item has a crc32 and a size header that must be counted.
  ptr -= 2;
  wordsz += 2;

  if ((u64)ptr % page_size == 0) {
    // item is page aligned. that means it's its own item that consumes one or
    // more complete pages and we round the length up to the nearest page.
    u64 rounded_size =
        ((wordsz + page_size_words - 1) / page_size_words) * page_size_words;
    *start = (uintptr_t)ptr;
    *end = (uintptr_t)(ptr + rounded_size);
  } else {
    // item is not page aligned. that means it's a small item on a grouped
    // page.
    if ((u64)ptr % page_size == 8) {
      // if it's the first item in the page, then adjust ptr downward one word
      // to cover the initial page header.
      ptr--;
      wordsz++;
    }

    // Get the number at the start of the page.
    u64* page_start = (u64*)round_down_align((u64)ptr, page_size);
    u64 page_len = *page_start;

    if (ptr + wordsz == page_start + page_len) {
      // if it's the last item in the page, adjust size upwards to the
      // end of the page so we can effectively merge with the next page.
      u64* next_page = page_start + page_size_words;
      wordsz = next_page - ptr;
    }

    *start = (uintptr_t)ptr;
    *end = (uintptr_t)(ptr + wordsz);
  }
}

bool mark_item_range(Val item, u64 wordsz, rb_tree* tree, bump_alloc_t* a) {
  uintptr_t start = 0;
  uintptr_t end = 0;
  calculate_item_range(item, wordsz, &start, &end);

  // We want to check if the current item is marked in the range so we don't
  // redundantly explore large shared trees.
  rb_node* start_node = rb_find_containing(tree, start);
  if (start_node && end <= start_node->end) {
    return true;
  }

  rb_node* n = bump_alloc_alloc(a);
  rb_node_init(n, start, end);
  rb_insert_or_coalesce(tree, n);

  return false;
}

// Marks an item, returning true if the item is already contained in the tree.
bool mark_item_in_tree(Val item, rb_tree* tree, bump_alloc_t* a) {
  u64 bitsz = get_bitsz(item);
  u64 wordsz = (bitsz + 63) / 64;
  return mark_item_range(item, wordsz, tree, a);
}

// Given an 'item', mark the adjusted memory range of it.
void mark_ranges(Val item, rb_tree* tree, bump_alloc_t* a) {
  // Direct items take up no space of their own.
  if ((item >> 63) == 0) return;

  if (mark_item_in_tree(item, tree, a)) {
    return;
  }

  u64* ptr = PTR(item);
  u64 pin_count = ptr[2];
  if (ismegapin(item)) {
    u64 megapin_count = ptr[6 + 2 * pin_count];

    // In a megapin, we don't recur into pins, we just mark the pin ranges here
    // one by one.
    for (size_t i = 0; i < pin_count; ++i) {
      mark_item_range(
          ptr[6 + i],
          ptr[6 + pin_count + i],
          tree, a);
    }

    // Now recur through the megapins
    for (size_t i = 0; i < megapin_count; ++i) {
      mark_ranges(ptr[6 + 2 * pin_count + 1 + i], tree, a);
    }
  } else {
    // It's a normal pin, we have to recursively go through its items
    for (size_t i = 0; i < pin_count; ++i) {
      mark_ranges(ptr[6 + i], tree, a);
    }
  }
}

/* -------------------------------------------------------------------- */

// inb4 leddit hate on using DONTNEED. MADV_FREE doesn't work with file backed
#define MADV_DONTNEED          4

#define FALLOC_FL_KEEP_SIZE 1
#define FALLOC_FL_PUNCH_HOLE 2

void punch_hole(u64 base, u64 end) {
  /* printf("Punch hole [%lx, %lx)\n", base, end); */
  u64 disk_start = (u64)base + headersize - base_addr;
  u64 len = end - (u64)base;

  syscall_fallocate(persistence_fd, FALLOC_FL_KEEP_SIZE | FALLOC_FL_PUNCH_HOLE,
                    disk_start, len);

  // Drop the backing memory pages of these items.
  syscall_madvise((u64*)base, len, MADV_DONTNEED);
}

void persist_collection() {
  // TODO: This is only valid while we have a single persist()/freeze()
  // function. Once we have precommit()/cool() which writes data to disk
  // without changing the toplevel committed value.
  /* printf("warning: persist_collection() unsafe until integrated with G1/G2\n"); */
  /* return; */

  bump_alloc_t alloc;
  bump_alloc_init(&alloc, sizeof(rb_node));

  rb_tree tree;
  rb_init(&tree);

  mark_ranges(active_sb.val, &tree, &alloc);
  /* print_inorder(used_ranges); */

  u64 base = (u64)persistence_start;
  /* u64 end = (u64)persistence_cur; */

  // Move to the leftmost node.
  rb_node* head = tree.first;

  // Iterate the tree of used_ranges, freeing the negative space between
  // entries.
  while (head) {
    uintptr_t start = head->start;
    uintptr_t end = head->end;
    /* printf("Live Range [%lx, %lx)\n", start, end); */

    u64 range_page_start = round_down_align((u64)start, page_size);
    u64 range_page_end = round_up_align((u64)end, page_size);
    /* printf("- range_start: %lx\n", range_page_start); */
    /* printf("- range_end: %lx\n", range_page_end); */

    if (base < range_page_start) {
      punch_hole(base, range_page_start);
    }

    /* printf("- base = %lx\n", range_page_end); */
    base = range_page_end;

    head = head->next;
  }

  // TODO: We also should do one final attempt to clear from the final
  // range_page_end to the active write_offset, but I won't be confident in
  // that until we're in a situation where that could happen, which means
  // backfilling free lists.

  // TODO: For when we start doing concurrent collection, we'll have to also
  // think about how allocating to write_offset interacts with the above.

  // Finally, we recommit the value of the current active superblock as a new
  // superblock because the current inactive superblock is highly likely to be
  // invalid after garbage collection. In the case we crash right before this,
  // it's fine since invalid superblocks are just going to have a warning
  // message printed out.
  active_sb.sequence = active_sb.sequence + 1;
  write_superblock(persistence_fd, inactive_offset, &active_sb);

  bump_alloc_free(&alloc);
}

Val get_active_superblock_val() {
  return active_sb.val;
}

void scrub_active_superblock() {
  scrub(active_sb.val, active_sb.val_checksum);
}
