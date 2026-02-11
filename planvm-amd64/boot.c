// Copyright (c) 2026 xoCore Technologies
// SPDX-License-Identifier: MIT
// See LICENSE for full terms.
// -*- tab-width: 8 -*-

// boot.c: main startup path, which either loads a seed or runs the embedded
// program in this binary.

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdnoreturn.h>
#include <stddef.h>
#include "printf.h"
#include "common.h"

typedef uint64_t u64;
typedef uint64_t Val;
typedef uint32_t u32;
typedef uint8_t  u8;

register u64 *heap_end  asm ("r13");
register u64 *heap_next asm ("r14");
register u64 *sp        asm ("r15");

#define push(w) (*(--sp) = w)

u64	zcall1	(u64(*)(u64), u64);

noreturn void sys_exit(u64);
void	sys_write      (char*, u64);
int syscall_open(const char* filename, int flags, int mode);
int syscall_close(int fd);
int syscall_fstat(int fd, struct stat *st);
int syscall_ftruncate(int fd, off_t len);
int syscall_sendfile(int out, int in, off_t* offset, size_t count);
Val  mkclosure (u64);

void push_strnat (const char *str);

Val get_active_superblock_val();
void scrub_active_superblock();
void persist_collection();;
Val persist_item(Val item);
void update_superblock(Val item);

extern int self_fd;
extern off_t elf_binary_end();


static inline u64 *PTR (Val x) {
	return (void*) ((x << 16) >> 16);
}

extern int string_length (const char *x);
extern noreturn void die (const char *msg);

extern int self_fd;

extern off_t elf_binary_end();
extern void init_persistence(int fd);
extern Val MkPin(Val);

u64 seedfile     (const char *filename);
void repl        (u64 argArray);
bool streq       (const char *a, const char *b);

extern const char* getenveq(const char*);

// The selffd, stack and loom are already setup in asm before this is called.
int boot_main (int argc, char **argv) {
  const char* cmd = getenveq("PLANCMD=");
  if (cmd && streq(cmd, "license")) {
    printf("PLAN native runtime, Version r0\n");
    printf("Copyright (C) 2026 xoCore Technologies\n");
    return 0;
  }

  // Get the file size
  struct stat st;
  if (syscall_fstat(self_fd, &st) < 0) {
    syscall_close(self_fd);
    die("Failed to get file size");
  }

  u64 elf_len = elf_binary_end();
  if (st.st_size == elf_len) {
    if (argc != 3) {
      die("Usage: ./boot <seedfile> <binary>\n");
    }

    // Our own binary does not have a persistence section. That means this is
    // in seed loader mode.

    int outfd = syscall_open(argv[2], O_CREAT|O_RDWR, 0700);
    if (syscall_ftruncate(outfd, elf_len) == -1) {
      die("Failed to extend file");
    }

    // Copy the interpreter part out of our own binary.
    syscall_sendfile(outfd, self_fd, 0, elf_len);

    // Initialize the persistence heap with our outfile descriptor instead of
    // our selffd descriptor.
    init_persistence(outfd);

    // Load the input seed file.
    *(--sp) = seedfile(argv[1]);

    // Persist the item into the binary.
    Val in = *sp++;
    Val pin = zcall1(MkPin, in);
    Val persisted_item = persist_item(pin);
    update_superblock(persisted_item);
    return 0;
  } else {
    // This binary has a persistence section. That means we have a program and
    // should just mount our own binary as the persistence store.
    init_persistence(self_fd);
    Val pin = get_active_superblock_val();

    // If we were asked to scrub the binary through the PLANCMD, scrub and exit.
    if (cmd && streq(cmd, "scrub")) {
      scrub_active_superblock();
      return 0;
    }

    *(--sp) = PTR(pin)[0];

    // If we were asked to do an offline collection, run the collector instead
    // of running the binary normally.
    if (cmd && streq(cmd, "collect")) {
      persist_collection();
      return 0;
    }

    switch (argc) {
    case 1:
        repl(0);
    default:
        for (int i=argc-1; i>0; i--) push_strnat(argv[i]);
        push(0);
        u64 argVec = zcall1(mkclosure, argc-1);
        repl(argVec);
        return 0;
    }
  }
}
