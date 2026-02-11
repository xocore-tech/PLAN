## Copyright (c) 2026 xoCore Technologies
## SPDX-License-Identifier: MIT
## See LICENSE for full terms.
### constants.s
# -*- tab-width: 8; indent-tabs-mode: t -*-

.intel_syntax noprefix

# Linux syscall numbers
.equ	SYS_READ,       0
.equ	SYS_WRITE,      1
.equ	SYS_OPEN,       2
.equ	SYS_CLOSE,      3
.equ	SYS_FSTAT,      5
.equ	SYS_MMAP,       9
.equ	SYS_MUNMAP,	11
.equ	SYS_PREAD64,    17
.equ	SYS_PWRITE64,   18
.equ	SYS_MSYNC,      26
.equ	SYS_MADVISE,    28
.equ	SYS_SENDFILE,	40
.equ	SYS_SENDTO,	44
.equ	SYS_CLONE,	56
.equ	SYS_EXIT,	60
.equ	SYS_FSYNC,      74
.equ	SYS_FTRUNCATE,  77
.equ	SYS_PERSONALITY, 135
.equ	SYS_FUTEX,	202
.equ	SYS_EXIT_GROUP, 231
.equ	SYS_FALLOCATE, 285
.equ	SYS_MEMFD_CREATE, 319
.equ	SYS_EXECVEAT,	322
.equ	SYS_CLONE3,	435

# open flags
.equ	O_RDWR,		2
.equ	O_RDONLY,	0

# memfd_create flags
.equ	MFD_CLOEXEC,	1

.equ	AT_EMPTY_PATH,	0x1000

.equ	ADDR_NO_RANDOMIZE, 0x0040000

# SYS_clone3 arguments
.equ CLONE_VM, 0x00000100
.equ CLONE_FS, 0x00000200
.equ CLONE_FILES, 0x00000400
.equ CLONE_SIGHAND, 0x00000800
.equ CLONE_THREAD, 0x00010000
.equ CLONE_PARENT_SETTID, 0x00100000
.equ CLONE_CHILD_CLEARTID, 0x00200000

# SYS_futex arguments
.equ FUTEX_WAIT, 0
.equ FUTEX_WAKE, 1
.equ FUTEX_CMP_REQUEUE, 4
.equ FUTEX_PRIVATE_FLAG, 128

# Futex
.equ FUTEX_WAKE_ONE, 1
.equ FUTEX_WAKE_ALL, 0x7fffffff

# SYS_mmap arguments
.equ MAP_PRIVATE, 0x02
.equ MAP_ANONYMOUS, 0x20
.equ MAP_STACK, 0x20000

# SYS_mmap arguments
.equ PROT_READ, 1
.equ PROT_WRITE, 2
