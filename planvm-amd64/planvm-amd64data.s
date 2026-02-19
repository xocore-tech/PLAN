.section .rodata
.align 8
hash_table_load_factor:
	.double	0.7
.section .data
.set HEAP_MAX_RANK,	51

.global heap_sizes
.align 8
heap_sizes:
	.zero HEAP_MAX_RANK * 4
.global heap_addr, mapd_size, heap_size, heap_rank
heap_addr:	.quad 0
mapd_size:	.quad 0
heap_size:	.quad 0
heap_rank:	.quad 0
.global	min_alloc_log2, max_alloc_log2
min_alloc_log2: .quad 0
max_alloc_log2: .quad 0
.global min_alloc, max_alloc, bucket_count
min_alloc: .quad 0
max_alloc: .quad 0
bucket_count: .quad 0
.global buddy_mmap_ptr, base_ptr, base_end_ptr
buddy_mmap_ptr: .quad 0
base_ptr: .quad 0
base_end_ptr: .quad 0
.global buckets_ptr
buckets_ptr: .quad 0
.global node_state_ptr
node_state_ptr: .quad 0
.global explore_tree_ptr
explore_tree_ptr: .quad 0
.global explore_max_idx
explore_max_idx: .quad 0
.global sweep_tree_ptr
sweep_tree_ptr: .quad 0
.global new_allocation_ptr
new_allocation_ptr: .quad 0
.global bitmap_bytes
bitmap_bytes: .quad 0
.global sweep_level
sweep_level: .quad -1

.global sweep_node_ptr
sweep_node_ptr: .quad 0

.global sweep_action_ptr
sweep_action_ptr: .quad 0
.global collector_lock
collector_lock:
	.zero	8*7
.global bucket_locks_ptr
bucket_locks_ptr: .quad 0
.global self_fd
self_fd:	.quad 0
.section .data
### ELF header of the executable loaded first thing on execution.
.align 16
elf_header:
	.zero	64

.section .rodata
proc_self_exe:
    .string "/proc/self/exe"
proc_fd_path:
    .string "/proc/self/fd/3"
plan_interpreter:
    .string "plan_interpreter"
empty_str:
    .asciz ""
.section .data
.global profile_base, profile_next, profile_end
profile_base:	.quad 0
profile_next:	.quad 0
profile_end:	.quad 0
