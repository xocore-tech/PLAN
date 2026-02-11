## Copyright (c) 2026 xoCore Technologies
## SPDX-License-Identifier: MIT
## See LICENSE for full terms.
### plan.s
# -*- tab-width: 8; indent-tabs-mode: t -*-

.intel_syntax noprefix

.include "planvm-amd64/constants.s"

.include "planvm-amd64/planvm-amd64.s"

### Data Section

.section .data
.include "planvm-amd64/planvm-amd64data.s"

.section .data
chosen_main:	.quad 0
stack_last:	.quad 0
exn_top:	.quad 0

argc:   .quad 0
argv:   .quad 0
envp:   .quad 0
auxp:   .quad 0

### Text Section

.section .text
.global xhole, xdone, xhead, xvar, xunknown, xunknownnoupdate
.global rpn_gc
.global mklaw, claim, reserve
.global c_reserve, c_claim
.global mkthunk, c_mkclosure, c_MkLaw, mkpin
.global c_mkpin
.global c_div
.global c_and, c_or, c_xor
.global c_eval, c_force
.global repl
.global stack_last
.global syscall_open
.global syscall_close
.global syscall_fstat
.global syscall_pread64
.global syscall_pwrite64
.global syscall_madvise
.global syscall_mmap
.global syscall_msync
.global syscall_write
.global syscall_read
.global	syscall_exit
.global syscall_exit_group
.global syscall_fsync
.global syscall_ftruncate
.global syscall_fallocate
.global syscall_sendfile
.global Judge
.global copy_qwords
.global mkapp

### Temporary export while building megapins.
.global c_ht_put, c_ht_get, ismegapin
.global pinarity

### Process Initialization

syscall_open: # rdi/filename rsi/flags => rax/fd
	mov     eax, SYS_OPEN
	syscall
	ret
syscall_open_chk:
	call	syscall_open
	test	rax, rax                    # if (fd >= 0)
	jns	1f                          # then ok
	ud2                                 # else crash
1:	ret
syscall_close:
	mov     rax, SYS_CLOSE
	syscall
	ret
syscall_close_chk:
	call	syscall_close               # close(rd)
	test	rax, rax                    # if (code == 0)
	jz	1f                          # then ok
	ud2                                 # else crash
1:	ret
syscall_fstat:
	mov     rax, SYS_FSTAT
	syscall
	ret
syscall_fstat_chk:
	call	syscall_fstat
	test	rax, rax                    # if (code == 0)
	jz	1f                          # then ok
	ud2                                 # else crash
1:	ret
syscall_pread64:
	mov     rax, SYS_PREAD64
	mov     r10, rcx
	syscall
	ret
syscall_pwrite64:
	mov     rax, SYS_PWRITE64
	mov     r10, rcx
	syscall
	ret
syscall_madvise:
	mov     rax, SYS_MADVISE
	syscall
	ret
syscall_mmap: # TODO
	mov     eax, 9                      # syscall_id = 9 (mmap)
	mov     r10, rcx                    # ?
	syscall
	ret
syscall_mmap_chk:
	call	syscall_mmap                # mmap(ptr,len,prot,flags,fd,off)
	test	rax, rax                    # if (code >= 0)
	jns	1f                          #   then okay
	ud2                                 #   else crash
1:	ret
syscall_munmap:
	mov     eax, 11                     # syscall_id = 11 (munmap)
	syscall
	ret
syscall_munmap_chk:
	call	syscall_munmap           # munmap(ptr, len)
	test	rax, rax                 # if (rax == 0)
	jz	1f                       #   then ok
	ud2                              #   else err
1:	ret                              # return
syscall_msync:
	mov     rax, SYS_MSYNC
	syscall
	ret
syscall_read: # rdi/fd rsi/ptr rdx/size => rax/count
	xor	eax, eax                    # syscall_id = 0 (read)
	syscall
	ret
syscall_write: # rdi/fd rsi/ptr rdx/size => rax/count
	mov	eax, SYS_WRITE
	syscall
	ret
syscall_exit: # rdi/code => *noreturn*
	mov	eax, SYS_EXIT
	syscall
	ud2
syscall_exit_group: # rdi/code => *noreturn*
	mov	eax, SYS_EXIT_GROUP
	syscall
	ud2
syscall_fsync:
	mov     rax, SYS_FSYNC
	syscall
	ret
syscall_ftruncate:
	mov     rax, SYS_FTRUNCATE
	syscall
	ret
syscall_fallocate:
	mov     rax, SYS_FALLOCATE
	mov     r10, rcx
	syscall
	ret
syscall_sendfile:
	mov     rax, SYS_SENDFILE
	mov     r10, rcx
	syscall
	ret


mapstack:
	# mmap() a large amount of space for the stack.
	xor	rdi, rdi
	mov	rsi, 0x100000000
	mov	rdx, 3
	mov	r10, 0x22
	mov	r8, -1
	xor	r9, r9
	mov	rax, SYS_MMAP
	syscall
	cmp	rax, -1
	je	mapstack.oom
	ret
mapstack.oom:
	ud2

initialize_stack_globals:
	add	rax, rsi
	sub	rax, 8
	mov	[stack_last], rax
	add	rax, 8
	mov	r15, rax
	ret

madvise_dontdump_stack:
	mov	rdx, 16            # MADV_DONTDUMP
	call	syscall_madvise
	test	rax, rax
	jnz	madvise_dontdump_stack.fail
	ret
madvise_dontdump_stack.fail:
	ud2

mapheap:				    # mapheap:
	mov	edi, [heap_sizes]	    #   start with smallest word size
	mov	[mapd_size], rdi	    #   set mapped size
	mov	[heap_size], rdi	    #   set heap size in words
	call	heap_new		    #   heap_new(heap_sizes[0])
	mov	[heap_addr], rax	    #   heap_addr = rax
	mov	r14, rax		    #   next available heap pointer
	mov	rax, [heap_size]	    #   rax = smallest word size
	lea	r13, [r14 + 8*rax]	    #   just after end-of-heap
	ret

### Garbage Collection

### rdi: number of u64 words for the heap size.
heap_new:
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	mov	rsi, 0x007
	call	buddy_malloc
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	test	rax, rax
	js	heap_new.oom
	ret
heap_new.oom:
	ud2

# copyref
#
# Args: rdi is a pointer to a memory-slot containing a reference.
#
# Outputs: none
#
# Clobbers: rax, rsi, rdx, rcx, r8, r9
#
# Preserves: Must not touch r10, r11 and r12, which are reserved for its
# callers.
#
# Result: the reference pointed to by rdi is updated to point to the
# new location.


copyref:                                    # copyref:
	mov	r9, [rdi]                   #   r9 = actual value
	jheap	r9, copyref.indirect        #   if direct return
	ret
copyref.indirect:                           # indirect:
	ptr_	rdx, r9                     #   strip pointer tag
	cmp	rdx, rbx                    #   if (p < fromspace)
	jb	copyref.oob                 #     goto oob
	cmp	rdx, rbp                    #   if (p >= fromspace_end)
	jae	copyref.oob                 #     goto oob
	lea	rsi, [rdx-8]                #   rsi = record ptr
	mov	rdx, [rsi]                  #   load RECORD
	jheap	rdx, copyref.moved          #   record=ref means moved
	cmp	dl, 5                       #   if (gctype == thunk)
	je	copyref.thunk               #     goto thunk
copyref.normal:                             # normal:
	add	rdx, 16128                  #   bitsize<<8 + 63<<8
	shr	rdx, 14                     #   wordsz
	inc	rdx                         #   node size
	mov	rax, rsi		    #   copy record ptr now in rax
	cmp	rdx, 3			    #   if (wordsz > 3)
	ja	copyref.normallarge	    #   then use rep movsq fallback
	test	rdx, rdx		    #   if (wordsz == 0)
	jz	copyref.normaldone	    #   then done
	mov	r8, [rsi]		    #   copy [0]
	mov	[r14], r8		    #   place [0]
	cmp	rdx, 1			    #   if (wordsz == 1)
	je	copyref.normaldone	    #   then done
	mov	r8, [rsi+8]		    #   copy [1]
	mov	[r14+8], r8		    #   place [1]
	cmp	rdx, 2			    #   if (wordsz == 2)
	je	copyref.normaldone	    #   then done
	mov	r8, [rsi+16]		    #   copy [2]
	mov	[r14+16], r8		    #   place [2]
	jmp	copyref.normaldone
copyref.normallarge:
	mov	r8, rdi			    #   save rdi to r8
	mov	rdi, r14                    #   destiniation = heap_next
	mov	rcx, rdx                    #   count = node_size
	rep	movsq
	mov	rdi, r8			     # restore rdi
copyref.normaldone:
	lea	rcx, [r14+8]
	lea	r14, [r14 + rdx*8]          # heapnext = &heapnext[sz]
	mov	rdx, r9
	shr	rdx, 48
	shl	rdx, 48                     # get the type tag (high 16 bits)
	or	rdx, rcx
	mov	[rax], rdx                  # write new ref to RECORD
copyref.moved:
	mov	[rdi], rdx
copyref.oob:
	ret
copyref.thunk:                              # thunk:
	mov	rcx, [rsi+8]                #   rdi = exec
	cmp	rcx, OFFSET [xdone]         #   if (exec==xdone)
	je	copyref.xdone               #     goto xdone
	cmp	rcx, OFFSET [xhole]         #   if (exec==xhole)
	je	copyref.xhole               #     goto xhole
	jmp	copyref.normal              #   goto normal
copyref.xhole:
	mov	qword ptr [r14], 0x8005 # HDR={ty=THK, sz=128bits}
	mov	qword ptr [r14+8], rcx  # node[0]  = xhole
	mov	qword ptr [r14+16], 0   # node[1]  = 0 (placeholder)
	add	r14, 8                  # heapnext++
	mov	rax, 0xE000000000000000
	or	rax, r14
	add	r14, 16                 # heapnext = &heapnext[2]
	mov	[rsi], rax              # old.gcheader = result
	mov	[rdi], rax              # *p = result
	ret                             # return
	# if node.gc.size==1 goto normal
	# mov	rdi, [rsi]
	# cmp	rdi, 0x8005    # if node.hdr == {ty=thunk, sz=128(bits)}
	# je	copyref.normal
copyref.xdone:
	mov	r9, [rsi+16]
	mov	[rdi], r9      # else *ptr = node[1]; continue
	jmp	copyref

# copystack has no inputs or outputs
#
# Clobbers: rdi, rsi, rcx, rdx, r8, r9, r10, rax
copystack:
	mov	rdi, [stack_last]	     # set p/rdi to stack-bottom
copystack.loop:				     #
	call	copyref			     # copyref(p)
	sub	rdi, 8			     # p--
	cmp	rdi, r15		     # while (rdi >= r15)
	jae	copystack.loop		     #
	ret				     # return

# copyheap has no inputs or outputs and clobbers everything.


### copyheap has no inputs, and owns the r10, r11 and r12 registers

copyheap:
	mov	r10, [heap_addr]               # r10 = heap_addr
copyheap.loop:
	cmp	r10, r14                       # if (r10 >= heap_end) return
	jae	copyheap.done
	mov	rax, [r10]                     # rax = *ptr (RECORD)
	gcsz_	r11, rax                       # r11 = nodesz(rax/gcrecord)
	test	al, al                        # al=type
	jz	copyheap.node_done            # nat case (if ty=0)
	mov	r12, 1                        # i=1
copyheap.nodeloop:
	cmp	r12, r11                       # if (i > nodesz)
	ja	copyheap.node_done            #     jmp node_done
	lea	rdi, [r10 + r12*8]             # dst = &p[i]
	call	copyref
	inc	r12
	jmp	copyheap.nodeloop
copyheap.node_done:
	lea	r10, [r10+r11*8+8]               # p = &p[sz+1]
	jmp	copyheap.loop
copyheap.done:
	ret

rpn_gc:
	push	rdi
	call	before_gc
	call	memcheck_forced_before_gc
	pop	rdi
	push	rdi
	push	rbp
	push	rbx
	call	asmgc
	pop	rbx
	pop	rbp
	pop	rdi
	call	RequestGC2Op
	call	after_gc
	call	memcheck_forced_after_gc
	ret

gc:
	ppush	r11, r10, r9, r8, rbp, rsi, rdx, rcx, rbx, rax
	push	rdi
	call	before_gc
	cmp	qword ptr [profile_next], 0 #   if profiling disabled
	je	gc.startrec		    #   then skip
	mov	rdi, 2			    #   rdi = begin gc type tag
	mov	rsi, [heap_size]	    #   rsi = old heap size
	call	recordevent		    #   recordevent(2, heap_size)
gc.startrec:
	pop	rdi
	push	rdi
	push	r12    # r12 is not a root, but we still preserve it
	call	asmgc
	pop	r12    # restore r12
	cmp	qword ptr [profile_next], 0 #   if profiling disabled
	je	gc.endrec		    #   then skip
	mov	rdi, 3
	mov	rsi, [heap_size]	    # recordevent(3, heap_size)
	call	recordevent
gc.endrec:
	pop	rdi
	call	submit_thread_marks
	## call	print_heap_summary
	call	after_gc
	ppop	r11, r10, r9, r8, rbp, rsi, rdx, rcx, rbx, rax
	ret

free_old_heap:
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
	push	r9
	mov	rdi, rbx # fromspace.top
	call	buddy_free
	pop	r9
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
	ret

### Nat Utilities

bufshr1:
	mov	rax, rsi
	dec	rax
	xor	r8, r8
bufshr1.loop:
	mov	r9, [rdi + rax*8]
	mov	r10, r9
	shr	r10, 1
	mov	rcx, r8
	shl	rcx, 63
	or	r10, rcx
	mov	[rdi + rax*8], r10
	and	r9, 1
	mov	r8, r9
	test	rax, rax
	jz	bufshr1.finalize
	dec	rax
	jmp	bufshr1.loop
bufshr1.finalize:
	mov	rax, rsi
	lea	rcx, [rsi - 1]
	mov	r9, [rdi + rcx*8]
	test	r9, r9
	jnz	bufshr1.return
	dec	rax
bufshr1.return:
	ret

bufor:
	xor	rcx, rcx
bufor.loop:
	cmp	rcx, rdx
	jge	bufor.done
	mov	rax, [rsi + rcx*8]
	or	[rdi + rcx*8], rax
	inc	rcx
	jmp	bufor.loop
bufor.done:
	ret
bufxor:
	xor	rcx, rcx
bufxor.loop:
	cmp	rcx, rdx
	jge	bufxor.done
	mov	rax, [rsi + rcx*8]
	xor	[rdi + rcx*8], rax
	inc	rcx
	jmp	bufxor.loop
bufxor.done:
	ret

bufand:
	xor	rcx, rcx
bufand.loop:
	cmp	rcx, rdx
	jge	bufand.done
	mov	rax, [rsi + rcx*8]
	and	[rdi + rcx*8], rax
	inc	rcx
	jmp	bufand.loop
bufand.done:
	ret

shiftr:					     # shiftr:
	push	rbx			     #   save rbx
	mov	r8, rcx			     #   left_shift = input
	shr	r8, 6			     #   word_shift = left_shift / 64
	cmp	r8, rdx			     #   if (word_shift > in size)
	ja	shiftr.done		     #   then skip shifting entirely
	cmp	r8, 0			     #   if word_shift is 0?
	jz	shiftr.word_offset	     #   then
	lea	rsi, [rsi + r8*8]	     #   inbuf += word_shift
	sub	rdx, r8			     #   insize -= word_shift
shiftr.word_offset:			     #
	and	rcx, 63			     #   bit_shift = word_shift % 64
	test	rcx, rcx		     #   if (bit_shift == 0)
	jz	shiftr.memcpy		     #   then just memcpy
	mov	rbx, 64			     #   inverse_bitshift = 64
	sub	rbx, rcx		     #   inverse_bitshift -= bit_shift
	xor	r8, r8			     #   i = 0
shiftr.loop:				     #
	cmp	r8, rdx			     #   if i >= insz
	jge	shiftr.done		     #   then goto done
	mov	r9, [rsi + r8*8]	     #   r9 = in[i]
	shr	r9, rcx			     #   r9 >>= bit_shift
	mov	r10, r8			     #   previ = i
	inc	r8			     #   i++
	cmp	r8, rdx			     #   if (i >= insize)
	jge	shiftr.after_borrow	     #   then goto after borrow
	mov	r11, [rsi + r8*8]	     #   r11 = in[i] (nexti)
	shlx	r11, r11, rbx		     #   r11 <<= inverse_bitshift
	or	r9, r11			     #   or two pieces together
shiftr.after_borrow:
	mov	[rdi + r10*8], r9	     #   outbuf[previ] = r9
	jmp	shiftr.loop
shiftr.memcpy:
	mov	rcx, rdx		     #   repsize = insizen
	rep	movsq			     #   memcpy (rdi and rsi prev set)
shiftr.done:
	pop	rbx
	ret

reorder:
	mov	rax, [rbp - 24]
	mov	rdx, [rbp - 64]
	cmp	rax, rdx
	jna	reorder.ret
	swapq	rax, rdx, [rbp -  8], [rbp - 48]
	swapq	rax, rdx, [rbp - 24], [rbp - 64]
	swapq	rax, rdx, [rbp - 32], [rbp - 72]
	swapq	rax, rdx, [rbp - 40], [rbp - 80]
reorder.ret:
	ret

slowor:
	push	rbp
	mov	rbp,rsp
	add	rsp, -88
	call	unpack
	call	reorder
	mov	rdi, [rbp - 80]
	xor	rsi, rsi           # might have 64-bit word
	call	reserve
	mov	[rbp - 88], rax
	call	unpack
	call	reorder
	mov	rdi, [rbp - 88]
	mov	rsi, [rbp - 72]
	mov	rcx, [rbp - 80]
	rep	movsq
	mov	rdi, [rbp - 88]
	mov	rsi, [rbp - 32]
	mov	rdx, [rbp - 40]
	call	bufor
	mov	rdi, [rbp - 80]
	call	claim
	pdrop	1
	mov	[r15], rax
	leave
	ret

slowxor:
	push	rbp
	mov	rbp,rsp
	add	rsp, -88
	call	unpack
	call	reorder
	mov	rdi, [rbp - 80]
	call	reserve
	mov	[rbp - 88], rax
	call	unpack
	call	reorder
	mov	rdi, [rbp - 88]
	mov	rsi, [rbp - 72]
	mov	rcx, [rbp - 80]
	rep	movsq
	mov	rdi, [rbp - 88]
	mov	rsi, [rbp - 32]
	mov	rdx, [rbp - 40]
	call	bufxor
	mov	rdi, [rbp - 80]
	call	claim
	pdrop	1
	mov	[r15], rax
	leave
	ret

slowbitwiseand:
	push	rbp
	mov	rbp,rsp
	add	rsp, -88
	call	unpack
	call	reorder
	mov	rdi, [rbp - 40]
	call	reserve
	mov	[rbp - 88], rax
	call	unpack
	call	reorder
	mov	rdi, [rbp - 88]
	mov	rsi, [rbp - 72]
	mov	rcx, [rbp - 40]
	rep	movsq
	mov	rdi, [rbp - 88]
	mov	rsi, [rbp - 32]
	mov	rdx, [rbp - 40]
	call	bufand
	mov	rdi, [rbp - 40]
	call	claim
	pdrop	1
	mov	[r15], rax
	leave
	ret

slowrsh:
	push	rbp
	mov	rbp,rsp
	add	rsp, -96
	call	unpack
	mov	rax, [rbp - 48]
	test	rax, rax
	jz	slowrsh.x
	mov	rax, [rbp - 64]
	mov	rdx, [rbp - 24]
	cmp	rax, rdx
	ja	slowrsh.zero
	mov	rdi, [rbp - 24]
	sub	rdi, [rbp - 64]
	btw	rdi
	mov	[rbp - 88], rdi
	call	reserve
	mov	[rbp - 96], rax
	call	unpack
	mov	rdi, [rbp - 96]
	mov	rsi, [rbp - 32]
	mov	rdx, [rbp - 40]
	mov	rcx, [rbp - 48]
	call	shiftr
	mov	rdi, [rbp - 88]
	call	claim
	pdrop	1
	mov	[r15], rax
	leave
	ret
slowrsh.x:
	pdrop	1
	mov	rax, [rbp - 8]
	mov	[r15], rax
	leave
	ret
slowrsh.zero:
	pdrop	1
	mov	qword ptr [r15], 0
	leave
	ret

### Constructing Apps

# `closure` takes a tag and a size and allocates a closure, but leaves the
# parameter data uninitialized.  The bulk of the logic here is around
# calculating the pointer tag.
#
# Args: rdi=tag, rsi=sz
# Rets: rax=res
# Clob: r8, r9
#
# TODO take NF flag as an additional argument?

closure:
	lea	rax, [r14+8*rsi+16]        # ptr = &heap_next[rsi+2]
	cmp	rax, r13                   # if (ptr > heap_end)
	ja	closure.gc                 #     goto gc
	xchg	rax, r14                   # swap(ptr, heap_next)
	lea	r8, [rsi + 1]		   # nodesz
	shl	r8, 14                     # r8 = bitsz << 8
	or	r8, 4                      # type=4 (whnf clz)
	mov	[rax], r8                  # write RECORD (t=4 sz=64*(args+1))
	add	rax, 8
	mov	r8, 0xD000000000000000
	or	rax, r8
	xor	r9, r9                     # r9 = 0
	mov	r8, rsi                    # r8 = args
	cmp	r8, 15
	cmova	r8, r9                     # r8 = args>15 ? 0 : args
	shl	r8, 48
	or	rax, r8                    # add size to metadata
	mov	r8, rdi                    # r8 = tag
	mov	r9, 15
	cmp	r8, r9
	cmova	r8, r9                     # r8 = min(tag,15)
	shl	r8, 52                     # add tag to metadata
	or	rax, r8
	ret
closure.gc:
	ppush	rdi
	lea	rdi, [rsi+2]               # arg = words needed
	call	gc
	ppop	rdi
	jmp	closure

.global mkclz1
mkclz1: # rax=tag rdi=val ==> rax=(tag[val])
	mov	r10, rax
	mov	r11, rdi
	mov	rdi, rax
	mov	rsi, 1
	call	closure # rax = closure(tag=fn sz=1)
	ptr_	r8, rax
	mov	[r8], r10
	mov	[r8+8], r11
	ret

.global mkclz2
mkclz2: # rax=tag rdi=a rsi=b ==> rax=(tag[a b])
	mov	r10, rax  # r10=f
	mov	r11, rdi  # r11=a
	mov	rcx, rsi  # rcx=b
	mov	rdi, rax  # tag=f
	mov	rsi, 2    # sz=2
	call	closure   # rax = closure(tag=fn sz=1) clobbers r8,r9
	ptr_	r8, rax
	mov	[r8], r10
	mov	[r8+8], r11
	mov	[r8+16], rcx
	ret

# rax=tag rdi=a rsi=b rdx=c ==> rax=(tag[a b c])
mkclz3:
	mov	r10, rax # r10=f
	mov	r11, rdi # r11=arg1
	mov	rcx, rsi # rcx=arg2 (rdx=arg3)
	mov	rdi, rax
	mov	rsi, 3
	call	closure # rax = closure(tag=fn sz=1) {r8 r9}
	ptr_	r8, rax
	mov	[r8], r10
	mov	[r8+8], r11
	mov	[r8+16], rcx
	mov	[r8+24], rdx
	ret

mkclz: # rdi=f, rsi=sz sp[0..(sz-1)]=params -> result/rax
	ppush	rdi
	mov	rdi, rsi
	jmp	mkclosure # TODO: specialize this

.global mkclosure
mkclosure: # rdi=sz sp[0]=f sp[1..sz]=params -> result/rax
	mov	rcx, rdi                      # x=params (avoiding clobber)
	mov	rdi, [r15]                    #
	mov	rsi, rcx                      #
	call	closure                       # res = closure(*sp, params)
	ptr_	rdi, rax                      # dst = PTR(res)
	inc	rcx                           # cnt = params+1
	mov	r8, rcx                       # copy params to r8
	cmp	r8, 3			      # if (cnt > 3)
	ja	mkclosure.large		      # then fallback
	mov	rcx, [r15]		      # copy [0] (always one item)
	mov	[rdi], rcx		      # place [0]
	cmp	r8, 1			      # if (cnt == 1)
	je	mkclosure.done		      # then done
	mov	rcx, [r15+8]		      # copy [1]
	mov	[rdi+8], rcx		      # place [1]
	cmp	r8, 2			      # if (cnt == 2)
	je	mkclosure.done		      # then done
	mov	rcx, [r15+16]		      # copy [2]
	mov	[rdi+16], rcx		      # place [2]
	jmp	mkclosure.done
mkclosure.large:
	mov	rsi, r15                      # src = stack
	rep	movsq                         # memcpy()
mkclosure.done:
	lea	r15, [r15+8*r8]               # sp = &sp[params+1]
	ret

copyclosure: # rdi=clz -> rax=clz {*}
	clzsz_	rcx, rdi       # n/rcx = clz.args
	ppush	rdi, rcx
	dref	rdi            # arg1 = clz.head
	mov	rsi, rcx       # arg2 = args
	call	closure        # rax = newclz
	ppop	r8, rcx        # r8 = oldclz, cnt=args
	ptr_	rsi, r8        # src = PTR(oldclz)
	ptr_	rdi, rax       # dst = PTR(newclz)
	inc	rcx            # cnt = args + 1
	rep	movsq          # memcpy
	ret

### Constructing Laws

.global MkLaw
oplaw:
MkLaw: # rdi=n rsi=a rdx=b -> rax={n a b}
	ENAT	rdi, rsi, rdx
	ENAT	rsi, rdx, rdi
	FORCE	rdx, rdi, rsi
	mov	r9, rdi
	mov	r10, rsi
	mov	r11, rdx
mklaw:
	test	r10, r10
	jz	mklaw.arityzero
mklaw.alloc:
	lea	rdx, [r14+8*7]         # resptr = heap_next + 7
	cmp	rdx, r13               # if (new_next > heap_end)
	ja	mklaw.gc               #     goto gc
	xchg	rdx, r14
	lea	rax, [rdx+8]
	mov	r8, 0xC800000000000000 # mask
	or	r8, rax                # result = resptr | mask
	ppush	r8                     # save result
	mov	r8, 0x0000000000018002 # r8 = RECORD(s=6w, t=2(LAW))
	mov	[rdx], r8              # result.gcrecord = r8
	lea	r8, Judge
	mov	[rdx + 32], r8         # operator
	mov	[rdx + 8], r9          # name
	mov	[rdx + 16], r10        # args
	mov	[rdx + 40], r10        # args (for evaluation)
	mov	[rdx + 24], r11        # body
	mov	rdi, r11
	call	weigh
	mov	[rdx + 48], rax        # law-weight
	ppop	rax                    # restore result
	ret                            # return result
mklaw.arityzero:
	ud2                            # TODO: accept zero-arity laws
mklaw.gc:
	ppush	rdi
	mov	rdi, 7                 # arg0 = words needed
	call	gc
	ppop	rdi
	jmp	mklaw.alloc

weigh:
	ppush	rdi
	mov	rcx, 1
	mov	rax, 0
	mov	rdi, 1
weigh.loop:
	test	rcx, rcx
	jz	weigh.done
	mov	r8, [r15]
	jnclzt	r8, weigh.pop # TODO is 0-clz w/ sz=2 can be 1 instr
	mov	r9, r8
	clzsz	r9
	cmp	r9, 2
	jne	weigh.pop
	clzhd_	r9, r8
	cmp	r9, 1
	jne	weigh.pop
	test	rdi, rdi
	jz	weigh.pop
	inc	rax
	inc	rcx
	sub	r15, 8
	ptr_	r9, r8
	mov	r9, [r9+16]
	mov	[r15], r9
	ptr_	r9, r8
	mov	r9, [r9+8]
	mov	[r15 + 8], r9
	jmp	weigh.loop
weigh.pop:
	xor	rdi, rdi
	pdrop	1
	dec	rcx
	jmp	weigh.loop
weigh.done:
	ret

### Constructing Pins

# mkpin : rdi/Item -> rax/Pin {rdi r8 r9 r10 rdx}

pinarity: # rdi -> rax (clobbers rdi, rcx, r12)
	jinat	rdi, arity.primop
	jmp	arity.notnat

.global arity
arity:
################################################################################
### TODO: rearrange things so that they work with arity=0 as infinite arity, ###
### and then use this code for a jet.                                        ###
################################################################################
	jithk	rdi, arity.one
	jinat	rdi, arity.max
arity.notnat:
	_jiclz	rdi, arity.closure
	drefo_	rax, rdi, 32
	ret
arity.primop:
	mov	rax, 4
	cmp	rdi, 3
	cmova	rdi, rax
	mov	rax, [arity.primtab + rdi*8]
	ret
arity.primtab:
	.quad 1, 3, 1, 6, 1
arity.max:
	mov	rax, 0x7fffffffffffffff # TODO: return 0 (change consumer)
	ret
arity.one:
	mov	rax, 1 # if given a lazy input
	ret
arity.closure:
	mov	rcx, rdi
	clzsz	rcx
	ptr	rdi
	mov	rdi, [rdi]
	call	arity
	sub	rax, rcx
	ret

# {jet} calculates which routine should be used to compute the result when a
# pin is used as a function.
#
# Input: rdi/Item
# Output: rax/Operator
# Clobbers: rax, r8, r9, r10, r12

jet:
	cmp	rdi, 21                   #   if item < 21
	jb	jet.primop                #   then goto primop
	jinat	rdi, jet.throw            #   if IsNat(item) goto throw
	_jnlaw	rdi, jet.crash            #   if not IsLaw(item) goto crash
	call	match
	test	r8, r8
	jnz	jet.wrapper
jet.nowrap:
	ptr_	rax, rdi                  #   rax/p = PTR(item)
	mov	rdi, [rax]                #   lawName  = p[0]
	mov	rsi, [rax+8]              #   lawArity = p[1]
	call	findop                    #   fp = findop(lawName, lawArity)
	test	rax, rax                  #   if (fp == 0)
	jz	jet.judge                 #     goto nomatch
	cmp	r8, rsi
	jne	jet.judge
	ret                               #   return result
jet.wrapper:                              # wrapper: rdi/law rax/meth rdx/op r8/arity
	cmp	rdx, 15                   #   if (op != <15>)
	jne	jet.nowrap                #     goto nowrap
	mov	r9, r8                    #   r9/arity
	mov	rbx, rdi
	mov	rdi, rax                  #   name = method
	call	findop                    #   proc/rax r8/arity
	mov	rdi, rbx
	test	rax, rax                  #   if (proc == NULL)
	jz	jet.nowrap                #     goto nowrap
	cmp	r8, r9                    #   if (wrapper arity doesn't match)
	jne	jet.nowrap                #     goto nowrap
jet.wrapperok:
	ret                               #   return proc
jet.judge:                                # judge:
	lea	rax, [Judge]              #   result = &Judge
	ret                               #   return result
jet.throw:
	lea	rax, [ThrowOp]
	ret
jet.crash:
	lea	rax, [Crash]
	ret
jet.primop:
	mov	rax, [jet.primtab + rdi*8]
	ret
jet.primtab:
	.quad MkPin, MkLaw, opinc, Eat, TraceOp, SyscallOp, PeekOp, PokeOp
	.quad TryOp, WriteOp, ReadOp, Precommit, Commit, xptr, xbuf, prim
	.quad SendOp, ThrowOp, RequestGC2Op
	.quad SetProfEnabledOp, GetProfOp

# findop() find an operation given a name an and arity.
#
# ARG rdi: name
# RET rax: proc (or NULL)
# RET r8: op arity
#
# Clobbers  rax, r8

findop:                                     # findop:
	lea	rax, [findop.table]         #   iter = &finop.table
findop.loop:                                # loop:
	mov	r8, [rax+16]                #   name = iter.name
	test	r8, r8                      #   if (name == 0)
	jz	findop.nomatch              #   then goto nomatch
	cmp	r8, rdi                     #   if (name == lawName)
	je	findop.found                #   then goto found
	add	rax, 24                     #   iter = iter++
	jmp	findop.loop                 #   goto loop
findop.found:                               # found:
	mov	r8, [rax+8]                 #   arity = iter.arity
	mov	rax, [rax]                  #   result = iter.fp
	ret                                 #   return result
findop.nomatch:                             # nomatch:
	xor	eax, eax                    #   result = 0
	ret                                 #   return
findop.table:
	.quad opinc,  1, 6516297         # "Inc"
	.quad opdec,  1, 6513988         # "Dec"
	.quad opadd,  2, 6579265         # "Add"
	.quad opsub,  2, 6452563         # "Sub"
	.quad opmul,  2, 7107917         # "Mul"
	.quad opdiv,  2, 7760196         # "Div"
	.quad opmod,  2, 6582093         # "Mod"
	.quad optrunc,2, 427054953044    # "Trunc"
	.quad oprsh,  2, 6845266         # "Rsh"
	.quad oplsh,  2, 6845260         # "Lsh"
	.quad opand,  2, 6581825         # "And"
	.quad opor,   2, 29263           # "Or"
	.quad opbytes, 1, 495623371074	 # "Bytes"
	.quad opload, 3, 1684107084      # "Load"
	.quad opdivmod,2, 110429203753284  # DivMod
	.quad opload8,2, 132256630077762   # ByteIx
	.quad opload8,2, 7891266           # Bix
	.quad opload8,2, 242202275660      # Load8
	.quad Cmp,    2, 7368003           # Cmp (Generic)
	.quad opseq,  2, 7431507           # Seq
	.quad opispin,1, 474213282633      # IsPin
	.quad opislaw,1, 512733508425      # IsLaw
	.quad opisapp,1, 482919674697      # IsApp
	.quad opisnat,1, 499848737609      # IsNat
	.quad opnat,  1, 7627086           # Nat
	.quad opnil,  1, 8024389           # Eqz
	.quad opnil,  1, 7104846           # Nil
	.quad optype, 1, 1701869908	   # Type
	.quad opunpin, 1, 474215378517	   # Unpin
	.quad opname, 1, 1701667150	   # Name
	.quad oparity, 1, 521644110401	   # Arity
	.quad opbody, 1, 2036625218	   # Body
	.quad opinit, 1, 1953066569	   # Init
	.quad oplast, 1, 1953718604	   # Last
	.quad opsz,   1, 31315             # Sz
	.quad opbits, 1, 525386148162      # BitSz
	.quad opbits, 1, 1937008962        # Bits
	.quad optest, 2, 1953719636	   # Test
	.quad opset,  2, 7628115	   # Set
	.quad opclear, 2, 491260308547	   # Clear
	.quad opbex,  1, 7890242           # Bex
	.quad opix,   2, 30793             # Ix
	.quad opup,   3, 28757             # "Up"
	.quad opif,   3, 26185             # If
	.quad opifz,  3, 8021577           # Ifz
	.quad opcase, 3, 29250             # Br
	.quad opcase, 3, 1702060355        # Case
	.quad opcoup, 2, 1886744387	   # Coup
	.quad oprep,  3, 7365970	   # Rep
	.quad ophd,   1, 25672             # "Hd"
	.quad oprow,  3, 7827282	   # "Row"
	.quad opcopy, 5, 2037411651	   # Copy
	.quad opcmp,  2, 123614585446734   # NatCmp
	.quad opcmp,  2, 28554735471390531 # Compare
	.quad opeq,   2, 119233752359246   # NatEql
	.quad opeq,   2, 28997             # Eq
	.quad opne,   2, 25934		   # Ne
	.quad opgt,   2, 29767             # Gt
	.quad opge,   2, 25927             # Ge
	.quad oplt,   2, 29772             # Lt
	.quad ople,   2, 25932             # Le
	## .quad opdis,  2, 7563588	   # Dis
	## .quad opcon,  2, 7237443	   # Con
	## .quad opmix,  2, 7891277	   # Mix
	.quad opcase3,   4, 220745392451      # Case3
	.quad opstore,   4, 435711603795      # Store
	.quad opstore8,  3, 62008362759251    # Store8
	.quad opstore64, 3, 14696508128457811 # Store64
	.quad opprofile, 2, 28548172593721936 # Profile
	.quad 0

# TODO: these need to be added to findop.table
#
#     opcase0 opcase1 opcase2
#     opcase4 opcase5 opcase6 opcase7 opclear opcut
#     opeat opedit operase opforce opfresh
#     opix0 opix1 opix2 opix3
#     opix4 opix5 opix6 opix7 opixN oplaw opload16 opload32
#     opload64 opnand opnor
#     oppin oppow opsap2 opsap3 opsap4 opsap5 opsap6 opsap7 opsap8
#     opsplice opstore16 opstore32
#     opswi optobit optrunc1 optrunc128 optrunc16
#     optrunc256 optrunc32 optrunc64 optrunc8 opupuniq opwipe

opdis:
	ud2
opcon:
	ud2
opmix:
	ud2

### Constructing Nats

# rawreserve() reserves some memory at the end of the heap, which
# the caller can then populate with an actual heap object.
#
# This does not actually move the heap pointer forward, it just guarentees
# that the memory is available.  The caller is responsible for actually
# moving the heap pointer forward when the object is done, which makes it
# possible to simply abandon the allocation without use any memory (for
# example, if the result of a bignat computation ends up being direct).
#
# Inputs: rdi=size (in words)
# Results: rax=buf (pointer to new object location)
# Clobbers: rax

.global rawreserve
rawreserve:                                 # rawreserve: rdi=size
	lea	rax, [r14+8*rdi+8]          #   rax = &hp[size+1]
	cmp	rax, r13                    #   if (rax < heap_end)
	ja	1f                          #     then
	ret                                 #     return
1:	push	rdi                         #   save size
	inc	rdi                         #   size++
	call	gc                          #   buf = gc(size++)
	pop	rdi                         #   restore size
	ret                                 #   return buf

# reserve() is a convenience wrapper around rawreserve.  It reserves one
# more word than what was asked for, and it zero-initializes the the memory.
#
# Inputs: rdi=size (in words)
# Results: rax=buf (pointer to new object location)
# Clobbers rdi, rcx, rax

reserve:                                    # reserve: rdi/sz
	call	rawreserve                  #   rax/ptr = rawreserve(rdi)
	lea	rcx, [rdi+1]                #   heap_size = size+1
	mov	rdi, r14                    #   dst = heap_next
	xor	eax, eax                    #   wrd = 0
	rep	stosq                       #   memset
	lea	rax, [r14+8]                #   res = &heap_next[1]
	ret                                 #   return

# reservecopy()
#
# Given a bignat, reserves space and then initializes it with a copy.
# The result will have a trailing zero since that's pretty cheap to
# provide and because many operations need an extra word since they might
# overflow into the next word.
#
# Arguments: rdi=bignat
# Clobbers: rdi, rsi, rcx
# Returns: rax=ptr, rdx=nwords

reservecopy:                                # reservecopy: rdi/big
	mov	rsi, rdi                    #   rsi/num
	wordsz	rdi                         #   rdi/num.wordsz
reservecopy.sized:                          # sized: rdi/wordsz rsi/big
	mov	rdx, rdi                    #   rdx=sz
	inc	rdi                         #
	call	rawreserve                  #   rawreserve(MAX(sz+1, minsz))
	xor	eax, eax                    #
	mov	[r14], rax                  #   zero the header
	mov	[r14+8*rdx+8], rax          #   zero the overflow
	mov	rcx, rdx                    #   cnt = sz
	ptr	rsi                         #   src = PTR(nat)
	lea	rdi, [r14+8]                #   dst = &heap_next[1]
	mov	rax, rdi                    #   result = dst
	rep	movsq                       #   memcpy
	ret                                 #   return result, sz

# reservecopymin()
#
# This copies a nat, with a minimum allocation size.  If the minimum
# size is greater than the size of the nat, there will be extra zero
# words in the resulting allocation.
#
# ARG: rdi/big -- number to copy
# ARG: rsi/min -- minimum buffer size
# RET: rax/buf -- pointer to new heap object
# RET: rdx/wid -- size of buf in words
#
# Clobbers: rdi, rsi, rcx, rax, rdx, r12

.global reservecopymin
reservecopymin:                             # reservecopymin: rdi/big rsi/min
	mov	rdx, rsi                    #   rdx/min
	mov	rsi, rdi                    #   rsi/big
	wordsz	rdi                         #   rdi/wordsz(big)
	cmp	rdx, rdi                    #   if (min <= words)
	jle	reservecopy.sized           #     enter reservecopy flow
	mov	r12, rdi                    #   r12/wordsz
	mov	rdi, rdx                    #   rdx = min
	call	reserve                     #   buf = reserve(min)
	ptr	rsi                         #   src = PTR(big)
	mov	rdi, rax                    #   dst = buf
	mov	rcx, r12                    #   cnt = wordsz
	rep	movsq                       #   memcpy
	ret                                 #   return ptr, wid

# reservecopyflex()
#
# This is just like reservecopymin, except that the input is allowed to
# be direct.
#
# ARG: rdi -- num -- a number
# ARG: rsi -- min -- a minimum buffer size, in words
# RET: rax -- buf -- a pointer to a buffer
# RET: rdx -- wid -- the size of the buffer
#
# Clobbers: rdi, rsi, rcx, rax, rdx
#
# Note that, in the direct path, if we are given a minimum size of 0,
# we call reserve(0).  But this is fine because reserve always allocates
# an extra word anyways.

.global reservecopyflex
reservecopyflex:                            # reservecopyflex: rdi/num rsi/min
	jheap	rdi, reservecopymin         #   if indirect reservecopymin
	mov	rdx, rdi                    #   num/rdx
	mov	rdi, rsi                    #   arg1 = min
	call	reserve                     #   buf = reserve(min)
	mov	[rax], rdx                  #   buf[0] = num
	mov	rdx, rsi                     #   wid = min
	mov	rcx, 1                      #
	test	rdx, rdx                    #
	cmovz	rdx, rcx                    #   wid = MIN(1,wid)
	ret                                 #   return buf, wid

# lstinline|writenat| just copies all of the words from a nat into a
# buffer, which must be big enough to hold the result.  The input may
# be either direct, or indirect.
#
# TODO: Currently untested and unused.
#
# Inputs: rsi=ptr, rsi=nat
# Returns: nothing
# Clobbers: rsi, rdi, rcx

writenat:                                   # writenat: rdi=ptr rsi=nat
	jheap	rsi, 1f                     #   if (nat.direct)
	mov	[rdi], rsi                  #     *ptr = nat
	ret                                 #     return
1:	wordsz_	rcx, rsi                    #   cnt = nat.words
	ptr	rsi                         #   src = PTR(nat)
	rep	movsq                       #   memcpy
	ret                                 #   ret

# natwords()
#
# ARG: rdi -- num -- the number ot
# RES: rax -- wid -- the word-width of the nat.
#
# Only clobbers r12.

.global natwords
natwords:                                   # natwords: rdi=nat
	test	rdi, rdi                    #   examine arg
	js	1f                          #   if (!indirect)
	setnz	al                          #     al = (arg!=0)
	movzx	rax, al                     #     rax = al
	ret                                 #     return rax
1:	wordsz_	rax, rdi                    #   res = nat.words
	ret                                 #   return

claim: # rdi/u64 -> rax/Val {rax rdi rsi r8}
	lea	rsi, [r14+8]            # ptr = heap_next+1
	xor	eax, eax                # rax = 0
claim.loop:
	dec	rdi                     # n--
	test	rdi, rdi                # if (n < 0) # (all words are zero)
	js	claim.ret               #     return
	mov	rax, [rsi + rdi*8]      # load msw
	jz	claim.u64               # if (n==0) goto u64 (single word)
	test	rax, rax                # if msw==0
	jz	claim.loop              #     goto loop (shrink result)
claim.indirect:
	inc	rdi                     # restore size
	mov	rsi, rdi
	shl	rsi, 6
	lzcnt	r8, rax                 # r8 = lzcnt(msw)
	sub	rsi, r8                 # rsi = result bitsize
	shl	rsi, 8                  # rsi = RECORD(ty=0, n=bitsz)
	mov	[r14], rsi              # result.header = rsi
	mov	rsi, 0x8200000000000000 # tag mask
	lea	rax, [r14+8]            # offset = hp+8
	lea	r14, [rax+rdi*8]        # claim the reserved heap space
	or	rax, rsi                # result = tagged heap pointer
	ret                             # return result
claim.u64:
	jheap	rax, claim.indirect
claim.ret:
	ret

### Constructing Thunks

# TODO: I have no memory of what mkthunk_clz was supposed to be doing
# and it isn't called from anywhere?

# TODO: handle the head-is-closure AND result-is-closure case.

mkthunk_clz: # rdi=params, rsi=exec
	# TODO: how to compute the arity
	mov	r9, [r15]                    # f = *sp
	jiclzt	r9, mkthunk                  # if CLZ(f) goto mkthunk
	xchg	r9, rdi                      # arg=f, stash params into r9
	call	arity                        # rax=arity(f) {rdi,rax,rcx}
	mov	rdi, r9                      # restore params from r9
	test	rax, rax                     # if (f.arity=0)
	jz	clzcase                      #     goto clzcase
	cmp	rdi, rax                     # if (params < arity)
	jb	clzcase                      #     goto clzcase
	jmp	mkthunk                      # return mkthunk()
clzcase:
	call	mkclosure
	ppush	rax
	ret
	# TODO mkthunk should return via rax, and then
	# this can be a tail call (and the caller can
	# do the stack manip).

mkapp: # stk=[x f ..] => stk[(f x)] {rdi, rsi, rax, r8, rcx}
	swapstk	rdi, rsi
	mov	rdi, 1
	lea	rsi, xunknown
	jmp	mkthunk

mkthunk:
	lea	rax, [r14+rdi*8+24]         # ptr = &heap_next[params+3]
	cmp	rax, r13                    # if (ptr > heap_end)
	ja	mkthunk.gc                  #   goto gc
	xchg	rax, r14                    # swap(ptr, heap_next)
	lea	r8, [rdi+2]                 # gc_size/r8 = params+2
	shl	r8, 14                      # ( convert size to bits and
	add	r8, 5                       #   shift it + set tag to 5 )
	mov	[rax], r8                   # write RECORD (t=5, sz=64*(args+2))
	add	rax, 8                      # thunk_ptr = &gc_ptr[1]
	mov	r8, 0xE000000000000000
	or	rax, r8                     # res = thunk_ptr | ptr_tag
	ptr_	r8, rax
	mov	[r8], rsi                   # *PTR(res) = executioner
	mov	rsi, r15                    # src = stack_pointer
	lea	rcx, [rdi+1]                # cnt = params+1
	lea	r15, [r15+rdi*8]            # sp += params
	lea	rdi, [r8+8]                 # dest = &(PTR(res)[1])
	rep	movsq                       # memcpy
	mov	[r15], rax                  # *sp = res
	ret                                 # return
mkthunk.gc:
	ppush	rdi
	lea	rdi, [rdi+3]                # needed size = params+3
	call	gc
	ppop	rdi
	jmp	mkthunk

# apple()
# =======
#
# Creates a thunk by applying a function to a series of arguments
# (passed via the stack).
#
# Input: rdi = function
# Input: rsi = nparams
# Input: rdx = cached (thunk update)
# Input: sp[0] .. sp[nparams-1]: closure params
#
# Output: rax=result
#
# Effect: closure params are consumed off the stack.
#
# Clobbers: rdi, rsi, rcx, r8, rax

apple:
	mov	rax, r14		    # rax = old heap_next
	lea	rcx, [r14 + rsi*8 + 24]	    # new_next = heap_next+(params+3)*8
	cmp	rcx, r13		    # if (new_next > heap_end)
	ja	apple.gc                    #   goto gc
	mov	r14, rcx		    # commit new_next
	mov	r8, rsi                     # r8 = nparams
	mov	[rax+16], rdi               # node[1] = function
	lea	rdi, [r8+2]                 # gc_size/rdi = params+2
	shl	rdi, 14                     # ( convert size to bits and
	add	rdi, 5                      #   shift it + set tag to 5 )
	mov	[rax], rdi                  # write RECORD (t=5, sz=64*(args+2))
	lea	rdi, [xunknown]             # exe = &xnknown
	lea	rsi, [xunknownnoupdate]
	test	rdx, rdx                    # if (!cached)
	cmovz	rdi, rsi                    #     exe = &xunknownnoupdate
	mov	[rax+8], rdi                # node[1] = exe
	mov	rsi, r15                    # src = &sp[0]
	lea	rdi, [rax+24]               # dst = &node[3]
	cmp	r8, 3			    # if (nparams > 3)
	ja	apple.over_3		    # then fallback rep movsq case
	test	r8, r8			    # if (nparams == 0)
	jz	apple.copied		    # then done
	mov	r9, [rsi]		    # copy [0]
	mov	[rdi], r9		    # place [0]
	cmp	r8, 1			    # if (nparams == 1)
	je	apple.copied		    # then we're done
	mov	r9, [rsi+8]		    # copy [1]
	mov	[rdi+8], r9		    # place [1]
	cmp	r8, 2			    # if (nparams == 2)
	je	apple.copied		    # then we're done
	mov	r9, [rsi+16]		    # copy [2]
	mov	[rdi+16], r9		    # place [2]
	jmp	apple.copied		    # we're done either way.
apple.over_3:				    # rep movsq fallback:
	mov	rcx, r8                     # cnt = params
	rep	movsq                       # memcpy
apple.copied:
	lea	r15, [r15 + r8*8]
	mov	r8, 0xE000000000000000      # mask
	add	rax, 8                      # thunk = &gc_ptr[1]
	or	rax, r8                     # res = thunk | mask
	ret                                 # return res
apple.gc:
	ppush	rdi
	lea	rdi, [rsi+3]                # needed size = nparams+3
	call	gc
	ppop	rdi
	jmp	apple

### Evaluation

c_eval:
	mov	rax, rdi
	ERAX
	ret
	# No tail call here just to keep the stack frame.  This is
	# not recursive, so it has almost no impact.

opforce:
	mov	rax, rdi
force.newabi:
	ppush	rax
	call	force
	ppop	rax
	ret

# TODO: once we switch to the new ABI, we can do a single `cmp` to test:
# "is thunk or closure", and if it isn't, we can skip over this whole call.
.global force
force:
	mov	rdi, [r15]
	jnthk	rdi, force.notthk
force.thunk:
	EVAL	rdi
	mov	[r15], rdi
force.notthk:
	jnclzt	rdi, force.exit
force.closure:
	ptr_	rsi, rdi
	mov	rsi, [rsi-8]          # read RECORD
	test	sil, 4
	jz	force.exit            # RECORD bit 2 clear ==> normalized
force.doit:
	shr	rsi, 14               # get size (from RECORD)
	dec	rsi
	xor	rcx, rcx
force.loop:
	cmp	rcx, rsi
	jae	force.break
	mov	r8, [r15]
	ptr	r8
	mov	rdi, [r8 + rcx*8 + 8]
	sub	r15, 8
	mov	[r15], rdi
	push	rsi
	push	rcx
	call	force
force.after:
	pop	rcx
	pop	rsi
	mov	rdi, [r15 + 8]
	ptr_	r8, rdi
	mov	r9, [r15]
	mov	[r8 + rcx*8 + 8], r9
	pdrop	1
	inc	rcx
	jmp	force.loop
force.break:
	ptr_	rsi, [r15]
	mov	byte ptr [rsi-8], 3          # Set RECORD type to 3 (nf clz)
force.exit:
	ret

# split takes apart an APP, returning the head and the tail. This happens
# whenever <3> is used with an app.
#
# -   Input: rdi (Must always be an APP nodes)
# -   Output: rax = (Init node)
# -   Output: rdx = (Last node)
# -   Clobbers: rdi, rsi, rcx, r9, r10
#
# Materializing the head of a large APP requires a large application,
# and often times the consumer does not actually need that value, so
# we produce a thunk instead.
#
# This uses a special thunk xhead, which doesn't corespond to any
# PLAN function, but which just materializes the `Init` of the given app.
#
# For example, if the input is [1..99] we produce (xhead [1..99], 5).
#
# However, in the special case of a pair, like 3[4], we don't use
# xhead, since the head can be produced without any allocation at all.
#
# Technically, in the case of a sz=2 input like 3[4 5], the thunk that
# we allocate is the same size as the actually-materialized closure 3[4],
# so this only adds overhead.  This is not worth over-optimizing,
# however, as we generally use jets for this and not ##3 directly.
#
# This optimization is here just to avoid the perverse behavior of
# certain patterns of use, not to make this blazing fast.
#
# TODO: the use of mkthunk here is a bit awkward.  Couldn't we instead
# have something like mkthunk1 which works in terms of registers instead?

split:                                      # node/rdi
	clzsz_	rcx, rdi                    # sz/rcx   = Sz(rdi)
	ptr_	r9, rdi                     # p/r9     = PTR(node)
	mov	rdx, [r9 + rcx*8]           # last/rdx = r9[rcx]
	cmp	rcx, 1                      # if (last != 1)
	jne	split.big                   #     goto split.big
	mov	rax, [r9]                   # init/rax = r9[0]
	ret                                 # return init, last
split.big:
	ppush	rdi, rdx                    # save node, last
	lea	rsi, xhead                  # rsi/exec   = xhead
	xor	edi, edi                    # rdi/params = 0
	call	mkthunk                     # replaces top of stack
	ppop	rax, rdx                    # restore init, last
	ret                                 # return init, last

### REPL

repl:
	xor	rax, rax
	xor	rcx, rcx
	xor	rdx, rdx
	xor	rsi, rsi
	xor	r8, r8
	xor	r9, r9
	xor	r10, r10
	xor	r11, r11
	ppop	rax
	ppush	rdi
	call	force
	ppop	rdi
	call	apply1
	ncast	rax
	mov	rdi, rax
	jmp	syscall_exit_group

### Basic Nat Operations

################################################################################

### If

opif: # rax=If rdi=cond rsi=then rdx=else
	EVAL	rdi, rsi, rdx
	mov	rax, rsi
	test	rdi, rdi
	cmovz	rax, rdx
	JRAX

opifz: # rax=Ifz rdi=cond rsi=then rdx=else
	EVAL	rdi, rsi, rdx
	mov	rax, rsi
	test	rdi, rdi
	cmovnz	rax, rdx
	JRAX

### Division and Modulo

opdivmod:
	ENAT	rdi, rsi
	ENAT	rsi, rdi
	call	_divmod    # rax=(a/b) rdx=(a%b)
	mov	rdi, rax   # x=(a/b)
	mov	rsi, rdx   # y=(a%b)
	xor	eax, eax   # f=0
	jmp	mkclz2     # return f[x y]

_divmod: # rdi=a rsi=b -> rax=(a/b) rdx=(a%b)
	ppush	rdi, rsi
	call	fastdiv
	mov	rsi, [r15+8]
	mov	[r15+8], rax
	mov	rdi, rax
	call	fastmul
	ppop	rdi
	mov	rsi, rax
	call	fastsub
	mov	rdx, rax
	ppop	rax
	ret

opmod:
	ENAT	rdi, rsi
	ENAT	rsi, rdi
fastmod: # rdi=a rsi=b -> rax=(a%b)
	# TODO: fast path!
	call	_divmod
	mov	rax, rdx
	ret

### Boolean Operations

.macro direct2 l1
	mov	rdi, [r15 + 8]
	mov	rsi, [r15]
	mov	rcx, rdi
	or	rcx, rsi
	test	rcx, rcx
	mov	rcx, 0
	js	\l1
.endm

opand:
	EVAL	rdi, rsi
	test	rdi, rdi
	jnz	1f
	xor	eax, eax
	ret
1:	mov	rax, rsi
	JRAX

opor:                                       # opor: rdi=x rsi=y
	EVAL	rdi, rsi                    #   eval x
	test	rdi, rdi                    #   if (x != 0)
	jz	1f                          #     then
	mov	rax, rdi                    #     result = x
	ret                                 #     return
1:	mov	rax, rsi                    #   result = y
	JRAX                                #   return eval(y)

bitwiseand:
	direct2	slowbitwiseand
	and	rdi, rsi
	pdrop	1
	mov	[r15], rdi
	ret

_or:
	mov	rdi, [r15]
	mov	rsi, [r15+8]
	mov	rax, rdi
	or	rax, rsi
	js	slowor
	pdrop	1
	mov	[r15], rax
	ret

Xor:
	ud2 # TODO
	# TODO: What *exactly* are the semantics of this?
_xor:
	direct2	slowxor
	xor	rdi, rsi
	pdrop	1
	mov	[r15], rdi
	ret

### Shifts

c_MkLaw: # rdi=n rsi=a rdx=b -> rax={n a b}
	xor	rax, rax
	xor	rcx, rcx
	xor	r8, r8
	xor	r9, r9
	xor	r10, r10
	xor	r11, r11
	call	MkLaw
	ret

c_or:
	xor	rax, rax
	xor	rcx, rcx
	xor	rdx, rdx
	xor	rsi, rsi
	xor	rdi, rdi
	xor	r8, r8
	xor	r9, r9
	xor	r10, r10
	xor	r11, r11
	jmp	_or

c_xor:
	xor	rax, rax
	xor	rcx, rcx
	xor	rdx, rdx
	xor	rsi, rsi
	xor	rdi, rdi
	xor	r8, r8
	xor	r9, r9
	xor	r10, r10
	xor	r11, r11
	jmp	_xor

c_and:
	xor	rax, rax
	xor	rcx, rcx
	xor	rdx, rdx
	xor	rsi, rsi
	xor	rdi, rdi
	xor	r8, r8
	xor	r9, r9
	xor	r10, r10
	xor	r11, r11
	jmp	bitwiseand

oprsh:
	ENAT	rdi, rsi
	ENAT	rsi, rdi
	mov	rax, rdi
	or	rax, rsi
	js	oprsh.slow
	cmp	rsi, 63
	ja	ret0
	mov	rax, rdi
	mov	rcx, rsi
	shr	rax, cl
	ret
oprsh.slow:
	ppush	rsi, rdi
	call	slowrsh
	ppop	rax
	ret

.global fastrsh
fastrsh:
	direct2	slowrsh
	mov	rcx, rsi
	shr	rdi, rcx
	pdrop	1
	mov	[r15], rdi
	ret

### Truncation

# _trunc.lsw :: rdi=width rsi=BigNat -> rax=word (where rdi<64)
#
# _trunc.word :: rdi=width rsi=word -> rax=word {r8 r9}
#
# TODO: I *think* `w` is evaluated first, but it's complex. Double check!
# Here's the PLAN code:
#
#     (Trunc w n)=(Mod n Bex-w)

optrunc8:                                   # optrunc8: rdi=nat
	ENAT	rdi                         #   eval+cast
	jdirect	rdi, 1f                     #   if indirect
	dref	rdi                         #     rdi = rdi.lsw()
1:	movzx	rax, dil                    #   res = (u8) res
	ret                                 #   return res

optrunc16:                                  # optrunc16: rdi=nat
	ENAT	rdi                         #   eval+cast
	jdirect	rdi, 1f                     #   if (indirect)
	dref	rdi                         #       rdi = rdi.lsw()
1:	movzx	eax, di                     #   res = (u16) res
	ret                                 #   return res

optrunc32: # rdi=nat                        # optrunc32 # rdi=thunk
	ENAT	rdi                         #   eval+cast
	jdirect	rdi, 1f                     #   if (indirect)
	dref	rdi                         #      rdi = rdi.lsw()
1:	mov	eax, edi                    #   res = (u32) rdi
	ret                                 #   return res

optrunc64: # rdi=nat                        # optrunc32 # rdi=thunk
	ENAT	rdi                         #   eval+cast
	mov	rax, rdi
	jheap	rax, 1f                     #   if (indirect)
	ret
1:	dref	rax
	jmp	mkword

optrunc128:
	ud2 # TODO
optrunc256:
	ud2 # TODO

optrunc: # rax=Trunc rdi=width rsi=nat
	ENAT	rdi, rsi
	ENAT	rsi, rdi
slowtrunc: # rdi=width, rsi=nat
	jdirect	rsi, slowtrunc.word
	cmp	rdi, 64
	jb	slowtrunc.lsw
slowtrunc.big:
	mov	r11, rdi           # r11 = w
	mov	rax, rsi           # result = nat
	bitsiz_	r8, rsi            # r8 = bitsize(nat)
	cmp	rdi, r8            # if (w >= bitsz)
	jae	slowtrunc.ret         #     return nat
	mov	rdi, rsi
	call	reservecopy        # rax,rdx = reservecopy(nat)
	mov	rcx, r11
	add	rcx, 63
	shr	rcx, 6             # wordsz = (bitsz+63) / 64
	test	r11, 63            # if width is not a multiple of 64,
	jnz	slowtrunc.msw         #     then we need to truncate the MSW.
	mov	rdi, rcx
	jmp	claim              # return claim(wordsz)
slowtrunc.msw:
	push	rcx
	mov	rdi, r11
	and	rdi, 63            # arg1 = w&63
	lea	r10, [rax+rcx*8-8]
	mov	rsi, [r10]         # arg2 = result.msw
	call	slowtrunc.word        # rax = slowtrunc.word(w&63, msw)
	mov	[r10], rax         # result.msw = rax
	pop	rdi                # restore wordsz
	jmp	claim              # return claim(wordsz)
slowtrunc.lsw:
	dref	rsi                # rsi = nat.lsw
slowtrunc.word:
	test	rdi, rdi
	jz	slowtrunc.zero
	mov	rax, rsi           # result = word
	mov	r8, 64
	lzcnt	r9, rsi
	sub	r8, r9             # r8 = bitsz(word)
	cmp	rdi, r8
	jae	slowtrunc.ret         # no change
	mov	rcx, 64
	sub	rcx, rdi
	shl	rax, rcx
	shr	rax, rcx
slowtrunc.ret:
	ret
slowtrunc.zero:
	xor	rax, rax           # return 0
	ret

### Binary Exponentiation

opbex:
Bex:  # rdi=o -> 2^NAT(o)
	ENAT	rdi
_bex: # rdi=x -> 2^x
	cmp     rdi, 63
	jae     _bex.indirect
	xor	eax, eax
	bts	rax, rdi
	ret
_bex.indirect:
	push	rdi
	shr	rdi, 6
	inc	rdi
	push	rdi
	call	reserve
	pop	rsi
	pop	rcx
	and	rcx, 63
	xor	r8, r8
	bts	r8, rcx
	mov	[rax + rsi*8 - 8], r8
	mov	rdi, rsi
	jmp	claim


### Bit Width

opbits:                                     # opbits:
	ENAT	rdi                         #   eval+cast
	# fallthrough to fastbitsz
.global fastbitsz
fastbitsz:                                  # fastbitsz: (rdi=nat)
	mov	rax, rdi                    #   tmp = nat
	test	rax, rax                    #   if (tmp is direct)
	jns	directbits                  #     then goto direct
	bitsiz	rax                         #   rax = rax.bitsz
	ret                                 #   return
directbits:                                 # directbits: (rdi=word)
	dbitsz_	rax, rdi                    #   rax = 64 - lzcnt(word)
	ret                                 #   return

### Byte Width

# fastbitsz returns in rax and clobbers r8

.global fastbytsz
opbytes:
Bytes:
	ENAT	rdi                         #   eval+cast
fastbytsz:
	call	fastbitsz
	add	rax, 7
	shr	rax, 3
	ret

### Closure Operations

# Since there are no empty closures, ix0 does not need to do bounds
# checking.  Simply checking that the input is a closure is sufficient.

ret0:
	xor	eax, eax
	ret

opix0:                                      # opix0:
	EVAL	rdi                         #   evaluate rdi
	jnclz	rdi, ret0                   #   if (!clz(rdi))
	drefo_	rax, rdi, 8                 #   res = rdi.ix(0)
	JRAX                                #   return eval(res)
opix1:
	mov	rsi, 1
	jmp	opixN
opix2:
	mov	rsi, 2
	jmp	opixN
opix3:
	mov	rsi, 3
	jmp	opixN
opix4:
	mov	rsi, 4
	jmp	opixN
opix5:
	mov	rsi, 5
	jmp	opixN
opix6:
	mov	rsi, 6
	jmp	opixN
opix7:
	mov	rsi, 7
opixN:
	xchg	rdi, rsi
opix:                                       # opix: (rdi=i rsi=clz)
	xor	edx, edx                    #   fallback = 0
opcase:                                     # opcase: (rdi=i rsi=clz rdx=fb)
	ENAT	rdi, rsi, rdx               #   eval i
	EVAL	rsi, rdi, rdx               #   eval clz
fastcase: # rdi=i:Nat rsi=o:Val rdx=f:Thunk
	jnclzt	rsi, fastcase.fall          # if (o not closure) return fallback
	clzsz_	rcx, rsi                    # rcx=sz
	cmp	rdi, rcx                    # if (i >= sz) return f
	jae	fastcase.fall
	ptr	rsi                         # o=PTR(o)
	mov	rax, [rsi+rdi*8+8]          # return o[i+1]
	JRAX
fastcase.fall:
	mov	rax, rdx # rax=f
	JRAX

opcase3:                                    # opcase3: rdi=i rsi=a rdx=b rcx=f
	ENAT	rdi, rsi, rdx, rcx          #   eval i
fastcase3:                                  # fastcase3:
	cmp	rdi, 1                      #   1
	mov	rax, rcx                    #   if (i>1) res = f
	cmovb	rax, rsi                    #   if (i<1) res = a
	cmove	rax, rdx                    #   if (i=1) res = b
	JRAX                                #   return eval(res)

opcase4: # opcase4: rdi=i rsi=a rdx=b rcx=c r8=f
	ENAT	rdi, rsi, rdx, rcx, r8      #   eval i
	cmp	rdi, 3                      #   if (i < 3)
	jb	fastcase3                   #     goto fastcase3
	mov	rax, r8                     #   res = f
	JRAX                                #   return eval(res)

opcase5: # opcase5: rdi=i rsi=a rdx=b rcx=c r8=d r9=f
	ENAT	rdi, rsi, rdx, rcx, r8, r9  #   eval i
	cmp	rdi, 3                      #   if (i < 3)
	jb	fastcase3                   #     goto fastcase3
	mov	rax, r8                     #   res = d
	cmova	rax, r9                     #   if (i > 3) res=f
	JRAX                                #   return eval(res)

# This is actually a bit complicated because replacing the head can
# cause evaluation or closure concatenation.
#
# -   Target is not a closure?
#
#     -   Return the head.
#
# -   Head is nat?
#
#     -   Replace the head.
#
# -   Head is a closure?
#
#     -   Push everything and apply.
#
# -   Head arity is bigger than target size?
#
#     -   Replace the head.
#
# -   Otherwise push everything and apply.

opcoup:                                # opcoup (rdi=hd rsi=clz)
	EVAL	rsi, rdi               #   eval clz
	EVAL	rdi, rsi               #   eval hd
	jnclz	rsi, opcoup.empty      #   if (!clz.isapp) empty
	jinat	rdi, opcoup.simple     #   if (hd.isnat) simple
	clzsz_	r8, rsi                #   r8 = clz.sz
	jiclz	rdi, opcoup.call       #   if (hd.isapp) call
opcoup.func:                           # func: (rdi=hd rsi=clz)
	ppush	rdi, rsi, r8           #   save hd, clz
	call	arity                  #   rax = arity(hd)
	ppop	rdi, rsi, r8           #   restore hd, clz
	cmp	r8, rax                #   if (size < arity)
	jb	opcoup.simple          #     goto simple
opcoup.call:                           # call: (rdi=hd rsi=clz r8=sz)
	mov	r9, rdi                #   r9/hd
	lea	rcx, [r8*8]            #   bytes = sz*8
	sub	r15, rcx               #   grow stack
	refo	rsi, 8                 #   src = &clz.ix(0)
	mov	rcx, r8                #   count=sz
	mov	rdi, r15               #   dest = stack
	rep	movsq                  #   memcpy
	ppush	r9                     #   push hd
	mov	rdi, r8                #   args=sz
	jmp	apply                  #   return apply(sz)
opcoup.simple:                         # simple: (rdi=hd rsi=clz)
	ppush	rdi                    #   save hd
	mov	rdi, rsi               #
	call	copyclosure            #   rax = copyclosure(row)
	ptr_	rdi, rax               #
	ppop	r8                     #   restore hd
	mov	[rdi], r8              #   result[0] = hd
	ret                            #   ret
opcoup.empty:                          # empty: (rdi=hd rsi=clz)
	mov	rax, rdi               #   result = hd
	ret                            #   return

# *** The =Up= Primop
#
# The code is: `(Up i v r)=(NatCase r (opp v r) (Sub Sz-r i))`
#
# The index is evaluated first because the second argument of Sub is
# evaluated first.  Then we evaluate the closure itself.  We never
# evaluate the value.

opup:                                       # opup: rdi=key rsi=val rdx=row
	ENAT	rdi, rsi, rdx               #   evaluate key
	EVAL	rdx, rsi, rdi               #   evaluate closure
fastup:                                     # fastup:
	mov	rax, rdx                    #   rax/row
	jnclz	rdx, fastup.ret             #   if (!closure.isapp) return
	clzsz	rdx                         #   rdx=sz
	cmp	rdi, rdx                    #   if (ix >= sz)
	jae	fastup.ret                  #     return
	ppush	rdi, rsi                    #   save key, val
	mov	rdi, rax                    #   arg1 = row
	call	copyclosure                 #   result = copyclosure(row)
	ppop	rdi, rsi                    #   restore key, val
	ptr_	r8, rax                     #   p = PTR(result)
	mov	[r8+rdi*8+8], rsi           #   p[i+1] = val
fastup.ret:                                 # ret:
	ret                                 #   return result

# *** The =UpUniq= Primop
#
# This has the same behavior as the \lstinline|Up| primop except that
# it the closure reference is unique (no other code-path will every use
# this closure again), and takes advantage of that to mutate the closure
# in-place.
#
# This primop cannot be jetted because the it is impure if the invariant
# is violated, but it can be used from XPLAN and it can be used from within
# the Blitz jet, which is capable of enforcing the pre-condition.

opupuniq:                                   # opup: rdi=key rsi=val rdx=row
	ENAT	rdi, rsi, rdx               #   evaluate key
	EVAL	rdx, rsi, rdi               #   evaluate closure
fastupuniq:                                 # fastupuniq:
	mov	rax, rdx                    #   rax/row
	clzsz	rdx                         #   rdx=sz
	cmp	rdi, rdx                    #   if (ix >= sz)
	jae	fastupuniq.ret              #     return
	ptr_	r8, rax                     #   p = PTR(result)
	mov	[r8+rdi*8+8], rsi           #   p[i+1] = val
fastupuniq.ret:                             # ret:
	ret                                 #   return result


### Closure Copy ###############################################################

# ; n = cnt = count
# ; i = sof = source offset
# ; o = dof = destination offset
# ; s = src = source array
# ; d = dst = destination array
# = (Copy n i o s d)
# | Ifz n d
# | Seq d
# | Copy Dec-n Inc-i Inc-o s (Up o (Ix i s) d)

# opcopy():
#
# The aliased version of Copy just duplicates the input first in order
# to obtain a new unique closure, and then defers to the mutating variant.

# opcopy: rdi=cnt rsi=sof rdx=dof rcx=src r8=dst
opcopy:                                     # opcopy: rdi=key rsi=val rdx=row
	ENAT	rdi, rsi, rdx, rcx, r8      #   eval+cast cnt
	ENAT	rsi, rdi, rdx, rcx, r8      #   eval+cast sof
	ENAT	rdx, rsi, rdi, rcx, r8      #   eval+cast dof
	EVAL	rcx, rsi, rdx, rdi, r8      #   eval src
	EVAL	r8,  rsi, rdx, rcx, rdi     #   eval dst
fastcopy:                                   # fastcopy:
	jiclz	r8, 1f                      #   if (dst not closure)
	mov	rax, r8                     #     res = dst
	ret                                 #     return res
1:	ppush	rdi, rsi, rdx, rcx          #   save all but dst
	mov	rdi, r8                     #   arg1 = dst
	call	copyclosure                 #   rax = copyclosure(dst)
	ppop	rdi, rsi, rdx, rcx          #   restore all (but dst)
	mov	r8, rax                     #   dst = rax
	jmp	fastcopyuniq                #   goto fastcopyuniq

# opcopyuniq():
#
# Copy is basically just a memcpy, but there are a lot of edge cases
# to handle.  The basic pseudocode is as follows:
#
#     for (i=0; i<count; i++)
#         dst[i+dstOff] = src[i+srcOff]
#
# However, each read/write could be out of bounds.  Out-of-bounds reads
# produce zeros and out-of-bounds writes have no effects.
#
# Simply capping the size of the copy to being within the valid range
# is sufficient except for the cases where where we read past end end
# of the input.  In that case, we need to produce zeros.
#
# This is implemented as a conditional memset(0) followed by a memcpy.
# This is logically straightforward, but there are a lot of variables
# to keep track of, which makes it a bit of a mouthful.  Tread carefully.

# rdi=cnt rsi=sof rdx=dof rcx=src r8=dst
opcopyuniq:                                 # opcopyuniq:
	ENAT	rdi, rsi, rdx, rcx, r8      #   eval+cast cnt
	ENAT	rsi, rdi, rdx, rcx, r8      #   eval+cast sof
	ENAT	rdx, rsi, rdi, rcx, r8      #   eval+cast dof
	EVAL	rcx, rsi, rdx, rdi, r8      #   eval src
	EVAL	r8,  rsi, rdx, rcx, rdi     #   eval dst
fastcopyuniq:                               # fastcopyuniq:
	jnclz	r8, fastcopyuniq.ret        #   if !(dst.isclz) return dst
	clzsz_	r9, r8                      #   r9/dwid = dst.sz
	xor	r10w, r10w                  #   r10/swid = 0
	jnclz	rcx, 1f                     #   if (isclz(src))
	clzsz_	r10, rcx                    #     swid = src.sz
1:	cmp	rsi, r10                    #   if (sof >= swid)
	jae	fastcopyuniq.ret            #     return dst
	cmp	rdx, r9                     #   if (dof >= dwid)
	jae	fastcopyuniq.ret            #     return dst
	test	rdi, rdi                    #   if (cnt == 0)
	jz	fastcopyuniq.ret            #     return dst
	sub	r10, rsi                    #   isize/r10 = swid-sof
	cmp	r10, rdi                    #   if (isize > cnt)
	cmova	r10, rdi                    #     isize = cnt
	sub	r9, rdx                     #   osize = dwid-dof
	cmp	r9, rdi                     #   if (osize > cnt)
	cmova	r9, rdi                     #     osize = cnt
	cmp	r9, r10                     #   if (osize <= isize)
	jbe	fastcopyuniq.nozero         #     goto nozero
fastcopyuniq.zero:                          # zero:
	ppush	rcx, rdi                    #   save src, cnt
	ptr_	rdi, r8                     #   dp = PTR(dst)
	lea	rdi, [rdi+8*rdx+8]          #   dp = &dp[dof]
	mov	rcx, r9                     #   cnt = osize
	xor	eax, eax                    #   val = 0
	rep	stosq                       #   memset
	ppop	rcx, rdi                    #   restore src, cnt
fastcopyuniq.nozero:                        # nozero:
	ptr_	rax, rcx                    #   tmp = PTR(src)
	lea	rax, [rax+8*rsi+8]          #   tmp = &tmp[sof]
	mov	rsi, rax                    #   sp = tmp
	ptr_	rdi, r8                     #   dp = PTR(dst)
	lea	rdi, [rdi+8*rdx+8]          #   dp = &dp[dof]
	mov	rcx, r9                     #   cnt = osize
	cmp	rcx, r10                    #   if (cnt > isize)
	cmova	rcx, r10                    #     cnt = isize
	rep	movsq                       #   memcpy
fastcopyuniq.ret:                           # ret:
	mov	rax, r8                     #   res = dst
	ret                                 #   return res

### Pattern Matching

.macro istype op, byte
	EVAL	rdi
	xor	eax, eax
	mov	rdx, 1
	shr	rdi, 56
	cmp	dil, \byte
	\op	rax, rdx
	ret
.endm

opispin: istype cmove, 0xC4
opislaw: istype cmove, 0xC8
opisapp: istype cmove, 0xD0
opisnat: istype cmovb, 0xC4

opeat:
Eat:
	test	r9, r9
	js	Eat.heap
	jz	Eat.zero
Eat.pos:
	ppush	r8
	mov	rdi, r9
	call	fastdec
	mov	rdi, rax
	ppop	rax
	jmp	apply1
Eat.zero:
	mov	rax, rcx
	JRAX
Eat.heap:
	tytag_	rax, r9
	jmp	[Eat.types + rax*8]
Eat.types:
	.quad	Eat.thk, Eat.clz, Eat.law, Eat.pin, Eat.pos
Eat.thk:
	EVAL	r9, rdi, rsi, rdx, rcx, r8
	jmp	Eat
Eat.clz:
	ppush	rdx
	mov	rdi, r9  # rdi=o:(f x) [a]
	call	split    # rax=f rdx=x [a] {rdi rsi rcx r9 r10}
	mov	rdi, rax
	mov	rsi, rdx
	ppop	rax
	jmp	apply2   # apply2(a,f,x)
Eat.pin:
	mov	rax, rdi
	dref_	rdi, r9
	jmp	apply1   # apply1(p, o.item)
Eat.law:
	mov	rax, rsi
	ptr	r9
	mov	rdi, [r9]
	mov	rsi, [r9+8]
	mov	rdx, [r9+16]
	jmp	apply3 # apply3(l,n,a,b)

### Exceptions

# INPUTS: rdi=error
# OUTPUTS: causes Try to return with 1[error]
#
# This works by simply restoring the stack to the state it was in the
# Try call, and then returning.

ThrowOp:                                    # rdi=err
	FORCE	rdi, rax                    # rdi=nf(err)
	mov	r8, [exn_top]               # restore stack from handler
	test	r8, r8                      # make sure it's not NULL.
	jz	Crash                       # If it is null, then crash
	mov	r15, r8                     # restore managed stack
	ppop	r8, rsp                     # restore native stack
	mov	[exn_top], r8               # restore previous exn state
	dref	rax                         # tag=(pinItem self)
	jmp	mkclz1                      # return rax=code[error]

# INPUTS: rdi=[func arg]
# OUTPUTS: rax=(0 result) OR rax=(1 error)

### EVAL/ENAT alwyas clobbers rax, so move rax (the syscall number) to the rcx
### register to work.
SyscallOp:
	call	unpacksyscall.sized # rax, rdi to r9 filled for syscall
	mov	rcx, rax            # rcx=temporary rax; eval always clobbers
	ENAT	rcx, rdi, rsi, rdx, r10, r8, r9
	ENAT	rdi, rcx, rsi, rdx, r10, r8, r9
	ENAT	rsi, rcx, rdi, rdx, r10, r8, r9
	ENAT	rdx, rcx, rdi, rsi, r10, r8, r9
	ENAT	r10, rcx, rdi, rsi, rdx, r8, r9
	ENAT	r8, rcx, rdi, rsi, rdx, r10, r9
	ENAT	r9, rcx, rdi, rsi, rdx, r10, r8
	mov	rax, rcx	# rax=restore evaluated rax
	syscall
	jmp mkword

xptr:                                       # xptr: (rdi=val)
	EVAL	rdi                         #   Get the nat
	jdirect	rdi, xptr.noref             #   Must be indirect
	ptr_	rax, rdi                    #   Get the pointer
	cmp	rax, r14                    #   After the moving heap?
	jae	xptr.ret                    #     Return it.
	mov	rdi, [heap_addr]            #   Loading heap_addr?
	cmp	rax, rdi                    #   Before the moving heap?
	jb	xptr.ret                    #     Return it
xptr.noref:                                 # noref:
	xor	eax, eax                    #   result=0 (not stable)
xptr.ret:                                   # ret:
	ret                                 #   return

xbuf:                                       # xptr: (rdi=val)
	EVAL	rdi                         #   evaluate the argument
	jdirect	rdi, mkbuffer               #   if direct nat then mkbuffer
	ud2                                 #   else crash (not a size)

prim:                                       # prim: (rdi=op)
	EVAL	rdi                         #   eval op
	jiclz	rdi, 1f                     #   if (op not closure)
	ud2                                 #     crash
1:	clzhd_	rsi, rdi                    #   rsi = tag
	cmp	rsi, 109                    #   if (tag > 109)
	ja	prim.oob                    #     goto oob
	shl	rsi, 4                      #   off = tag*16
	mov	r10, [prim.tab + rsi]       #   r10 = op ptr (not clobbered)
	mov	rsi, [prim.tab + rsi+8]     #   rsi = expected arity
	call	unpackop.sized              #   unpackop.sized(op, arity)
	jmp	r10                         #   goto tab[tag]
prim.oob:                                   # oob: rdi/op rsi/nm
	xchg	rdi, rsi                    #   rdi/nm rsi/op
	call	findop
	test	rax, rax
	jz	prim.nomatch
	mov	r10, rax
	mov	rdi, rsi                    #   arg1 = op
	mov	rsi, r8                     #   arg2 = arity
	call	unpackop.sized
	jmp	r10
prim.nomatch:
	ud2
prim.tab:
	.quad	oppin, 1                    # 0
	.quad	oplaw, 3                    # 1
	.quad	opinc, 1                    # 2
	.quad	opeat, 6                    # 3
	.quad	opforce, 1                  # 4
	.quad	opseq, 2                    # 5
	.quad	opsap2, 2                   # 6
	.quad	opsap3, 3                   # 7
	.quad	opsap4, 4                   # 8
	.quad	opsap5, 5                   # 9
	.quad	opsap6, 6                   # 10
	.quad	opsap7, 7                   # 11
	.quad	opsap8, 8                   # 12
	.quad	opispin, 1                  # 13
	.quad	opislaw, 1                  # 14
	.quad	opisapp, 1                  # 15
	.quad	opisnat, 1                  # 16
	.quad	optobit, 1                  # 17
	.quad	opnil, 1                    # 18
	.quad	optype, 1                   # 19
	.quad	opunpin, 1                  # 20
	.quad	opname, 1                   # 21
	.quad	oparity, 1                  # 22
	.quad	opbody, 1                   # 23
	.quad	opinit, 1                   # 24
	.quad	oplast, 1                   # 25
	.quad	opif, 3                     # 26
	.quad	opifz, 3                    # 27
	.quad	opnat, 1                    # 28
	.quad	opdec, 1                    # 29
	.quad	opfresh, 1                  # 30
	.quad	opand, 2                    # 31
	.quad	opor, 2                     # 32
	.quad	opnand, 2                   # 33
	.quad	opnor, 2                    # 34
	.quad	opadd, 2                    # 35
	.quad	opsub, 2                    # 36
	.quad	opmul, 2                    # 37
	.quad	oppow, 2                    # 38
	.quad	opbex, 1                    # 39
	.quad	opeq, 2                     # 40
	.quad	opne, 2                     # 41
	.quad	ople, 2                     # 42
	.quad	opge, 2                     # 43
	.quad	oplt, 2                     # 44
	.quad	opgt, 2                     # 45
	.quad	opcmp, 2                    # 46
	.quad	opdiv, 2                    # 47
	.quad	opmod, 2                    # 48
	.quad	opdivmod, 2                 # 49
	.quad	oplsh, 2                    # 50
	.quad	oprsh, 2                    # 51
	.quad	optrunc, 2                  # 52
	.quad	optrunc1, 1                 # 53
	.quad	optrunc8, 1                 # 54
	.quad	optrunc16, 1                # 55
	.quad	optrunc32, 1                # 56
	.quad	optrunc64, 1                # 57
	.quad	optrunc128, 1               # 58
	.quad	optrunc256, 1               # 59
	.quad	opcut, 3                    # 60
	.quad	opwipe, 3                   # 61
	.quad	opedit, 4                   # 62
	.quad	opbits, 1                   # 63
	.quad	optest, 2                   # 64
	.quad	opset, 2                    # 65
	.quad	opclear, 2                  # 66
	.quad	ophd, 1                     # 67
	.quad	opsz, 1                     # 68
	.quad	opcase, 3                   # 69
	.quad	opcase0, 2                  # 70
	.quad	opcase1, 2                  # 71
	.quad	opcase2, 2                  # 72
	.quad	opcase3, 4                  # 73
	.quad	opcase4, 5                  # 74
	.quad	opcase5, 6                  # 75
	.quad	opcase6, 7                  # 76
	.quad	opcase7, 8                  # 77
	.quad	opcoup, 2                   # 78
	.quad	opup, 3                     # 79
	.quad	opupuniq, 3                 # 80
	.quad	opix, 2                     # 81
	.quad	opix0, 1                    # 82
	.quad	opix1, 1                    # 83
	.quad	opix2, 1                    # 84
	.quad	opix3, 1                    # 85
	.quad	opix4, 1                    # 86
	.quad	opix5, 1                    # 87
	.quad	opix6, 1                    # 88
	.quad	opix7, 1                    # 89
	.quad	oprep, 3                    # 90
	.quad	oprow, 3                    # 91
	.quad	opcopy, 5                   # 92
	.quad	opswi, 4                    # 93
	.quad	opbytes, 1                  # 94
	.quad	opload, 3                   # 95
	.quad	operase, 3                  # 96
	.quad	opstore, 4                  # 97
	.quad	opsplice, 5                 # 98
	.quad	opload8, 2                  # 99
	.quad	opload16, 2                 # 100
	.quad	opload32, 2                 # 101
	.quad	opload64, 2                 # 102
	.quad	opstore8, 3                 # 103
	.quad	opstore16, 3                # 104
	.quad	opstore32, 3                # 105
	.quad	opstore64, 3                # 106
	.quad	oprefeq, 2                  # 107
	.quad	opcopyuniq, 5		    # 108
	.quad	opsnore, 5		    # 109
	# TODO: move seq2, seq3, etc
	# TODO: move store8/etc next to store

# Store is complex because there are a lot of edge-cases.
#
# -   If the size is 1, 2, 4, or 8 we should fallback to the
#     specialized versions (Store8, Store64, etc).
#
# -   If the size is bigger than the input, then we need to copy only
#     what is available, and then fill in zeros for the rest.
#
# -   The dumbest way to do this is to just always:
#
#     1.  Zero out the requested size.
#
#     2.  Copy in the available size.
#
# -   We will need to have a separate path for when the input is direct
#     vs when the input is indirect.
#
# -   We need to calculate in advance how big the output will need to
#     be in order to have space for our writes.
#
# -   Once we know the minimum output size, we can use reservecopyflex
#     in order to allocate the result.  This handles the cases where
#     the destination is direct for us.

.global faststore

opstore:                                    # opstore:
	ENAT	rdi, rsi, rdx, rcx          #   eval+cast rdi/off
	ENAT	rsi, rdi, rdx, rcx          #   eval+cast rsi/wid
	ENAT	rdx, rdi, rsi, rcx          #   eval+cast rdx/val
	ENAT	rcx, rdi, rsi, rdx          #   eval+cast rcx/buf
faststore:                                  # faststore:
	mov	r8, rsi                     #   r8 = wid
	mov	rsi, rdx                    #   rsi = val
	mov	rdx, rcx                    #   rdx = buf (rdi=off still)
	cmp	r8, 1                       #   if (wid==1)
	je	faststore8                  #     goto faststore8
	# cmp	r8, 2                       #   if (wid==2)
	# je	faststore16                 #     goto faststore16
	# cmp	r8, 4                       #   if (wid==4)
	# je	faststore32                 #     goto faststore32
	cmp	r8, 8                       #   if (wid==8)
	je	faststore64                 #     goto faststore64
faststore_generic:                          # faststore_generic:
	mov	r9, rdi                     #   r9=off
	mov	r10, rsi                    #   r10=val
	lea	rsi, [rdi+7]                #   min=off+7
	add	rsi, r8                     #   min=off+wid+7
	shr	rsi, 3                      #   min=(off+wid+7)/8
	mov	rdi, rdx                    #   tocopy=buf
	call	reservecopyflex             #   rax=buf rdx=bufsz r8=wid
	mov	r11, rax                    #   r11=buf
	lea	rdi, [rax+r9]               #   dst=buf+off
	xor	eax, eax                    #   v=0
	mov	rcx, r8                     #   cnt=bytes
	rep	stosb                       #   memset(dst, v, cnt)
	jheap	r10, faststore_indirect     #   if indirect(val) indirect case
faststore_direct:                           # faststore_direct:
	ppush	r10                         #   push to the stack
	mov	rsi, r15                    #   src = sp
	mov	rcx, 8                      #   cnt = 8
	call	faststore_common            #   re-use the indirect logic
	pdrop	1                           #   remove from the stack
	ret                                 #   return
faststore_indirect:                         # faststore_indirect:
	ptr_	rsi, r10                    #   src = PTR(val)
	bitsiz_	rcx, r10                    #   cnt = bitsiz(val)
	add	rcx, 7                      #   cnt = bitsiz(val)+7
	shr	rcx, 3                      #   cnt = (bitsiz(val)+7)/8
faststore_common:                           # faststore_common:
	lea	rdi, [r11+r9]               #   dst = buf+off
	cmp	rcx, r8                     #   if (cnt > wid)
	cmova	rcx, r8                     #     cnt=wid
	rep	movsb                       #   copy bytes
	mov	rdi, rdx                    #   bufsz (words)
	jmp	claim

opsap8: ud2
opsap7: ud2
opsap6: ud2
opsap5: ud2
opsap4: ud2
opsap3: ud2
opsap2:
	EVAL	rsi, rdi
	mov	rax, rdi
	mov	rdi, rsi
	jmp	apply1

opfresh: ud2
opnand: ud2
opnor: ud2
oppow: ud2
optrunc1: ud2
opcut: ud2
opwipe: ud2
opedit: ud2
opcase0: ud2
opcase1: ud2
opcase2: ud2
opcase6: ud2
opcase7: ud2


# \subsection{Rep}
#
# Rep creates an array with a certain head and a single value repeated
# many times.
#
#     (Rep hd v n)=(times acc&(acc v) (Nat hd) n)
#
# The repeated value is not evaluted and can be lazy.
#
# The head is cast to a Nat so that we are guarenteed that the result
# is a closure and not a thunk.
#
# Some notes on implementation concerns:
#
# If the requested size is zero, we return the head instead of
# constructing a closure.  Because of this, the result is not guarenteed
# to be unique (TODO: is this a problem for Blitz, should we do something
# about this?)
#
# If the given size is indirect, then the result should be a failure to
# allocate (resource exaustion).  However, the word-value of a bignat will
# always be big enough to trigger resource exaustion, so we don't have to
# explicitly handle this case (which saves us a branch).

oprep:                                      # oprep: hd/rdi vl/rsi n/rdx
	ENAT	rdx, rdi, rsi               #     eval+cast n
	ENAT	rdi, rsi, rdx               #     eval+case hd
	# fallthrough to fastrep

# Note that closure() only clobbers r8 and r9, so we don't need to
# preserve any registers in order to call `closure`.

fastrep:                                    # fastrep: hd/rdi vl/rsi n/rdx
	test	rdx, rdx
	jnz	1f
	mov	rax, rdi
	ret
1:	xchg	rsi, rdx                    # hd/rdi n/rsi vl/rdx
	call	closure                     #     rax = closure(hd, n)
	xchg	rax, rdx                    # hd/rdi n/rsi rax/vl rdx/res
	ptr_	r8, rdx                     #     dst = PTR(res)
	mov	[r8], rdi                   #     dst[0] = hd
	lea	rdi, [r8+8]                 #     dst/rdi = &dst.ix(0)
	mov	rcx, rsi                    #     cnt = n
	rep	stosq                       #     memset(dst, vl, cnt)
	mov	rax, rdx
	ret

optype:                                     # optype: (rdi=thunk)
	EVAL	rdi                         #   eval rdi
fasttype:                                   # fasttype: (rdi=whnf)
	tytag_	rsi, rdi                    #   thk=0 clz=1 law=2 pin=3 nat=4
	mov	rax, 4                      #   res=4
	sub	rax, rsi                    #   nat=0 pin=1 law=2 clz=3
	xor	rsi, rsi                    #   tmp=0
	test	rdi, rdi                    #   if input is direct
	cmovns	rax, rsi                    #   result=0
	ret                                 #   return

opunpin:				    # opunpin: (rdi=thunk)
	EVAL	rdi			    #   eval rdi
	xor	eax, eax		    #   zero return value on non-pin
	jnpin	rdi, 2f			    #   if not pin, skip unpinning
	mov	rax, rdi                    #   unrolled dref_ so i can nobeef
	ptr	rax			    #   tagged val to ptr
	mov	rsi, [rax + 16]		    #   look at pin_count/ptr[2]
	nobeef	rsi			    #   assert valid pin count
	cmp	rax, [base_ptr]             #   if under heap base pointer
	jb	1f                          #   then just unpin
	mov	rsi, [rax - 16]             #   rsi = redirection pointer
	cmp	rsi, 0                      #   if redirection is 0
	je	1f                          #   then just unpin
	cmp	rsi, -1                     #   if redirection is -1 (0xFF...)
	je	1f                          #   then just unpin
	mov	rax, rsi                    #   follow redirection pointer
1:	mov	rax, [rax]		    #   actually unpin
2:	ret				    #   return

opname:
	EVAL	rdi
	xor	eax, eax
	jnlaw	rdi, 1f
	dref_	rax, rdi
1:	ret

oparity:
	EVAL	rdi
	xor	eax, eax
	jnlaw	rdi, 1f
	drefo_	rax, rdi, 8
1:	ret

opbody:
	EVAL	rdi
	xor	eax, eax
	jnlaw	rdi, 1f
	drefo_	rax, rdi, 16
1:	ret

opswi: ud2
operase: ud2
opsplice: ud2
opload16: ud2
opload32: ud2
opload64: ud2
opstore16: ud2
faststore16: ud2
opstore32: ud2
faststore32: ud2

oprefeq:                                    # oprefeq: rdi/a rsi/b
	EVAL	rdi, rsi                    #     eval a
	EVAL	rsi, rdi                    #     eval b
fastrefeq:
	cmp	rdi, rsi
	sete	al
	movzx	rax, al
	ret

ReadOp: # rdi=[fd nat size offset]
	mov	rsi, 4
	call	unpackop.sized # rdi=fd rsi=nat rdx=count rcx=offset
	ENAT	rdi, rsi, rdx, rcx
	ENAT	rsi, rdi, rdx, rcx
	ENAT	rdx, rdi, rsi, rcx
	ENAT	rcx, rdi, rsi, rdx
	ppush	rsi                         # Push the buffer
	jdirect	rsi, ReadOp.direct
ReadOp.indirect:
	bitsiz_	r8, rsi                     # r8=bitsize(nat)
	ptr	rsi                         # rsi/ptr = PTR(nat)
	jmp	ReadOp.final
ReadOp.direct:
	lzcnt	r9, rsi
	mov	r8, 64
	sub	r8, r9                      # r8=bitSize(nat)
	mov	rsi, r15                    # rsi/ptr = sp
ReadOp.final: # rdi=fd rsi=ptr rdx=sz rcx=off
	add	r8, 7
	shr	r8, 3                       # r8/byteSize = bitSize+7/8
	sub	r8, rcx                     # bytes -= offset
	jc	ReadOp.zero                 #     if (underflow) return 0
	cmp	r8, 1                       # if (bytes <= 1)
	jbe	ReadOp.zero                 #     return 0
	test	rdx, rdx                    # if (sz == 0)
	jz	ReadOp.zero                 #     return 0
	cmp	rdx, r8                     # if (sz >= bytes)
	jge	ReadOp.zero                 #     return 0 (oob)
ReadOp.syscall:
	xor	eax, eax   # rax/syscall_id = 0 (read)
	add	rsi, rcx   # rsi/ptr += off
	syscall            # rdi=fd rsi=size rdx=ptr => rax=read
	ppop	rdi        # new_buffer = rdi
	test	rax, rax   # if (result >= 0)
	jns	mkclz1     #    return bytes_read[new_buffer]
	neg	rax        # code = -code
	ret                # return code
ReadOp.zero:
	xor	eax, eax
	ret

WriteOp:                                    # rdi=[fd nat count]
	mov	rsi, 4
	call	unpackop.sized # rdi=fd rsi=nat rdx=count rcx=offset
	ENAT	rdi, rsi, rdx, rcx
	ENAT	rsi, rdi, rdx, rcx
	ENAT	rdx, rdi, rsi, rcx
	ENAT	rcx, rdi, rsi, rdx
WriteOp.nats:
	jdirect	rsi, WriteOp.direct
	ptr	rsi                         # rsi=ptr(nat)
	add	rsi, rcx                    # rsi += offset
	mov	rax, 1                      # syscall=WRITE
	syscall
	test	rax, rax
	js	WriteOp.error               # crash if negative
	ret
WriteOp.direct:
	ppush	rsi
	mov	rsi, r15
	add	rsi, rcx                    # rsi += offset
	mov	rax, 1                      # syscall=WRITE
	syscall
	ppop	rsi
	test	rax, rax
	js	WriteOp.error               # crash if negative
	ret
WriteOp.error:
	ud2

### TODO: This shouldn't be here. We should actually remove this, along with
### WriteOp, but that depends on getting working xbufs from user land.
###
### There is no send() syscall on Linux, this is calling sendto() with the extra
### fields set to 0.
###
SendOp:					# rdi=[fd nat count offset flags]
	mov	rsi, 5
	call	unpackop.sized # rdi=fd rsi=nat rdx=count rcx=offset r8=flags
	ENAT	rdi, rsi, rdx, rcx, r8
	ENAT	rsi, rdi, rdx, rcx, r8
	ENAT	rdx, rdi, rsi, rcx, r8
	ENAT	rcx, rdi, rsi, rdx, r8
	ENAT	r8, rdi, rdi, rdx, rcx
SendOp.nats:
	jdirect	rsi, SendOp.direct
	ptr	rsi                         # rsi=ptr(nat)
	add	rsi, rcx                    # rsi += offset
	mov	rax, SYS_SENDTO
	mov	r10, r8		            # r10=flags
	mov	r8, 0			    # r8=null sockaddr*
	mov	r9, 0			    # r9=0 sizeof(sockaddr)
	syscall
	jmp mkword
SendOp.direct:
	ppush	rsi
	mov	rsi, r15
	add	rsi, rcx                    # rsi += offset
	mov	rax, SYS_SENDTO
	mov	r10, r8		            # r10=flags
	mov	r8, 0			    # r8=null sockaddr*
	mov	r9, 0			    # r9=0 sizeof(sockaddr)
	syscall
	ppop	rsi
	jmp mkword


TryOp:                                      # rdi=[f x]
	mov	rsi, 2
	call	unpackop.sized              # rdi=f rsi=x
	FORCE	rdi, rsi                    # nf(f)
	FORCE	rsi, rdi                    # nf(x)
	mov	rax, [exn_top]              # rax = old handler
	ppush	rax, rsp                    # push old handler + stack
	mov	[exn_top], r15              # new handler = stack pointer
	mov	rax, rdi                    # func=f
	mov	rdi, rsi                    # arg=x
	call	apply1.whnf                 # rax=whnf(f x)
	call	force.newabi                # rax=nf(f x)
	ppop	r8, r9                      # pop restore frame
	mov	[exn_top], r8               # restore previous handler
	mov	rdi, rax                    # arg1=result
	xor	eax, eax                    # tag=0
	jmp	mkclz1                      # return rax=0[result]

### Commits a single item to on disk persistence without updating
Precommit: # rdi=thunktoforce
	FORCE	rdi
	ppush	r11, r10, r9, r8, rbp, rdi, rsi, rdx, rcx, rbx
	call	precommit_shim
	ppop	r11, r10, r9, r8, rbp, rdi, rsi, rdx, rcx, rbx
	JRAX

Commit:
	FORCE	rdi
	ppush	r12, r11, r10, r9, r8, rbp, rdi, rsi, rdx, rcx, rbx
	call	commit_shim
	ppop	r12, r11, r10, r9, r8, rbp, rdi, rsi, rdx, rcx, rbx
	ret

### Input and Output

# TODO: create a variant which is not sized, so that we can have a generic
# "primop" call, like:
#
#     (#4 0[x])    => NAT(x)+1
#     (#4 1[x])    => NAT(x)-1
#     (#4 2[x y])  => NAT(x)+NAT(y)
#     (#4 77[f x]) => try(f,x)
#     (#4 88[f])   => force(f), print(f), return 0
#
# This should always run `unpack` at the beginningg, switch on `rax,
# and then jump to the appropriate routine.  This avoids the need to
# have a separate unpack prelude before each operator definition.
#
# And the same pattern for syscalls (but syscalls use a different
# register order).
#
#     (##5 1[fd ptr sz])
#
# The above is sys_write.  The syscall primitive can simply load these
# values into registers and then call `syscall` without actually looking
# at the code or the size.
#
# And the same pattern for syscalls (but syscalls use a different
# register order).
#
#     (##5 1[fd ptr sz])
#
# The above is sys_write.  The syscall primitive can simply load these
# values into registers and then call `syscall` without actually looking
# at the code or the size.
#
# And then the special system operations would use the same pattern:
#
#     (##6 0[a b c])   -> peek
#     (##6 1[a b c d]) -> poke
#     (##6 2[...]) -> read
#     (##6 3[...]) -> write
#     (##6 4[...]) -> open
#
# Actually, should unpackop just always use the syscall register order?
#
# Hmm, actually, it seems like almost all syscalls read/write memory
# and would need to be wrapped in order to have a nice PLAN interface.
# Should we just do that, or try to be clever about it?

unpackop.sized: # rdi=c[x y..] rsi=expectedSz ==> r11=sz rax=c rdi=x rsi=y..
	mov	rax, rdi
	ERAX	rsi                         # evaluate arg
	jnclzt	rdi, unpackop.err           # error if not app
	clzsz_	r11, rdi                    # r11=sz(cmd)
	cmp	r11, rsi
	jne	unpackop.err
	jmp	[unpackop.tab + 8*r11 - 8]
unpackop.tab:
	.quad	unpackop_1, unpackop_2, unpackop_3
	.quad	unpackop_4, unpackop_5, unpackop_6
unpackop_6:
	drefo_	r9, rax, 48      # r9 = arg6
unpackop_5:
	drefo_	r8, rax, 40      # r8 = arg5
unpackop_4:
	drefo_	rcx, rax, 32     # rcx = arg4
unpackop_3:
	drefo_	rdx, rax, 24     # rdx = arg3
unpackop_2:
	drefo_	rsi, rax, 16     # rsi = arg2
unpackop_1:
	drefo_	rdi, rax, 8      # rdi = arg1
	dref	rax              # rax=tag
	ret
unpackop.err:
	ud2

### Variant of unpackop that doesn't check size, it just sets all registers
### past the size to zero.
###
### TODO: Merge with unpackop if we decide to just use the syscall convention.
###
unpacksyscall.sized: # rdi=c[x y..] ==> rax=c rdi=x rsi=y..
	mov	rax, rdi
	jnclzt	rdi, unpacksyscall.err      # error if not app
	clzsz_	r11, rdi                    # r11=sz(cmd)
	mov	r9, 6
	cmp	r11, r9
	ja	unpacksyscall.err # if (sz > 6) goto error
	xor	r9, r9
	xor	r8, r8
	xor	r10, r10
	xor	rdx, rdx
	xor	rsi, rsi
	xor	rdi, rdi
	## rdi should always get filled below.
	jmp	[unpacksyscall.tab + 8*r11 - 8]
unpacksyscall.tab:
	.quad	unpacksyscall_1, unpacksyscall_2, unpacksyscall_3
	.quad	unpacksyscall_4, unpacksyscall_5, unpacksyscall_6
unpacksyscall_6:
	drefo_	r9, rax, 48      # r9 = arg6
unpacksyscall_5:
	drefo_	r8, rax, 40      # r8 = arg5
unpacksyscall_4:
	drefo_	r10, rax, 32     # r10 = arg4
unpacksyscall_3:
	drefo_	rdx, rax, 24     # rdx = arg3
unpacksyscall_2:
	drefo_	rsi, rax, 16     # rsi = arg2
unpacksyscall_1:
	drefo_	rdi, rax, 8      # rdi = arg1
	dref	rax              # rax=tag
	ret
unpacksyscall.err:
	ud2

	
# rdi=[x y] ==> rax=y
TraceOp:
	mov	rsi, 2
	call	unpackop.sized
# rdi=x rsi=y ==> rax=y
Trace: # TODO: replace this jet with raw syscall effects.
	ppush	rsi
	FORCE	rdi
	ppush	r11, r10, r9, r8, rbp, rdi, rsi, rdx, rcx, rbx, rax
	call	trace_value
	ppop	r11, r10, r9, r8, rbp, rdi, rsi, rdx, rcx, rbx, rax
	ppop	rax
	JRAX

opseq:
	EVAL	rdi, rsi
	mov	rax, rsi
	JRAX

Crash:
	mov	rdi, 9
	jmp	syscall_exit_group

### Nat Comparison

.global opcmp
opcmp: # rax/natCmp rdi/Thunk rsi/Thunk -> rax/Ord
	ENAT	rdi, rsi
	ENAT	rsi, rdi
compare: # rdi/Nat rsi/Nat -> rax/Ord
	test	rdi, rsi
	js	compare.indirect
	cmp	rdi, rsi
compare.enum:
	seta	al
	setae	ah
	add	al, ah
	movzx	rax, al # b=0, e=1, a=(1+1)
compare.done:
	ret
compare.indirect:
	ptr	rdi
	ptr	rsi
	mov	rcx, [rdi-8]
	cmp	rcx, [rsi-8] # Direct RECORD compare is okay.
	jne	compare.enum
	gcsz	rcx          # rcx = gcsize(rcx/gcrecord)
compare.loop:
	mov	rax, 1
	dec	rcx
	js	compare.done
	mov	rax, [rdi + rcx*8]
	cmp	rax, [rsi + rcx*8]
	jne	compare.enum
	jmp	compare.loop

.macro natcmp op
	call	opcmp
	cmp	rax, 1
	\op	al
	ret
.endm

opeq: # natcmp sete
	call	opcmp
	cmp	rax, 1
	sete	al
	ret
opne:
NatNeq: # natcmp setne
	call	opcmp
	cmp	rax, 1
	setne	al
	ret
opgt: natcmp seta
opge: natcmp setae
oplt: natcmp setb
ople: natcmp setbe

### Plan Comparision (TODO: nuke all of this)

Cmp:
	EVAL	rdi, rsi
	EVAL	rsi, rdi
Cmp.whnf:
	jinat	rdi, Cmp.xnat
	jinat	rsi, Cmp.greater
	mov	r8, rdi
	shl	r8, 1
	shr	r8, 61
	mov	r9, rsi
	shl	r9, 1
	shr	r9, 61
	cmp	r8, r9
	jne	Cmp.type_mismatch
	slots_	r10, rdi
	slots_	r11, rsi
	cmp	r10, r11
	jne	compare.enum
	cmp	r8, 1
	cmove	r10, r8
	mov	rax, 2
	cmp	r8, 3
	cmove	r10, rax
	mov	rdx, r10
	xor	rcx, rcx
Cmp.rowloop: # rdi=x rsi=y rcx=i rdx=count
	cmp	rcx, rdx
	jae	Cmp.equal
	ppush	rdi, rsi, rcx, rdx
	drefi	rdi, rcx
	drefi	rsi, rcx
	call	Cmp
	ppop	rdi, rsi, rcx, rdx
	inc	rcx
	cmp	rax, 1
	je	Cmp.rowloop
	ret
Cmp.equal:
	mov	rax, 1
	ret
Cmp.greater:
	mov	rax, 2
	ret
Cmp.xnat:
	jinat	rdi, compare
Cmp.lesser:
	mov	rax, 0
	ret
Cmp.type_mismatch:
	cmp	r8, 1
	je	Cmp.lesser
	cmp	r9, 1
	je	Cmp.greater
	cmp	r8, 2
	je	Cmp.lesser
	jmp	Cmp.greater

### Legal Interpretation

argpop: # rdi/count -> pops args (rdi, rsi, ..) from the stack
	mov	r11, 6
	cmp	rdi, r11
	cmova	rdi, r11
	jmp	[argpop.table + rdi*8 - 8]
argpop.table:
	.quad	argpop1, argpop2, argpop3, argpop4, argpop5, argpop6
argpop6:
	ppop	rdi, rsi, rdx, rcx, r8, r9
	ret
argpop5:
	ppop	rdi, rsi, rdx, rcx, r8
	ret
argpop4:
	ppop	rdi, rsi, rdx, rcx
	ret
argpop3:
	ppop	rdi, rsi, rdx
	ret
argpop2:
	ppop	rdi, rsi
	ret
argpop1:
	ppop	rdi
	ret

argpush: # r11/count -> pushes args (rdi, rsi, ..) to the stack
	mov	r10, 6
	cmp	r11, r10
	cmova	r11, r10
	jmp	[argpush.table + r11*8 - 8]
argpush.table:
	.quad	argpush1, argpush2, argpush3, argpush4, argpush5, argpush6
argpush6:
	ppush	rdi, rsi, rdx, rcx, r8, r9
	ret
argpush5:
	ppush	rdi, rsi, rdx, rcx, r8
	ret
argpush4:
	ppush	rdi, rsi, rdx, rcx
	ret
argpush3:
	ppush	rdi, rsi, rdx
	ret
argpush2:
	ppush	rdi, rsi
	ret
argpush1:
	ppush	rdi
	ret

Blitzer: # Interpreter for Blitz invokation
	nop
Blitz:
	nop
Judge: # rax=func rdi=arg1 rsi=arg2 .. -> rax/result
	drefo_	r11, rax, 32  # r11=arity rax=func
	call	argpush       # r11=arity [rax arg1 arg2 ...]
	ppush	rax           # r11=arity [func rax arg1 arg2 ...]
	mov	rdx, [r15]
	_jilaw	rdx, Judge.setup_has_law
	dref	rdx
Judge.setup_has_law:
	drefo_	r10, rdx, 40
	drefo_	r11, rdx, 32
	test	r10, r10
	jz	Judge.expo_setup
	xor	rcx, rcx
Judge.holes_loop:
	cmp	rcx, r10
	jae	Judge.holes_done
	push	rcx
	xor	rdi, rdi
	lea	rsi, xvar
	sub	r15, 8
	mov	[r15], rdi
	call	mkthunk # placeholder thunk for let bindings.
	pop	rcx
	inc	rcx
	jmp	Judge.holes_loop
Judge.holes_done:
	mov	rax, r10
	xor	rcx, rcx
Judge.reorg_loop:
	cmp	rcx, r11
	ja	Judge.reorg_done
	mov	r8, [r15 + rcx*8]
	mov	r9, [r15 + rax*8]
	mov	[r15 + rcx*8], r9
	mov	[r15 + rax*8], r8
	inc	rcx
	inc	rax
	jmp	Judge.reorg_loop;
Judge.reorg_done:
	mov	rdx, [r15]
	_jilaw	rdx, Judge.expo_setup
	dref	rdx
Judge.expo_setup:
	mov	rsi, r15
	mov	rdi, r11
	add	rdi, r10
	sub	r15, 8
	drefo_	r8, rdx, 16
	mov	[r15], r8
	xor	rcx, rcx
Judge.lets_loop:
	cmp	rcx, r10
	jae	Judge.body
	mov	rax, [r15]
	drefo_	r8, rax, 16
	mov	[r15], r8
	sub	r15, 8
	drefo_	r8, rax, 8
	mov	[r15], r8
	push	rcx
	push	r10
	push	r11
	push	rdi
	push	rsi
	call	expo
	pop	rsi
	pop	rdi
	pop	r11
	pop	r10
	pop	rcx
	mov	r8, r11
	add	r8, rcx
	add	r8, 3
	mov	r8, [r15 + r8*8]
	ptr	r8
	mov	r9, [r15]
	mov	[r8 + 8], r9
	pdrop	1
	inc	rcx
	jmp	Judge.lets_loop
Judge.body:
	push	rdi
	call	expo
	pop	rdi
	mov	rax, [r15]
	lea	r15, [r15 + rdi*8 + 8]
	pdrop	1
	JRAX

expo:
	mov	r10, rdi                    # r10 = maxref
	mov	r11, rsi                    # r11 = environment
	mov	rdi, [r15]                  # rdi = body
	call	bloom                       # bloom(body)
	mov	[r15], rax
bloom:                                      # bloom(x/Exp) -> Thunk
	push	rbp
	mov	rsi, 1                      # mode=UNCACHED
	call	bloom.recur
	pop	rbp
	ret
bloom.recur:
	mov	rdx, 0                      # n=0
bloom.expand:
	cmp	rdi, r10                    # if (x > maxref)
	ja	bloom.notref                # then goto maxref
	mov	rdi, [r11+8*rdi]            # x=env[x]
	jmp	bloom.examine               # goto examine
bloom.notref:
	mov	rax, rdi
	rol	rax, 16                     # get tag into low 16 bits
	cmp	ax, 0xD002                  # if not x ~ 0[_ _]
	jne	bloom.notapp                # then goto notapp
bloom.app_exp:
	shr	rax, 16                     # x = PTR(x)
	mov	r8, [rax+16]                # x = (a in 0[f a])
	ppush	r8                          # and push it.
	inc	rdx                         # n++
	mov	rdi, [rax+8]                # x = (f in 0[f x])
	jmp	bloom.expand                # goto expand
bloom.notapp:
	cmp	ax, 0xD001                  # if not x ~ 0[_]
	jne	bloom.notquote              # then goto notquote
bloom.quote_exp:
	shr	rax, 16                     # x = ptr(x)
	mov	rdi, [rax+8]                # x = x[1]
	#                                   # goto examine
bloom.notquote:
	#                                   # goto examine
bloom.examine:
	xor	ecx, ecx                    # i=0
	mov	rax, rdi                    # result=x
	test	rdx, rdx                    # if (n != 0)
	jnz	bloom.params                # then goto bloom.params
	ret                                 # return result (x)
bloom.params:
	jinat	rdi, bloom.mkclz            # goto mkclz if IsNat(x)
	mov	rax, rdi
	shl	rax, 2
	lzcnt	rax, rax
	jmp	[bloom.table+8*rax]         # switch x.type (THUNK,CLZ,LAW,PIN)
bloom.table:
	.quad	bloom.isthk
	.quad	bloom.isapp
	.quad	bloom.islaw
	.quad	bloom.ispin
bloom.isthk:
	xor	r8, r8                      # submode=CACHED
	jmp	bloom.mkthk                 # goto mkthk
bloom.ispin:
	ptr_	rax, rdi                    # xp = PTR(x)
	mov	rbp, [rax+16]               # xp->pincount
	nobeef	rbp                         # assert valid pincount
	mov	rbp, [rax+32]               # arity = xp->pinarity
	mov	rax, [rax+24]               # fp    = xp->exec
	cmp	rdx, rbp                    # if (n < arity)
	jb	bloom.mkclz                 # then goto mkclz
	lea	r9, Judge
	xor	r8, r8                      # submode = CACHED
	cmp	rax, r9                     # if (fp == Judge)
	je	bloom.mkthk                 #     goto mkthk
	lea	r9, opcase
	cmp	rax, r9                     # if (fp == opcase)
	je	bloom.mkjmp                 #     goto mkjmp
	lea	r9, opix
	cmp	rax, r9                     # if (fp == opix)
	je	bloom.mkjmp                 #     goto mkjmp
	inc	r8                          # submode = UNCACHED
	jmp	bloom.mkthk                 # goto mkthk
bloom.islaw:
	ptr_	rax, rdi                    # xp = PTR(x)
	drefo_	rbp, rax, 32                # arity = xp->lawarity
	cmp	rdx, rbp                    # if (n < arity)
	jb	bloom.mkclz                 #     goto mkclz
	xor	r8d, r8d                    # submode=CACHED
	jmp	bloom.mkthk                 # goto mkthk
bloom.isapp:
	clzsz_	r8, rdi                     # sz = CLZSZ(x)
	ptr_	rax, rdi                    # xp = PTR(x)
bloom.isapp.loop:
	test	r8, r8                      # if (sz == 0)
	jz	bloom.isapp.done            #     break
	mov	r9, [rax+8*r8]              # tmp = xp[sz]
	ppush	r9                          # push(tmp)
	inc	rdx                         # n++
	inc	rcx                         # i++
	dec	r8                          # sz--
	jmp	bloom.isapp.loop            # continue
bloom.isapp.done:
	mov	rdi, [rax]                  # x = xp[0]  (head of closure)
	jmp	bloom.params                # continue
bloom.mkclz:
	mov	r8, 0                       # submode=CACHED
	mov	r9, 1                       # tmp=UNCACHED
	cmp	rsi, 2                      # if (mode == JUMP_TABLE)
	cmove	r8, r9                      # then submode=UNCACHED
	ppush	rdi                         # push x
bloom.mkclz.loop:
	cmp	rcx, rdx                    # if (rcx >= rdx)
	jae	bloom.mkclz.break           #     break
	inc	rcx                         # i++
	push	rcx
	push	rdx                         # save i, n, submode
	push	r8
	mov	rdi, [r15+8*rcx]            # x=sp[i]
	mov	rsi, r8                     # mode=submode
	call	bloom.recur                 # rax = bloom.recur(x,mode)
	pop	r8
	pop	rdx                         # restore i, n, submode
	pop	rcx
	mov	[r15+8*rcx], rax            # sp[i] = rax
	jmp	bloom.mkclz.loop            # continue
bloom.mkclz.break:
	mov	rdi, rdx                    # arg2 = n
	jmp	mkclosure                   # tailcall mkclosure(n)
bloom.mkthk:
	ppush	rdi, rsi                    # save(x, mode)
bloom.mkthk.loop:
	cmp	rcx, rdx                    # if (i >= n)
	jae	bloom.mkthk.break           # then break
	mov	r9, 0                       # tmp=CACHED
	cmp	rcx, rbp                    # if i>arity
	cmova	r8, r9                      # then submode=CACHED
	mov	rdi, [r15+8*rcx+16]         # x = sp[i+2]
	ppush	rcx, rdx, r8, rbp           # save i, n, submode
	mov	rsi, r8                     # mode=submode
	call	bloom.recur                 # rax = bloom.recur(x, mode)
	ppop	rcx, rdx, r8, rbp           # restore i, n, submode
	mov	[r15+8*rcx+16], rax         # sp[i+2] = x
	inc	rcx                         # i++
	jmp	bloom.mkthk.loop            # continue
bloom.mkthk.break:
	ppop	rdi, rsi                    # restore(x,mode)
	xchg	rsi, rdx
	test	dl, dl
	setz	dl
	jmp	apple                       # apple(x, n, mode==CACHED)
bloom.mkjmp:
	ppush	rdi, rsi                    # save x, n
	mov	r8, 1                       # submode=UNCACHED
bloom.mkjmp.loop:
	cmp	rcx, rdx                    # if (i >= n)
	jae	bloom.mkjmp.break           # then break
	push	rcx
	push	rdx                         # save i, n, submode, arity
	push	r8
	push	rbp
	mov	rsi, r8                     # mode=submode
	mov	r9, 0
	cmp	rcx, rbp                    # if i>arity
	cmova	rsi, r9                     # then mode=CACHED
	mov	r9, 2                       # tmp=JMP_TABLE
	cmp	rcx, 1                      # if (i == 1)
	cmove	rsi, r9                     # then mode=JUMP_TABLE
	mov	rdi, [r15+8*rcx+16]         # x = sp[i+2]
	call	bloom.recur                 # rax = bloom.recur(x, mode)
	pop	rbp
	pop	r8
	pop	rdx                         # restore i, n, submode, arity
	pop	rcx
	mov	[r15+8*rcx+16], rax         # sp[i+2] = rax
	inc	rcx                         # i++
	jmp	bloom.mkjmp.loop            # continue
bloom.mkjmp.break:
	mov	rsi, rdx                    # arg2=n
	ppop	rdi, rdx                    # restore x, mode
	test	dl, dl
	setz	dl                          # arg3 = mode==CACHED
	jmp	apple                       # tailcall apple(x, n, cached)


### Graph Reduction

# TODO: instead of always using the stack, have a special unrolled state
# machine.
#
# We already have apply1, apply2, etc.  Instead of having those fallback
# to the stack-based mechanism, they should attempt to just directly
# pass their registers to the underlying operator.
#
# If those encounter small closures in their heads, we should have the
# whole state-machine unwrapped up to 6 arguments.  apply1_1, apply1_2,
# .., apply1_5, apply2_1, .. apply5_1 .. apply6.
#
# I was thinking this through before, and I think that most of the moving
# of registers around can live in just a few blocks with a bunch of
# different entry-points into the same blocks.

apply:
	mov	r8, rdi # TODO: Use a jump table keyed on type enum
apply.loop:
	mov	r9, [r15]
	jnthk	r9, apply.notthk
	push	r8          # save args
	mov	rdi, r9
	mov	rax, rdi
	dref	rdi
	call	rdi         # Invokes EXEC (THUNK PROCEDURE)
	pop	r8
	mov	r9, rax
	mov	[r15], rax
apply.notthk:
	jnclzt	r9, apply.notclz
apply.closure:
	ptr_	rsi, r9         # source = PTR(clz)
	mov	rcx, [rsi - 8]  # HEAP RECORD
	shr	rcx, 14         # nodesz
	dec	rcx             # clz args
	add	r8, rcx         # args += clz args
	shl	rcx, 3
	sub	r15, rcx        # grow(args)
	shr	rcx, 3
	inc	rcx
	mov	rdi, r15        # dest = stack
	rep	movsq           # copy
	mov	r9, [r15]       # load func from closure
apply.notclz:
	jnnat	r9, apply.notnat
	mov	rdi, r8
	jmp	mkclosure
apply.oversat:
	mov	rdi, rcx
	sub	r8, rcx
	push	r8
	push	r9
	mov	rdi, rcx
	call	operate # operate(rdi=arity, rax=operator)
	ppush	rax
	pop	r9
	pop	r8
	jmp	apply.loop
apply.notnat:
	ptr_	rsi, r9
	mov	rcx, [rsi + 32] # arity
	mov	rax, [rsi + 24]
	mov	rdi, r8
	test	rcx, rcx
	jz	mkclosure # For arity=0
	cmp	r8, rcx
	jb	mkclosure
	ja	apply.oversat
	mov	rdi, rcx # rax=operator rdi=arity
operate: # rax=operator rdi=arity [function arg1 ...] -> rax=result
	mov	r10, rax # stash the operator into r10
	ppop	rax      # pop the function
	call	argpop   # argpop(arity) pops into rdi, rsi, etc
	jmp	r10      # Invokes OPER (TODO: tail call!)
### =operate= invokes an operator from the stack.

apply3: # rax=f rdi=x rsi=y rdx=z -> rax=whnf(f x y z)
	ERAX	rdi, rsi, rdx
apply3.whnf:
	jinat	rax, mkclz3
	_jiclz	rax, apply3.fallback
apply3.pinlaw:
	ptr_	r8, rax
	mov	r9,  [r8+32] # arity
	mov	r10, [r8+24] # exec
	cmp	r9, 3
	ja	mkclz3
	jb	apply3.fallback
	jmp	r10
apply3.fallback:
	ppush	rax, rdi, rsi, rdx
	mov	rdi, 3
	jmp	apply

apply1: # rax=f rdi=x -> rax=whnf(f x)
	ERAX	rdi
apply1.whnf:
	jinat	rax, mkclz1                 # if f is nat, mkclz
	_jiclz	rax, apply1.clz
apply1.pinlaw:
	ptr_	r8, rax
	mov	r9,  [r8+32] # arity
	mov	r10, [r8+24] # exec
	cmp	r9, 1                       # if arity!=1 mkclz
	jne	mkclz1
	jmp	r10
apply1.clz:                                 # clz: rax=f rdx=x (f is clz)
	clzsz_	r10, rax                    #   n = f.sz
	cmp	r10, 2                      #   compare n with 2
	jb	apply1_1                    #   if n=1 apply1_1 fastpath
	je	apply1_2                    #   if n=2 apply1_2 fastpath
	ppush	rax, rdi                    #   push both
	mov	rdi, 1                      #   args=1
	jmp	apply                       #   apply(1) (fallback to slowpath)
apply1_2:
	mov	rdx, rdi       # arg3=arg1
	drefo_	rsi, rax, 16   # arg2=fn[2]
	drefo_	rdi, rax, 8    # arg1=fn[1]
	drefo	rax, 0         # fn=fn[0]
	jmp	apply3.whnf
apply1_1:
	mov	rsi, rdi       # arg2=arg1
	drefo_	rdi, rax, 8    # arg1=fn[1]
	drefo	rax, 0         # fn=fn[0]
	jmp	apply2.whnf

# apply2 : rax=f rdi=x rsi=y -> rax=whnf(f x y)
#
# apply2.whnf if rax is know to be in WHNF

apply2:
	ERAX	rdi, rsi
apply2.whnf:
	jinat	rax, mkclz2
	_jiclz	rax, apply2.fallback
apply2.pinlaw:
	ptr_	r8, rax
	mov	r9,  [r8+32] # arity
	mov	r10, [r8+24] # exec
	cmp	r9, 2
	ja	mkclz2
	jb	apply2.over1
	jmp	r10
apply2.over1:
	ppush	rsi         # Save extra argument
	call	r10         # fn = operator(fn, arg1)
	ppop	rdi         # restore to arg1
	jmp	apply1.whnf # apply extra arg (fn always in WHNF).
apply2.fallback:
	ppush	rax, rdi, rsi
	mov	rdi, 2
	jmp	apply

### rdi: memory location of table
### rsi: Val
### rdx: data
c_ht_put:
	push	r12
	mov	r12, rdi
	mov	rdi, [rdi]
	call	ht_put
	mov	[r12], rdi
	pop	r12
	ret

### rdi: memory location of table
### rsi: table location
c_ht_get:
	mov	rdi, [rdi]
	htval	rdi, rsi, rax
	ret

### Implementation detail of mkpin; note non-standard calling convention
###
### rdi[in/out]: pin_table pointer
### rsi[clobber]: item
### rdx[in/out]: total plan size
### rcx[in/out]: pin count
###
### Clobbers:
### - r9: general temporary
### - r10: i loop counter

### Recursively traverses memory, building a hash table which maps each tagged
### pointer to a value. That value is either its integer position in the pin
### table if it's a pin, or the wordsize offset to copy the value to in the
### output pin.

assign_space:				    # assign_space(rdi, rsi, rdx, rcx):
	jdirect	rsi, assign_space.ret	    #   if direct, do nothing
	push	rcx			    #   save rcx
	call	ht_has			    #   rax = ht_has(*pin_table, item);
	pop	rcx			    #   retore rcx
	cmp	rax, -1			    #   if a valid index
	jne	assign_space.ret	    #   then already exists, skip out
	tytag_	rax, rsi		    #   rax = typetag(item)
	jmp	[assign_space.types+rax*8]  #   jump based on tag
assign_space.types:			    # indirect jump table:
	.quad	assign_space.thk	    #
	.quad	assign_space.clz	    #
	.quad	assign_space.law	    #
	.quad	assign_space.pin	    #
	.quad	assign_space.nat	    #
assign_space.thk:			    # thunk handler:
	ud2				    #   thunks should be impossible
assign_space.clz:			    # closure handler:
assign_space.law:			    # law handler:
	push	rcx			    #   save rcx
	call	ht_put			    #   ht_put(pintbl, item, plan_size)
	pop	rcx			    #   restore rcx
	wordsz_	rax, rsi		    #   rax = wordsize(item)
	mov	r9, rax			    #   num_items_to_cpy = wordsize
	inc	rax			    #   rax++ for header item
	add	rdx, rax		    #   *plansz += full wordsize
	ptr	rsi			    #   rsi = PTR(rsi)
	xor	r10, r10		    #   i = 0
assign_space.clzloop:			    # closure and law loop:
	cmp	r10, r9			    #   if (i < wordsize)
	jae	assign_space.ret	    #   then we're done
	ppush	rsi, r9, r10		    #   save registers
 	mov	rsi, [rsi + 8*r10]	    #   item = ith item
	call	assign_space		    #   recur for this item
	ppop	rsi, r9, r10		    #   restore registers
	inc	r10			    #   i++
	jmp	assign_space.clzloop	    #   loop
assign_space.pin:			    # pin:
	push	rdx			    #   save total plan size
	push	rcx			    #   save total pin size
	mov	rdx, rcx		    #   set total pin size in ht
	call	ht_put			    #   ht_put(pintbl, item, pin_count)
	pop	rcx			    #   restore total pin size
	pop	rdx			    #   restore total plan size
	inc	rcx			    #   pin_count++
	ret				    #   return
assign_space.nat:			    # nat:
	push	rcx			    #   save total pin size
	call 	ht_put			    #   ht_put(pintbl, item, plan_size)
	pop	rcx			    #   restore total pin size
	wordsz_	rax, rsi		    #   rax = wordsize(item)
	inc	rax			    #   rax++ for header item
	add	rdx, rax		    #   *plansz += full wordsize
assign_space.ret:			    # ret:
	ret				    #   return

### mkpin : rdi/Item -> rax/Pin (Clobbers everything)
###
### Uses preserved registers:
### - r12: [as temporary for different things]
### - r13: always total pin count
### - r14: always hash table register
###
### TODO: This never invokes GC on the pinheap. Once we have a GC process for
### the pinheap, we need to pause execution to collect whenever we
### buddy_allocate and fail.
.global MkPin
oppin:
MkPin:
	FORCE	rdi
mkpin:					    # mkpin(rdi=Item): rax/Pin
	jipin	rdi, mkmegapin		    #   if pin, make megapin
	push	rbp			    #   start real frame
	mov	rbp, rsp		    #   frame setup
	sub	rsp, 32			    #   four u64 words
	mov	[rsp + 0], rdi		    #   [0] = save FORCEd rdi
	mov	[rsp + 8], r12		    #   [8] = save r12
	mov	[rsp + 16], r13		    #   [16] = save r13
	mov	[rsp + 24], r14		    #   [24] = save r14
	ppush	rdi			    #   save item to stack

	mov	rdi, 5			    #   2^5 sized
	call	ht_create		    #   rax = ht_create(5)

	mov	rdi, rax
	mov	rsi, [rsp]
	xor	rdx, rdx
	xor	rcx, rcx
	call	assign_space	# nonstandard calling convention

mkpin.malloc:
	push	rcx			    # save pin count (will be r13)
	push	rdi			    # save pin table (will be r14)

	add	rdx, rcx	# plansz/r12 + pin_count/r13
	add	rdx, 6		# 6 payload words
	mov	rdi, rdx
	mov	rsi, 0x001	# tag=Pin
	call	buddy_malloc
	test	rax, rax
	jz	mkpin.buddyoom

	pop	r14			    # restore pin table (was rdi)
	pop	r13			    # restore pin count (was rcx)
	## TODO: We've temporarily gotten r13 and r14 back to the normal global
	## registers for all calls to buddy_malloc during mkpin proper. In the
	## future, clean up the rest of mkpin, skimming there's a lot of low
	## hnaging fruit here.

	## If we're a direct item, we don't have to copy or relocate the
	## pointer. Just jump to the end.
	mov	rdi, [rsp]
	jdirect	rdi, mkpin.item_relocated

	## Inside the loop, rsi is your free variable for doing things.
	##
	## rcx: key
	## rdx: val
	## r9: cur
	## r10: plan_table_size
	## r11: i
	## r12: immediate local temporary; usually hash table or type tag.
	mov	r9, rax
	add	r9, 48		# cur = ptr + 6(u64 words)

	mov	r10, [r14 + 8]
	inc	r10		# sz = plan_table->mask + 1

	xor	r11, r11
mkpin.copyloop:
	cmp	r11, r10
	jnb	mkpin.aftercopy

	htkey	r14, r11, rcx	# rcx = hash_table/r14->tbl[r11].key

	test	rcx, rcx
	jz	mkpin.copycontinue # if key is null, skip

	htval	r14, r11, rdx	# rdx = hash_table/r14->tbl[r11].val

	tytag_	r12, rcx
	jmp	[mkpin.types + r12*8]
mkpin.types:
	.quad	mkpin.copythk, mkpin.copyclz, mkpin.copyclz
	.quad	mkpin.copypin, mkpin.copynat
mkpin.copythk:
mkpin.copyclz:
	##  this is the complicated one. copyclz copies the first header byte
	##  verbatim, but then has to reinterpret every subsequent byte as a
	##  pointer that has to be relocated.
	push	r9
	push	r10
	push	r11

	ptr_	rsi, rcx	# src = PTR(item)
	mov	r11, rcx	# r11/item = rcx/item [shuffle; we reuse rcx]

	mov	rcx, r13	# pin count
	add	rcx, rdx	# val
	shl	rcx, 3
	add	rcx, r9		# dst = (u64*)ptr + 6 + pin_count + val

	mov	rdx, [rsi - 8]	# header = *(src - 1)
	mov	[rcx], rdx	# dst[0] = header
	add	rcx, 8		# dst++

	wordsz_ rdx, r11	# rdx = wordsize(r11/item)

	xor	r9, r9		# r9 = inner loop counter
mkpin.copyclz.loop:
	cmp	r9, rdx
	jnb	mkpin.copyclz.complete

	mov	r10, [rsi]	# tp = *src
	## If this is a direct nat, we can just copy it
	jdirect	r10, mkpin.copyclz.loop.simple

	## If this is a pin, we also can just copy the pin reference
	mov	r11, r10
	shr	r11, 56
	cmp	r11, 0xC4
	je	mkpin.copyclz.loop.simple

	## This is a closure or a law and we have to reinterpret it by
	## offset from the plan table.
	push	rax
	push	rsi
	push	rcx
	push	r8
	mov	rdi, r14
	mov	rsi, r10
	call	ht_has		# ht_has(plan_table/rdi, key/r10)
	htval	rdi, rax, r11	# offset/r11 = hash->tbl[rax].val
	pop	r8
	pop	rcx
	pop	rsi
	pop	rax

	inc	r11		# offset++
	add	r11, r13	# offset pin count words
	shl	r11, 3		# wordsize *8
	add	r11, rax	# ptr
	add	r11, 48		# 6(u64 words)
	## newptr/r11 = (u64*)cur + offset + 1

	shr	r10, 48
	shl	r10, 48
	or	r10, r11	# ((tp >> 48) << 48) | (newptr)
	mov	[rcx], r10

	jmp mkpin.copyclz.loop.continue
mkpin.copyclz.loop.simple:
	mov	[rcx], r10	# *dst = tp
	## fallthrough
mkpin.copyclz.loop.continue:
	inc	r9		# i++
	add	rcx, 8		# dst++
	add	rsi, 8		# src++
	jmp	mkpin.copyclz.loop

mkpin.copyclz.complete:
	pop	r11
	pop	r10
	pop	r9
	jmp	mkpin.copycontinue

mkpin.copypin:
	shl	rdx, 3		# convert to bytes
	lea	rdx, [r9 + rdx]	# cur[val]/rdx
	mov	[rdx], rcx	# cur[val] = keyitem
	jmp	mkpin.copycontinue

mkpin.copynat:
	ptr_	rsi, rcx
	sub	rsi, 8		# source = PTR(keyitem) - 1

	wordsz	rcx
	inc	rcx		# natsize with header

	mov	rdi, rdx	# rdi = val
	add	rdi, r13	# rdi += pin_count
	shl	rdi, 3		# rdi: bytes to words
	add	rdi, r9		# rdi += cur pointer
	rep	movsq
	##  fallthrough
mkpin.copycontinue:
	inc	r11
	jmp	mkpin.copyloop
mkpin.aftercopy:
	mov	rdi, [rsp]
	shr	rdi, 48
	shl	rdi, 48		# remove previous pointer, keeping tag

	mov	rdx, r13	# pin count
	add	rdx, 7		# 6 words of pin header, 1 word of data header
	shl	rdx, 3		# convert to bytes
	add	rdx, rax	# first_item = (u64*)ptr + 6 + pin_count + 1
	or	rdi, rdx

mkpin.item_relocated:
	call	fill_pin_header

	push	rax
	mov	rdi, r14
	call	buddy_free
	pop	rax

	ppop	rdi			    #   restore rdi item
	mov	r12, [rsp + 8]		    #   restore r12
	mov	r13, [rsp + 16]		    #   restore r13
	mov	r14, [rsp + 24]		    #   restore r14
	leave
	ret
mkpin.buddyoom:
	ud2

RequestGC2Op:
	push rdi
	push rsi
	push rdx
	push rcx
	push r8
	push r9
	push r10
	push r11
	push r12
	call request_gc2
	pop r12
	pop r11
	pop r10
	pop r9
	pop r8
	pop rcx
	pop rdx
	pop rsi
	pop rdi
	ret


### Megapins have to be interpretable as pins. That means they have to have the
### same first header slots as pins, but then any additional information has to
### come afterwards.
###
### Megapins expand the information in a pin so that the list of pins is
### actually all reachable pins up against the next megapin boundary, the
### megapins, and also the length of the pin and megapin segments so they don't
### need to be dereferenced during garbage collection.

### A normal pin is 0xC400.
### A megapin is 0xC401.
### A normal pin with crc32 information is 0xC402.
### A megapin with crc32 information is 0xC403.

### Standard pin:
###     [ptr -  8]: header
###     [ptr     ]: pinned pin pointer
###     [ptr +  8]: 0 (hash)
###     [ptr + 16]: pin_count
###     [ptr + 24]: "func"
###     [ptr + 32]: pinarity
###     [ptr + 40]: fp_data
###     [ptr + 48]: pin[0]
###                 ...
###     ~~~~~~~ megapin additions ~~~~~~~
###                 pin_length[0]
###                 ...
###                 megapin_count
###                 megapin[0]
###                 ...
###                 megapin_length[0]
###     ~~~~~~~~ crc32 additions ~~~~~~~~
###                 pin_crc32[0]
###                 ...
###                 megapin_crc32[0] (if applicable)
###                 ...

.macro jimpin reg, label, tmp	# Jump if megapin
	mov	\tmp, \reg
	bt	\tmp, 48
	jnc	jimpinno\@
	shr	\tmp, 56
	cmp	\tmp, 0xC4
	je	\label
jimpinno\@:
.endm

// C export
ismegapin:
	jimpin	rdi, ismegapin.yes, tmp=rax
ismegapin.no:
	mov	rax, 0
	ret
ismegapin.yes:
	mov	rax, 1
	ret

### rdi[in/out]: table
### rsi: item
### rdx[in/out]: megapin count
### rcx[in/out]: pin count
mega_assign_space:
	push	rcx
	call	ht_has		# rax = ht_has(*pin_table, item);
	pop	rcx
	cmp	rax, -1
	jne	mega_assign_space.ret # if already exists, skip put.

	jimpin	rsi, mega_assign_space.mega_pin, tmp=rax
mega_assign_space.normal_pin:
	push	rcx
	push	rdx
	mov	rdx, rcx
	call	ht_put		# ht_put(table, item, pin_count);
	pop	rdx
	pop	rcx

	inc	rcx		# pin_count++

	xor	r10, r10	# i/r10 = 0
	ptr	rsi		# ptr/rsi = PTR(item/rsi)
	mov	r11, [rsi + 16]	# size/r11 = ptr/rsi[2]
mega_assign_space.normal_pin.loop:
	cmp	r10, r11
	jnb	mega_assign_space.ret

	push	rsi
	push	r10
	push	r11
	mov	rsi, [rsi + 48 + r10*8] # ptr[6 + i]
	call	mega_assign_space
	pop	r11
	pop	r10
	pop	rsi

	inc	r10
	jmp	mega_assign_space.normal_pin.loop

mega_assign_space.mega_pin:
	push	rcx
	call	ht_put		# ht_put(table, item, rdx/megapin_count);
	pop	rcx
	inc	rdx		# megapin_count++
mega_assign_space.ret:
	ret

### Implementation detail of mkpin when the input is a pin.
### rdi was assumed to be pre-forced in mkpin, which should be the only thing
### that calls this.
mkmegapin:
	push	rbp			    #   start real frame
	mov	rbp, rsp		    #   frame setup
	add	rsp, -32		    #   four u64 words
	mov	[rsp], rdi		    #   [0] = save FORCEd rdi
	mov	[rsp - 8], r12		    #   [-8] = save r12
	mov	[rsp - 16], r13		    #   [-16] = save r13
	mov	[rsp - 24], r14		    #   [-24] = save r14
	ppush	rdi			    #   save item to stack for liveness

	mov	rdi, 5
	call	ht_create

	mov	rdi, rax
	mov	rsi, [rsp]
	xor	rdx, rdx
	xor	rcx, rcx
	call	mega_assign_space # nonstandard calling convention

mkmegapin.malloc:
	push	rdx			    # save megapin cnt (will be r12)
	push	rcx			    # save pin count (will be r13)
	push	rdi			    # save table (will be r14)

	mov	rdi, rdx
	add	rdi, rcx
	shl	rdi, 1		# sz = (pin_count + megapin_count) * 2
	add	rdi, 7		# sz += 6 pin header + 1 megapin header
	mov	rsi, 0x006	# tag=Megapin
	call	buddy_malloc

	## TODO: There's still low hanging fruit for rewriting mkmegapin to be
	## more efficient and in a better modern style, but for now, we want to
	## see if getting r12-r14 overwritting out of the buddy_malloc path is
	## enough to make concurrent gc2 work.
	pop	r14			    # restore table (was rdi)
	pop	r13			    # restore pin count (was rcx)
	pop	r12			    # restore megapin count (was rdx)

	test	rax, rax
	jz	mkmegapin.oom

	mov	r9, rax
	add	r9, 48		# cur = ptr[6]
	mov	r10, [r14 + 8]
	inc	r10		# sz = table->mask + 1
	xor	r11, r11

	push	rax

	## r9: pointer right after header
	## r10: table_size
	## r11: i
mkmegapin.copyloop:
	## rdi: current tbl[r11].key
	## rsi: current tbl[r11].val
	## rdx: pointer to where to write pin pointer
	## rcx: pointer to where to write pin length

	cmp	r11, r10
	jnb	mkmegapin.aftercopy

	htkey	r14, r11, rdi	# rdi = hash_table/r14->tbl[r11].key

	test	rdi, rdi
	jz	mkmegapin.copycontinue # if key is null, skip

	htval	r14, r11, rsi	# rsi = hash_table/r14->tbl[r11].val

	jimpin	rdi, mkmegapin.copyloop.usemegapin, tmp=rax
mkmegapin.copyloop.usepin:
	mov	rdx, rsi
	shl	rdx, 3
	add	rdx, r9		# rdx = ptr to slot if it was a pin

	mov	rcx, r13
	shl	rcx, 3
	add	rcx, rdx	# rcx = ptr to pin length slot
	jmp	mkmegapin.copyloop.setslot
mkmegapin.copyloop.usemegapin:
	mov	rdx, r13
	shl	rdx, 4		# pin count to number of words * 2
	add	rdx, 8		# add one word for the megapin header
	add	rdx, r9

	mov	rcx, rsi
	shl	rcx, 3		# convert words to bytes
	add	rdx, rcx	# add current megapin offset in bytes

	mov	rcx, r12
	shl	rcx, 3
	add	rcx, rdx	# rcx = ptr to megapin length slot
mkmegapin.copyloop.setslot:
	mov	[rdx], rdi	# write item to designated slot
	wordsz	rdi
	mov	[rcx], rdi	# write length to designated slot
mkmegapin.copycontinue:
	inc	r11		# i++
	jmp	mkmegapin.copyloop
mkmegapin.aftercopy:
	pop	rax		# restore rax = malloced pointer
	mov	rdi, [rsp]	# rdi = input pin from stack

	mov	r11, r12        # avoid clobbering r12
	call	fill_pin_header
	mov	r12, r11        # restore r12

	bts	rax, 48		# set megapin bit in tagged pointer

	mov	rdi, r13
	shl	rdi, 4
	mov	[r9 + rdi], r12 # set the megapin count in the payload header

	push	rax
	mov	rdi, r14
	call	buddy_free
	pop	rax

	ppop	rdi			    #   remove stack item
	mov	r12, [rsp - 8]		    #   restore r12
	mov	r13, [rsp - 16]		    #   restore r13
	mov	r14, [rsp - 24]		    #   restore r14
	leave
	ret
mkmegapin.oom:
	ud2

### item/rdi, ptr/rax, pin_count/r13 {rsi, rcx, r8, r12}
fill_pin_header:
	mov	[rax], rdi		# ptr[0] = relocated_item
	mov	qword ptr [rax + 8], 0	# ptr[1] = 0; // hash (todo: remove?)
	mov	[rax + 16], r13		# ptr[2] = pin_count

	push	rax
	push	rdi
	## rdi already set
	call	pinarity
	mov	rsi, rax
	pop	rdi
	pop	rax
	mov	[rax + 32], rsi	# ptr[4] = pinarity(relocated_item)

	push	rax
	call	jet
	xor	r8d, r8d
	mov	rcx, rax
	pop	rax
	mov	[rax + 24], rcx	# ptr[3] = "func"
	mov	[rax + 40], r8	# ptr[5] = fp_data

	## rax still points to the allocation from the buddy_malloc above
	mov	r12, 0xC400000000000000   # r12 = mask
	or	rax, r12                  # rax/result = ptr | mask

	ret


.global getenveq
getenveq:				    # getenveq(rdi=char*):
	push	rbx			    #   save rbx
	push	r12			    #   save r12
	push	r13			    #   save r13
	mov	r12, rdi		    #   r12 = key ("NAME=")
	mov	r13, [envp]		    #   r13 = &envp[0] (char**)
getenveq.next_env:			    # next_env:
	mov	rbx, [r13]		    #   rbx = envp[i]
	test	rbx, rbx		    #   if we hit the null terminator
	jz	getenveq.not_found	    #   then nothing
	xor	rcx, rcx		    #   rcx = j = 0
getenveq.cmp_loop:			    # cmp_loop:
	mov	al, [r12 + rcx]		    #   al = key[j]
	cmp	al, 0			    #   if key[j] == null
	je	getenveq.match		    #   then matched on end of "NAME="
	mov	dl, [rbx + rcx]		    #   dl = env[j]
	cmp	al, dl			    #   if key[j] != env[j]
	jne	getenveq.advance	    #   then key mismatch, next key
	inc	rcx			    #   j++
	jmp	getenveq.cmp_loop	    #   loop character check
getenveq.advance:			    # advance:
	add	r13, 8			    #   ++envp
	jmp	getenveq.next_env	    #   try next environment var
getenveq.match:				    # match:
	lea	rax, [rbx + rcx]	    #   rax is the value after "="
	jmp	getenveq.done		    #   goto done
getenveq.not_found:			    # not_found:
	xor	rax, rax		    #   return NULL
getenveq.done:				    # done:
	pop	r13			    #   restore r13
	pop	r12			    #   restore r12
	pop	rbx			    #   restore rbx
	ret				    #   return

.global sharedbegin
sharedbegin:				    # sharedbegin(rsp=base frame):
	mov	rax, [rsp]		    #   rax = stack argc val
	mov	[argc], rax		    #   store argc
	lea	rbx, [rsp + 8]		    #   rbx = stack &argv[0]
	mov	[argv], rbx		    #   store argv
	mov	rcx, rax		    #   rcx = argc
	lea	rdx, [rbx + rcx*8 + 8]	    #   rdx = stack &argv[argc] + 1
	mov	[envp], rdx		    #   set envp
	mov	rsi, rdx		    #   rsi = p = envp
sharedbegin.find_env_end:		    # find_env_end:
	cmp	qword ptr [rsi], 0	    #   if envp[i] == 0
	jz	sharedbegin.found_end	    #   then found end of envp
	add	rsi, 8			    #   next pointer
	jmp	sharedbegin.find_env_end    #   loop
sharedbegin.found_end:			    # found_end:
	add	rsi, 8			    #   skip the final envp null
	mov	[auxp], rsi		    #   auxp = next pointer
sharedbegin.initialize:			    # initialize:
	mov	rdi, 9			    #   min_alloc_log2=9 (512 bytes)
	mov	rsi, 34			    #   max_alloc_log2=34 (16 gigabytes)
	call	init_buddy
	call	build_heap_table
	call	mapheap
	call	mapstack
	mov	rdi, rax
	mov	rsi, 0x100000000
	push	rdi
	push	rsi
	call	initialize_stack_globals
	pop	rsi
	pop	rdi
	call	madvise_dontdump_stack
	pop	rdi
	mov	rsi, rsp
	mov	rax, [chosen_main]
	call	rax
	mov	rdi, rax
	jmp	syscall_exit_group

# plan_start is the entry-point for the plan executable, which just runs
# some PLAN code and doesn't start a database.

.global plan_start
plan_start:
	lea	rax, xplan_main
	mov	[chosen_main], rax
	jmp	sharedbegin

# rpn_start is the entry-point for a testing executable, which either runs
# unit-tests.  It also doesn't start the in-exe database.

.global rpn_start
rpn_start:
	lea	rax, rpn_main
	mov	[chosen_main], rax
	jmp	sharedbegin
