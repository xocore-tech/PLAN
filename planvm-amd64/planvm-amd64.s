.global zzcal2
zzcal2:	jmp zcall2

.global zcall0, zcall1, zcall2, zcall3, zcall4, zcall5

zcall0: xor	rsi, rsi
zcall1: xor	rdx, rdx
zcall2: xor	rcx, rcx
zcall3: xor	r8d, r8d
zcall4: xor	r9d, r9d
zcall5:	sub	rsp, 32
	mov	[rsp], r12                  # save r12, rbx, rbp
	mov	[rsp+8], rbx
	mov	[rsp+16], rbp
	mov	rax, rdi                    # self=arg1 (function pointer)
	mov	rdi, rsi                    # arg1=arg2
	mov	rsi, rdx                    # arg2=arg3
	mov	rdx, rcx                    # arg3=arg4
	mov	rcx, r8                     # arg4=arg5
	mov	r8, r9                      # arg5=arg6
	xor	r9d, r9d                    # clear r9
	xor	r10d, r10d                  # clear r10
	xor	r11d, r11d                  # clear r11
	xor	r12d, r12d                  # clear r12
	xor	ebx, ebx                    # clear rbx
	xor	ebp, ebp                    # clear rbp
	call	rax                         # Call into asm routine
	mov	r12, [rsp]
	mov	rbx, [rsp+8]
	mov	rbp, [rsp+16]               # restore r12, rbx, rbp
	add	rsp, 32
	ret
.macro pdrop n
	.set bytes, 8 * \n
	add	r15, bytes
.endm
.macro ppush regs:vararg
	.ifnc "\regs", ""

	.set n\@, 0
	.irp r, \regs
	.set n\@, n\@ + 1
	.endr

	.set total, 8 * n\@

	sub     r15, total

	.set offset\@, 0
	.irp r, \regs
		mov	qword ptr [r15 + offset\@], \r
		.set offset\@, offset\@ + 8
	.endr
	.endif
.endm
.macro ppop regs:vararg
	.ifnc "\regs", ""

	.set n\@, 0
	.irp r, \regs
	.set n\@, n\@ + 1
	.endr

	.set total, 8 * n\@

	.set offset\@, 0
	.irp r, \regs
		mov	\r, qword ptr [r15 + offset\@]
		.set offset\@, offset\@ + 8
	.endr

	add     r15, total

	.endif
.endm
.macro pop_r8_to_rdi
	pop	r8
	pop	rcx
	pop	rdx
	pop	rsi
	pop	rdi
.endm
.macro push_rdi_to_r8
	push	rdi
	push	rsi
	push	rdx
	push	rcx
	push	r8
.endm
.macro sub0 a, b
	cmp	\a, \b
	cmovb	\a, \b
	sub	\a, \b
.endm
.macro cap r, max
	cmp	\r, \max
	cmova	\r, \max
.endm
.macro jzero reg, label
	test	\reg, \reg
	jz	\label
.endm
.macro btw r1                               # bits_to_words(x) = (x+63)>>6
	add	\r1, 63
	shr	\r1, 6
.endm
.macro swapq r1, r2, m1, m2
	mov	\r1, \m1
	mov	\r2, \m2
	mov	\m1, \r2
	mov	\m2, \r1
.endm
.macro swapstk r1, r2
	swapq	\r1, \r2, [r15], [r15+8]
.endm
.macro ptr r                                # r = PTR(r)
	shl	\r, 16                      # lsh clears high bits
	shr	\r, 16                      # rsh recovers original shape
.endm

.macro ptr_ r, from                         # r = FROM(from)
	mov	\r, 48
	bzhi	\r, \from, \r		    # mask the bottom 48 bits
.endm

.macro dref r                               # r = *PTR(r)
	ptr	\r
	mov	\r, [\r]
.endm

.macro dref_ r from                         # r = *PTR(from)
	mov	\r, \from
	dref	\r
.endm

.macro refo r, o                           # r = PTR(r)[i]
	ptr	\r
	lea	\r, [\r + \o]
.endm

.macro refo_ r, src, o                     # r = PTR(src)[i]
	mov	\r, \src
	refo	\r, \o
.endm

.macro drefo r, o                           # r = PTR(r)[i]
	ptr	\r
	mov	\r, [\r + \o]
.endm

.macro drefo_ r, src, o                     # r = PTR(src)[i]
	mov	\r, \src
	drefo	\r, \o
.endm

.macro clzix_ r, src, i
	.set	ix, 8+(\i * 8)
	drefo_	\r, \src, ix
.endm

.macro drefi r, i                           # r = PTR(r)[i*8]
	ptr	\r
	mov	\r, [\r + 8 * \i]
.endm

.macro drefi_ r, src, i                     # r = PTR(str)[i*i]
	mov	\r, \src
	ptr	\r
	mov	\r, [\r + \i * 8]
.endm
.macro tytag r                            # tytag(r):
	shl	\r, 2                     #   r <<= 2
	lzcnt	\r, \r                    #   r = clz(r)
.endm
.macro tytag_ out, from                   # tytag_(out,from):
	mov	\out, \from               #   out = inp
	tytag	\out                      #   out = tytag(out)
.endm
.macro jdirect reg, label                   # jump if direct
	test	\reg, \reg                  # examine register
	jns	\label                      # not signed == high bit clear
.endm
.macro jheap reg, label                     # jump if indirect
	test	\reg, \reg                  # examine register
	js	\label                      # signed == high bit set
.endm
.macro jinat reg, lab                       # jump if nat
	rorx	r12, \reg, 56               # rorx to get high byte
	cmp	r12b, 0xC4                  # smaller than a pin?
	jb	\lab                        # then jump to the label
.endm
.macro jnnat reg, lab                       # jump if not nat
	rorx	r12, \reg, 56               # rorx to get high byte
	cmp	r12b, 0xC4                  # big enough to be a pin?
	jae	\lab                        # then jump to the label
.endm
.macro ncast reg                            # cast to nat
	xor	r11d, r11d                  # r11=0
	rorx	r12, \reg, 56               # r12 = (reg>>56)
	cmp	r12b, 0xC4                  # if (r12 >= 0xC4)
	cmovae	\reg, r11                   #   reg=0
.endm
.macro jithk reg, lab
	rorx	r12, \reg, 56
	cmp	r12b, 0xE0
	jae	\lab
.endm
.macro jnthk reg, lab
	rorx	r12, \reg, 56
	cmp	r12b, 0xE0
	jb	\lab
.endm
.macro jiclzt reg, lab                      # Jump if closure or thunk
	rorx	r12, \reg, 56
	cmp	r12b, 0xD0
	jae	\lab
.endm
.macro jnclzt reg, lab
	rorx	r12, \reg, 56
	cmp	r12b, 0xD0
	jb	\lab
.endm
.macro jiclz reg, lab                       # Jump if closure.
	rorx	r12, \reg, 56
	cmp	r12b, 0xD0
	je	\lab
.endm
.macro jnclz reg, lab                       # Jump if not closure.
	rorx	r12, \reg, 56
	cmp	r12b, 0xD0
	jne	\lab
.endm
.macro jipin reg, lab                       # Jump if closure.
	rorx	r12, \reg, 56
	cmp	r12b, 0xC4
	je	\lab
.endm
.macro jnpin reg, lab                       # Jump if not closure.
	rorx	r12, \reg, 56
	cmp	r12b, 0xC4
	jne	\lab
.endm
.macro jilaw reg, lab                       # Jump if closure.
	rorx	r12, \reg, 56
	cmp	r12b, 0xC8
	je	\lab
.endm
.macro jnlaw reg, lab                       # Jump if not closure.
	rorx	r12, \reg, 56
	cmp	r12b, 0xC8
	jne	\lab
.endm
.macro _jiclz reg, lab # Jump if closure (assuming it is not a nat)
	bt	\reg, 60
	jc	\lab
.endm

.macro _jilaw reg, lab # Jump if law (assuming it is not a nat)
	bt	\reg, 59
	jc	\lab
.endm

.macro _jnlaw reg, lab # Jump if not law (assuming it is not a nat)
	bt	\reg, 59
	jnc	\lab
.endm
.macro jnkal reg, lab                       # Jump if not closure.
	mov	r12, \reg
	shr	r12, 48
	cmp	r12w, 0xD002
	jne	\lab
.endm
.macro jnquo reg, lab                       # Jump if not closure.
	mov	r12, \reg
	shr	r12, 48
	cmp	r12w, 0xD001
	jne	\lab
.endm
assert_fail:
	ud2
.macro ablptr reg                             # assert buddy list pointer
	cmp	\reg, buddy_mmap_ptr
	jb	assert_fail
	cmp	\reg, base_end_ptr
	jae	assert_fail
.endm
.macro nobeef reg
	movabs	r12, 0xBADBEEFBADBEEF
	cmp	\reg, r12
	je	assert_fail
.endm
.macro getgc r                              # gcheader(r) = PTR(r)[-8]
	ptr	\r
	mov	\r, [\r - 8]
.endm

.macro gcsz hdr                             # Size of allocation box
	add	\hdr, 16128
	shr	\hdr, 14
.endm

.macro gcsz_ reg, hdr
	mov	\reg, \hdr
	gcsz	\reg
.endm

.macro gcbits hdr
	shr	\hdr, 8
.endm
.macro bitsiz r
	getgc	\r
	gcbits	\r
.endm

.macro bitsiz_ r, from
	mov	\r, \from
	bitsiz	\r
.endm

.macro bytsiz r
	bitsiz	\r
	add	\r, 7
	shr	\r, 3
.endm

.macro bytsiz_ r, from
	mov	\r, \from
	bytsiz	\r
.endm

.macro dbitsz r
	xchg	\r, r12
	mov	\r, 64
	lzcnt	r12, r12
	sub	\r, r12
.endm

.macro dbitsz_ r, from
	lzcnt	r12, \from
	mov	\r, 64
	sub	\r, r12
.endm

.macro dbytsz_ r, from
	dbitsz_	\r, \from
	add	\r, 7
	shr	\r, 3
.endm

.macro wordsz r
	getgc	\r
	gcsz	\r
.endm

.macro wordsz_ r, from
	mov	\r, \from
	wordsz	\r
.endm

.macro slots_ r, from
	mov	\r, \from
	slots	\r
.endm

.macro slots r                              # wordsize, assuming not a nat
	getgc	\r
	shr	\r, 14
.endm

.macro clzsz r                              # Sz(x) = slots(x)-1
	slots	\r
	dec	\r
.endm

.macro clzsz_ r, from
	mov	\r, \from
	clzsz	\r
.endm

.macro clzhd_ r, from
	dref_	\r, \from
.endm

.macro ix0 r
	drefo	\r, 8
.endm

.macro ix0_ r, from
	drefo_	\r, \from, 8
.endm

.macro ix1 r
	drefo	\r, 16
.endm
.macro ix1_ r, from
	drefo_	\r, \from, 16
.endm
.macro unpin r
	dref	\r
.endm
.macro unpin_ r, from
	dref_	\r, \from
.endm
.macro name r
	dref	\r
.endm
.macro name_ r, from
	dref_	\r, \from
.endm
.macro arity r
	drefo	\r, 8
.endm
.macro arity_ r, from
	drefo_	\r, \from, 8
.endm
.macro body r
	drefo	\r, 16
.endm
.macro body_ r, from
	drefo_	\r, \from, 16
.endm
.global mkbuffer
mkbuffer:                                   # mkbuffer: (rdi=size)
	mov	r12, rdi                    #   r12=bytes
	mov	rbx, 8                      #
	cmp	rdi, rbx                    #   size = max(size, 8)
	cmovb	rdi, rbx                    #
	shr	rdi, 3                      #   words = bytes/8
	inc	rdi                         #   words++
	mov	rbx, rdi                    #   rbx=words
	xor	esi, esi                    #   tag=0
	call	buddy_malloc                #   buddy_malloc() (rax=ptr)
	mov	rsi, rax                    #   copy pointer to rsi
	mov	rcx, rbx                    #   count=words
	mov	rdi, rax                    #   dst=ptr
	xor	eax, eax                    #   value=0
	rep	stosq                       #   memcopy
	mov	byte ptr [rsi+r12], 1       #   set high byte
	mov	rax, 0x8200000000000000     #   result=NATMASK
	or	rax, rsi                    #   result |= ptr
	shl	r12, 3                      #
	inc	r12                         #   bits = (bytes*8)+1
	shl	r12, 8                      #   hdr = (bits, tag=0)
	mov	[rsi-8], r12                #   replace header w/ correct sz
	ret                                 #   return result
mkword:                                     # mkword: (rax=word)
	test	rax, rax                    #   if high bit is set
	js	mkword64                    #     tailcall mkword64(word)
	ret                                 #   return
mkword64:                                   # mkword64: (rax=word)
	lea	r8, [r14+16]                #   tmp = hp+2
	cmp	r8, r13                     #   if (tmp > heapend)
	ja	mkword64.gc                 #     goto gc
	xchg	r8, r14                     #   ptr=hp, hp=tmp
	mov	qword ptr [r8], 0x4000      #   gc record
	add	r8, 8                       #   ptr++
	mov	[r8], rax                   #   *ptr = word
	mov	rax, 0x8200000000000000     #   result = mask
	or	rax, r8                     #   result |= word
	ret                                 #   return result
mkword64.gc:                                # gc:
	not	rax                         #   hide 64bit word via negation
	ppush	rdi                         #   save rdi
	mov	rdi, 2                      #   need two words
	call	gc                          #   gc()
	ppop	rdi                         #   restore rdi
	not	rax                         #   restore 64bit word
	jmp	mkword64                    #   try again
mkdouble: # rax=lo, rdx=hi                  # mkdouble128:
	test	rdx, rdx                    #   if (rdx == 0)
	jz	mkword                      #     return mkword(rax)
mkdouble128:                                # mkdouble128:
	lea	rsi, [r14+8*3]              #
	cmp	rsi, r13                    #   if (heap_next > heap_end)
	ja	mkdouble128.gc              #     then gc
	xchg	rsi, r14                    #   swap (heap_next, rsi)
	mov	[rsi+8], rax                #   rsi[1] = low_word
	mov	[rsi+16], rdx               #   rsi[2] = high_word
	lzcnt	rdx, rdx                    #
	mov	rax, 128                    #
	sub	rax, rdx                    #   rax = 128 - clz(high_word)
	shl	rax, 8                      #
	mov	[rsi], rax                  #   RECORD (t=nat/0 sz=bitsz)
	add	rsi, 8                      #   res = &gc_ptr[1]
	mov	rax, 0x8200000000000000     #
	or	rax, rsi                    #   rax=(res | 0x8200000000000000)
	ret                                 #   return (rax+rdx both safe)
mkdouble128.gc:                             #
	push	rax                         #   Hide 64-bit words on c stack
	push	rdx                         #
	xor	rax, rax                    #
	xor	rdx, rdx                    #
	ppush	rdi                         #
	mov	rdi, 3                      #   needed words
	call	gc                          #
	ppop	rdi                         #
	pop	rdx                         #
	pop	rax                         #
	jmp	mkdouble128                 #
opennat: # rdi=plan value,rsi=buffer	    # openat:
	jdirect	rdi, opennat.direct
opennat.indirect:
	bitsiz_	rax, rdi		    # rax = bitsize(rdi/bitnat)
	ptr_	rdx, rdi		    # rdx = ptr(rdi/ref)
	ret
opennat.direct:
	mov	[rsi], rdi
	mov	rdx, rsi	            # *tmp=word, rdx=temp
	mov	rax, 64         	    # rax=64
	test	rdi, rdi
	jz	opennat.direct_zero_sized
	lzcnt	rcx, rdi
	sub	rax, rcx
opennat.direct_zero_sized:
	ret
unpack:                                     # unpack:
	mov	rdi, [r15+8]                #   arg1
	mov	[rbp-8], rdi                #   rbp[-8] = arg1
	mov	rsi, [r15]                  #   arg2
	mov	[rbp-48], rsi               #   rbp[-48] = arg2
	lea	rsi, [rbp-16]               #
	call	opennat                     #
	mov	[rbp-24], rax               #
	mov	[rbp-32], rdx               #
	btw	rax                         #
	mov	[rbp-40], rax               #
	mov	rdi, [rbp-48]               #
	lea	rsi, [rbp-56]               #
	call	opennat                     #
	mov	[rbp-64], rax               #   rbp[-64] = arg2.bits
	mov	[rbp-72], rdx               #
	btw	rax                         #
	mov	[rbp-80], rax               #   rbp[-80] = arg2.words
	ret
.global ht_create
ht_create:				    # ht_create(rdi=lg):
	mov	rcx, rdi		    #   move to rcx fro shl
	mov	rsi, 1			    #   base 1
	shl	rsi, rcx		    #   size = 1 << lg
	ppush	rdi, rsi, rcx		    #   save registers
	mov	rdi, rsi		    #   bytes = size
	shl	rdi, 1			    #   bytes *= 2 (keys+vals)
	add	rdi, 3			    #   bytes += 3 (header words)
	mov	rsi, 0x008		    #   tag=Hash Table
	call	buddy_malloc		    #   buddy_malloc -> rax=buffer
	ppop	rdi, rsi, rcx		    #   restore registers
	mov	[rax], rdi		    #   h->lg = lg;
	mov	rcx, rsi		    #   mask = power of two size
	dec	rcx			    #   mask-- (now a mask of 1s)
	mov	[rax+8], rcx		    #   h->mask = size - 1
	inc	rcx			    #   restore size
	mov	qword ptr [rax+16], 0	    #   h->count = 0
	mov	rsi, rax		    #   save return value
	lea	rdi, [rsi+24]		    #   rdi = beginning of kv buffer
	shl	rcx, 1			    #   rcx = size * 2
	xor	rax, rax		    #   set value to 0
	rep	stosq			    #   zero out table memory
	mov	rax, rsi		    #   restore return value
	ret				    #   return
.macro htkey base, idx, out
	mov	\out, \idx
	shl	\out, 4		# idx * 16; can't rdx*16 inside mov.
	mov	\out, [\base + \out + 24] # base->tbl[idx].key
.endm
.macro htkpos base, idx, out
	mov	\out, \idx
	shl	\out, 4		# idx * 16; can't rdx*16 inside mov.
	lea	\out, [\base + \out + 24] # base->tbl[idx].key
.endm
.macro htval base, idx, out
	mov	\out, \idx
	shl	\out, 4		# idx * 16; can't rdx*16 inside mov.
	mov	\out, [\base + \out + 32] # base->tbl[idx].val
.endm
.global ht_has
ht_has:					    # ht_has(rdi=ht*,rsi=key):
	mov	rax, -1			    #   initialize hash to -1
	crc32	rax, rsi		    #   crc32 on key value as hash
	mov	r8, [rdi + 8]		    #   r8 = h->mask
	and	rax, r8			    #   idx = hash & h->mask
ht_has.loop:				    # loop:
	htkey	rdi, rax, rcx		    #   rcx = hdi->tbl[rax].key
	test	rcx, rcx		    #   if key is zero
	jz	ht_has.not_found	    #   then item not found
	cmp	rcx, rsi		    #   if key is requested key
	je	ht_has.return		    #   then found and return idx
	inc	rax			    #   idx++
	and	rax, r8			    #   use mask to loop around
	jmp	ht_has.loop		    #   loop
ht_has.not_found:			    # not_found:
	mov	rax, -1			    #   set return value to -1
ht_has.return:				    # return:
	ret				    #   return
ht_put:					    # ht_put(rdi=ht*, rsi=key, rdx=val):
	mov	rax, [rdi + 16]		    #   rax = tbl->count
	inc	rax			    #   add 1 for new count
	cvtsi2sd xmm0, rax		    #   convert to double in xmm0
	mov	rax, [rdi + 8]		    #   rax = tbl->mask
	inc	rax			    #   rax is now capacity
	cvtsi2sd xmm1, rax		    #   convert to double in xmm1
	divsd   xmm0, xmm1                  #   xmm0 = (h->count++)/(h->mask++)
	movsd	xmm1, qword ptr [hash_table_load_factor]
	ucomisd xmm0, xmm1		    #   if percent < load_factr
	jbe     ht_put.has_valid_hash_table #   jump if not greater
	ppush	rdi, rsi, rdx		    #   save registers
	call	ht_resize		    #   resize the table
	ppop	rdi, rsi, rdx		    #   restore registers
	mov	rdi, rax		    #   rdi is now resized table
ht_put.has_valid_hash_table:		    # has_valid_hash_table:
	mov	rax, -1			    #   initialize crc32
	crc32	rax, rsi		    #   hash the key
	mov	r8, [rdi + 8]		    #   load mask
	and	rax, r8			    #   idx = hash & h->mask
ht_put.loop:				    # loop:
	mov	r9, rax			    #   r9 = idx
	shl	r9, 4			    #   r9 = idx * 16
	lea	r9, [rdi + r9 + 24]	    #   r9 = key location
	mov	r10, [r9]		    #   r10 = key value
	test	r10, r10		    #   if key value is null
	jz	ht_put.increment	    #   then increment count and store
	cmp	r10, rsi		    #   if key value is search key
	je	ht_put.store		    #   then store here w/o increment
	inc	rax			    #   idx++
	and	rax, r8			    #   overflow by mask if needed
	jmp	ht_put.loop		    #   goto loop
ht_put.increment:			    # increment:
	mov	r8, [rdi + 16]		    #   get current h->count
	inc	r8			    #   increment
	mov	[rdi + 16], r8		    #   set updated h->count
ht_put.store:				    # store:
	mov	[r9], rsi		    #   current location = key
	mov	[r9 + 8], rdx		    #   current location++ = val
	ret				    #   return
ht_resize:				    # ht_resize(rdi=ht*):
	push	rdi			    #   save input hash table
	mov	rdi, [rdi]		    #   new_lg = ht->lg
	inc	rdi			    #   new_lg++
	call	ht_create		    #   rax = ht_create(new_lg)
	pop	rdi			    #   restore input hash table
	mov	r8, [rax + 8]		    #   newh->mask
	mov	r10, [rdi + 16]		    #   oldh->count
	mov	[rax + 16], r10		    #   newh->count = oldh->count
	mov	rsi, [rdi + 8]		    #   get old mask as terminator
	inc	rsi			    #   rsi = old table size
	mov	rdx, 0			    #   i = 0
ht_resize.outer_loop:			    # loop:
	htkey	rdi, rdx, rcx		    #   rcx = oldh->tbl[i].key
	test	rcx, rcx		    #   if key is zero
	jz	ht_resize.continue	    #   then try next item
	mov	r10, -1			    #   initialize crc32 hashing
	crc32	r10, rcx		    #   hash the key
	and	r10, r8			    #   dst = crc32(0, k) & newh->mask
ht_resize.inner_loop:			    # inner_loop:
	htkey	rax, r10, r11		    #   r11 = newh->tbl[dst].key
	test	r11, r11		    #   if dst's key is zero
	jz	ht_resize.inner_loop_target #   then place item here
	inc	r10			    #   dst++
	and	r10, r8			    #   dst = (dst + 1) & newh->mask
	jmp	ht_resize.inner_loop	    #   retry next slot
ht_resize.inner_loop_target:		    # inner_loop_target:
	htkpos	rax, r10, r11		    #   r11 = &(newh->tbl[dst].key)
	htkpos	rdi, rdx, rcx		    #   rcx = &(oldh->tbl[i].key)
	mov	r10, [rcx]		    #   read key
	mov	[r11], r10		    #   write key
	mov	r10, [rcx + 8]		    #   read value
	mov	[r11 + 8], r10		    #   write value
ht_resize.continue:			    # continue:
	inc	rdx			    #   i++
	cmp	rdx, rsi		    #   if i < size of old table
	jb	ht_resize.outer_loop	    #   then lop
	push	rax			    #   save new table
	call	buddy_free		    #   free old table
	pop	rax			    #   restore new table
	ret				    #   return
.macro EVAL target, tosave:vararg
	mov	rax, \target
	jnthk	rax, 777f
	ppush	\tosave
	dref_	r12, rax
	call	r12
	mov	\target, rax
	ppop	\tosave
777:
.endm
.macro ERAX tosave:vararg
	jnthk	rax, 777f
	ppush	\tosave
	dref_	r12, rax
	call	r12
	ppop	\tosave
777:
.endm
.macro JRAX
	jnthk	rax, 777f
	dref_	r12, rax
	jmp	r12
777:
	ret
.endm
.macro FORCE target, tosave:vararg
	ppush	\tosave
	mov	rax, \target
	call	force.newabi
	mov	\target, rax
	ppop	\tosave
.endm
.macro ENAT reg, tosave:vararg
	EVAL	\reg, \tosave
	ncast	\reg
.endm
xdone:
	drefo	rax, 8
	ret
xhole:
	mov	rdi, 1 # stdout
	lea	rsi, [loopstr]
	mov	rdx, 9
	call	syscall_write
	mov	rsi, 1
	jmp	syscall_exit_group
loopstr: .string "<<loop>>\n"
xvar:
	ppush	rax
	drefo	rax, 8
	ERAX
	ppop	rdi           # rdi=thunk
	ptr	rdi           # rdi=PTR(thunk)
	mov	[rdi+8], rax
	lea	rsi, [xdone]
	mov	[rdi], rsi
	ret
xhead:
	ppush	rax                         # save thunk
	drefo	rax, 8                      # rax=(a b)
	clzsz_	rcx, rax                    # n = rax.size
	ppush	rax                         # save oldclz
	dref_	rdi, rax                    # arg1 = oldclz.ptr
	lea	rsi, [rcx-1]                # arg2 = n-1
	call	closure                     # rax = newclz
	ppop	rsi                         # src = restore(oldclz)
	ptr	rsi                         # src = oldclz.ptr
	ptr_	rdi, rax                    # dst = newclz.ptr
	mov	r8b, [rsi-8]                # r8 = oldclz.heaprecord.type
	mov	[rdi-8], r8b                # newclz.heaprecord.type = r8
	rep	movsq                       # memcpy(newclz.ptr, oldclz.ptr, n)
	ppop	rdi                         # rdi = restore thunk
	ptr	rdi                         # rdi = thunk.ptr
	mov	[rdi+8], rax                # thunk[1] = newclz
	lea	rsi, [xdone]
	mov	[rdi], rsi                  # thunk[0] = xdone
	ret                                 # return newclz
xunknown:
	lea	rdx, [xhole]
	ptr_	r10, rax
	mov	[r10], rdx
	ppush	rax
	call	xunknownnoupdate
	ppop	r10               # thunk
	ptr	r10
	lea	rdx, [xdone]
	mov	[r10], rdx
	mov	[r10+8], rax
	ret
xunknownnoupdate:
thapple:
	ptr_	r8, rax        # r8 = ptr thunk
	mov	rcx, [r8-8]    # fetch RECORD
	shr	rcx, 14        # heapsz (in words)
	dec	rcx            # size = heapsz-1 (ignoring fp)
	cmp	rcx, 2
	je	thapple1
	cmp	rcx, 3
	je	thapple2
	cmp	rcx, 4	       # TODO: use a jump table (for arities up to 6)
	je	thapple3
	mov	r11, rcx
	mov	r9, rcx
	shl	r9, 3          #
	sub	r15, r9        # sp -= size (*8 for bytes)
	lea	rsi, [r8+8]    # src = &thunk[1]
	mov	rdi, r15       # dst = stack
	rep	movsq          # copy
	lea	rdi, [r11-1]
	jmp	apply          # return apply(args)
thapple1:
	mov	rax, [r8+8]
	mov	rdi, [r8+16]
	jmp	apply1
thapple2:
	mov	rax, [r8+8]
	mov	rdi, [r8+16]
	mov	rsi, [r8+24]
	jmp	apply2
thapple3:
	mov	rax, [r8+8]
	mov	rdi, [r8+16]
	mov	rsi, [r8+24]
	mov	rdx, [r8+32]
	jmp	apply3
.global opnil
opnil:                                      # opnil(rdi=val):
	EVAL	rdi                         #   evaluate
fastnil:                                    # fastnil:
	xor	eax, eax                    #   set return value to 0
	mov	edx, 1                      #   possible return value of 1
	test	rdi, rdi                    #   is val 0?
	cmovz	eax, edx                    #   set return value to 1 if 0
	ret                                 #   return
optobit:                                    # optobit(rdi=val):
	EVAL	rdi                         #   evaluate
fasttobit:                                  # fasttobit:
	xor	eax, eax                    #   set return value to 0
	mov	edx, 1                      #   possible return value of 1
	test	rdi, rdi                    #   is val not zero?
	cmovnz	eax, edx                    #   set return value to 1 if not 0
	ret                                 #   return
.global opnat
opnat:                                      # opnat(rdi=n):
	mov	rax, rdi                    #   set to rax for evaluation
	ERAX                                #   evaluate rax
	ncast	rax                         #   cast result to nat
	ret                                 #   return
.global fastinc
opinc:
	ENAT	rdi                         # evaluate+cast
fastinc:
	mov	rax, rdi                    # result = input
	add	rax, 1                      # result += 1
	js	slowinc                     # if indirect, slowinc
	ret                                 # fast path: return (x+1)
slowinc:
	jdirect	rdi, mkword64               # if direct(x) return mkword64(x+1)
	xor	eax, eax                    # clear invalid register state
	call	reservecopy                 # rax/ptr, rdx/sz = reservecopy(x)
	mov	r10, rdx                    # r10 = wordsz
	mov	rdi, rax                    # arg = resultptr
	call	bufinc                      # bufinc
	lea	rdi, [r10+1]                # arg = wordsz+1
	jmp	claim                       # return claim(arg)
bufinc.loop:                                # rax:Ptr[Word] -> (clobbers rax)
	add	rax, 8                      # rax = &rax[1]
bufinc:
	add	qword ptr [rax], 1          # *rax += 1
	jc	bufinc.loop                 # if overflow, repeat
	ret                                 # return
.global fastdec
opdec:
	ENAT	rdi                         # Evaluate + cast
fastdec:
	jheap	rdi, slowdec                # If indirect, slowpath
	xor	eax, eax                    # result = 0
	dec	rdi                         # rdi--
	cmovns	rax, rdi                    # if unsigned, result=rdi
	ret                                 # return result
slowdec:
	call	reservecopy                 # rax=pt rdx=sz {rdi rsi rcx}
	call	bufdec                      # ptr > () {rax}
	mov	rdi, rdx                    # arg1 = sz
	jmp	claim                       # rax=Val {rax rdi rsi r8}
bufdec:
	sub	qword ptr [rax], 1          # (*buf)--
	lea	rax, [rax+8]                # buf++
	jc	bufdec                      # if underflow, loop
	ret                                 # otherwise return
opadd:
	ENAT	rsi, rdi                    # evaluate y + cast (saving x)
	ENAT	rdi, rsi                    # evaluate x + cast (saving y)
	# fallthrough to fastadd
.global fastadd
fastadd:                                    # rdi=x rsi=y
	mov	rax, rdi                    #   res = x
	add	rax, rsi                    #   res += y
	jc	slowadd                     #   overflow?
	js	fastadd.maybeoverflow       #   high bit set?
	ret                                 #   return
fastadd.maybeoverflow:                      # rax=res rdi=x rsi=y
	mov	rdx, rdi                    #
	or	rdx, rsi                    #   if direct(x|y):
	jns	mkword64                    #     tailcall mkword64(res)
	# fallthrough to slowadd
slowadd:
	xor	eax, eax                    # clear invalid register state
	jdirect	rdi, slowadd.directx        # if x is direct, goto directx
	jdirect	rsi, slowadd.directy        # if y is direct, goto directy
slowadd.slowpath:
	wordsz_	r8, rdi                     # r8 = x.sz
	wordsz_	r9, rsi                     # r9 = y.sz
	cmp	r9, r8                      # if (y.sz > x.sz)
	mov	rax, rdi                    #
	cmova	rdi, rsi                    #     swap x and y
	cmova	rsi, rax                    #
	mov	r10, r8                     #
	cmova	r8, r9                      #     swap xsz, ysz
	cmova	r9, r10                     #
	ppush	rsi                         # save y
	call	reservecopy                 # rax=buf rdx=xsz
	mov	r11, rdx                    # stash xsz in r11
	ppop	rdi                         # restore y
	mov	rcx, r9                     # count = y.sz
	ptr	rdi                         # input = y.buf
	call	bufadd                      # bufadd(buf, y.buf, count)
	lea	rdi, [r11+1]                # may have grown by one via carry.
	jmp	claim                       # construct result.
slowadd.directx:                            # rdi=word rsi=big
	xchg	rdi, rsi                    #   swap big/word
slowadd.directy:                            # rdi=big rsi=word
	mov	r11, rsi                    #   r11=word (prevent clobber)
	call	reservecopy                 #   rdi=bignat
	mov	rdi, rax                    #   rdi=ptr
	mov	rsi, r11                    #   rsi=word
	call	bufadd1                     #   bufadd1(ptr, word)
	lea	rdi, [rdx+1]                #   rdi = xsz+1
	jmp	claim                       #   return claim(rdi)
bufadd:                                     # rax=dst rdi=src rcx=n
	clc                                 #   clear carry
1:                                          # loop:
	mov	r8, [rdi]                   #   load src word
	adc	[rax], r8                   #   dst[i] += src[i]+CF; sets CF
	lea	rax, [rax+8]                #   rax++
	lea	rdi, [rdi+8]                #   rdi++
	dec	rcx                         #   n--
	jnz	1b                          #   if (n != 0) goto loop
	jc	bufinc                      #   if (carry) goto bufinc(rax)
	ret                                 #   return
bufadd1:                                    # rdi=dst rsi=word
	add	[rdi], rsi                  # Perform the addition in-place
	lea	rax, [rdi+8]                # nx = &buffer[1]
	jc	bufinc                      # if (overflow) tailcall bufinc(nx)
	ret                                 # return
opsub:
	ENAT	rdi, rsi
	ENAT	rsi, rdi
	# fallthrough to fastsub
.global fastsub
fastsub:
	mov	rax, rdi
	or	rax, rsi  # Careful!  rax value is gc-unsafe in the slow path.
	js	slowsub
	xor	rdx, rdx
	sub	rdi, rsi
	cmovc	rdi, rdx
	mov	rax, rdi
	ret
slowsub:
	xor	eax, eax # Clear GC-unsafe register
	jdirect	rsi, slowsub.yword
	mov	r10, rdi             # stash rdi/rsi in unclobbered regs
	mov	r11, rsi
	call	compare              # compare arguments
	mov	rdi, r10
	mov	rsi, r11             # restore rdi/rsi
	cmp	rax, 2
	jne	slowsub.zero         # if not a>b, result is 0
	call	reservecopy          # rax = rBuf, rdx = a.wordSz
	mov	r10, rdx             # save size
	ptr_	rdi, r11
	wordsz_	rcx, r11
	call	bufsub               # bufsub(rBuf, PTR(b), b.wordSz)
	mov	rdi, r10             # restore size
	jmp	claim                # return claim(resSz)
slowsub.zero:
	xor	eax, eax
	ret
slowsub.yword:
	mov	r11, rsi
	call	reservecopy
	mov	rdi, rax
	mov	rsi, r11
	call	bufsub1
	mov	rdi, rdx
	jmp	claim
bufsub1: # rdi/a:Ptr[Word] rsi/b:Word (clobbers rax)
	sub	[rdi], rsi
	jc	bufsub1.underflow
	ret
bufsub1.underflow:
	lea	rax, [rdi+8]
	jmp	bufdec
bufsub:                                     # bufsub: (rax/x rdi/y rcx/yWords)
	clc                                 #   Clear carry
bufsub.loop:                                # loop:
	mov	r8, [rdi]                   #   r8 = y[i]
	sbb	[rax], r8                   #   x[i] -= r8
	lea	rdi, [rdi+8]                #
	lea	rax, [rax+8]                #
	dec	rcx                         #   i--
	jnz	bufsub.loop                 #   if (i != 0) loop
	jc	bufdec                      #   If underflow, decrement
	ret                                 #   Otherwise return.
opmul:
	ENAT	rdi, rsi                    #
	ENAT	rsi, rdi                    #
	# fallthrough to fastmul            #
.global fastmul
fastmul:                                    # fastmul:
	mov	rax, rdi                    #   tmp = arg1
	or	rax, rsi                    #   tmp |= arg2
	js	slowmul                     #   if either indirect, slowmul
	mov	rax, rdi                    #   a = x
	mov	rdx, rsi                    #   b = y
	mul	rdx                         #   rax, rdx = a*b
	jmp	mkdouble                    #   return mkdouble(rax, rdx)
slowmul:
	xor	eax, eax                    # Clear out gc-unsafe register rax
	jdirect	rdi, slowmul.directx
	jdirect	rsi, slowmul.directy
slowmul.indirect:
	mov	rcx, rsi                    # rcx   = y (n)
	mov	rsi, rdi                    # rsi   = x (m)
	wordsz_	r8, rsi                     # nx/r8 = x.wordsz (msz)
	wordsz_	r9, rcx                     # ny/r9 = y.wordsz (nsz)
	lea	rdi, [r8+r9]                # rsz   = nx + ny
	push	rdi                         # save rsz
	ppush	rcx                         # save y
	call	reserve                     # Clobbers rax, rdi, rcx
	ppop	rcx                         # restore y
	ptr	rcx                         # rcx = n (y.buf)
	ptr	rsi                         # rsi = m (x.buf)
	lea	rdi, [r14+8]                # rdi = &hp[1]
	call	bufmul                      # call bufmul
	pop	rdi                         # restore rsz
	jmp	claim                       # return claim(rsz)
slowmul.directx:
	xchg	rdi, rsi
slowmul.directy:
	xor	eax, eax                    # result = 0
	cmp	rsi, 1
	ja	1f                          # if (y <= 1):
	cmove	rax, rdi                    #     if y=1, result=x
	ret                                 #     return result
1:	mov	rdx, rdi                    # rdx=xref (avoid clobber)
	wordsz	rdi
	mov	r8, rdi                     # r8=xsz   (avoid clobber)
	inc	rdi                         # reserve(wordsz(x) + 1)
	call	reserve                     # Clobbers rdi, rcx, rax
	ptr_	r9, rdx                     # r9 = xbuf
bufmul1:                                    # bufmul1:
	xor	ecx, ecx                    #   rcx/carry = 0
	xor	edi, edi                    #   rdi/i     = 0
bufmul1.loop:                               # loop:
	mov	rax, rsi                    #   lo    = yword
	mul	qword ptr [r9+8*rdi]        #   hi,lo = lo*xbuf[i]
	add	rax, rcx                    #   lo    = lo+carry
	adc	rdx, 0                      #   hi    = hi+cf
	mov	rcx, rdx                    #   carry = hi
	inc	rdi                         #   i++
	mov	[r14+8*rdi], rax            #   hp[i] = lo
	cmp	rdi, r8                     #   if (i < xsz)
	jb	bufmul1.loop                #     goto loop
	inc	rdi                         #   i++
	mov	[r14+8*rdi], rcx            #   hp[i] = carry
	jmp	claim                       #   tailcall claim(i)
bufmul:
	push	r12
	push	r13
	xor	r10, r10                  # i=0
	xor	r13, r13                  # carry=0
bufmul.iloop:
	xor	r11, r11                  # j=0
bufmul.jloop:
	# clc                             # clear carry (pointless?)
	mov	rax, [rsi+8*r10]          # lo = m[i]
	mul	qword ptr [rcx+8*r11]     # hi:lo = lo*n[j]
	lea	r12, [r10+r11]            # i+j
	add	rax, r13                  # lo += carry
	adc	rdx, 0                    # hi += hi+cf
	add	[rdi+8*r12], rax          # out[i+j] += lo
	adc	rdx, 0                    # rdx += cf
	mov	r13, rdx                  # carry=hi
	inc	r11                       # j++
	cmp	r11, r9                   # if (j < nsz)
	jb	bufmul.jloop              #     continue
	inc	r12                       # (i+j)++
	add	[rdi+8*r12], r13          # out[i+j] += carry
	xor	r13, r13                  # carry=0
	inc	r10                       # i++
	cmp	r10, r8                   # if i < mSz
	jb	bufmul.iloop              #     continue outer
	pop	r13                       # pop r13
	pop	r12                       # pop r12
	ret                               # return
.global opdiv
opdiv: # rdi=x rsi=y
	ENAT	rdi, rsi
	ENAT	rsi, rdi
	# fallthrough to fastdiv
fastdiv:                     # fastdiv: (rdi=x, rsi=y)
	jheap	rdi, slowdiv #   if indirect, goto slowdiv
	test	rsi, rsi     #   if (y != 0)
	jnz	1f           #     then okay
	ud2                  #     else crash (divide by zero)
1:	xor	edx, edx     #   high word = 0
	mov	rax, rdi     #   low word = x
	div	rsi          #   rax,rdx = x/y
	ret
slowdiv: # rsi=x rdi=y -> rax=(x/y)
	jdirect	rsi, slowdiv.y_direct
	mov	r8, rdi
	mov	r9, rsi
	call	compare    # clobbers rax, rdi, rsi, rcx
	mov	rdi, r8
	mov	rsi, r9
	cmp	rax, 2     # if x>y goto x_greater
	je	slowdiv.x_greater
	ret                # if x<y return 0 else return 1
slowdiv.y_direct:
slowdiv.x_greater:
	ppush	rsi, rdi
	push	rbp
	mov	rbp,rsp
	add	rsp, -120
	call	unpack
	mov	rdi, [rbp - 24]
	sub	rdi, [rbp - 64]
	mov	[rbp - 88], rdi
	inc	rdi
	btw	rdi
	mov	[rbp - 96], rdi
	mov	rsi, [rbp - 40]
	add	rdi, rsi
	add	rdi, rsi
	call	reserve
	mov	[rbp - 104], rax
	mov	rdi, [rbp - 96]
	lea	rsi, [rax + rdi * 8]
	mov	[rbp - 112], rsi
	add	rdi, [rbp - 40]
	lea	rsi, [rax + rdi * 8]
	mov	[rbp - 120], rsi
	call	unpack
	mov	rdi, [rbp - 112]
	mov	rsi, [rbp - 32]
	mov	rcx, [rbp - 40]
	rep	movsq
	mov	rdi, [rbp - 120]
	mov	rsi, [rbp - 72]
	mov	rdx, [rbp - 80]
	mov	rcx, [rbp - 88]
	call	buflsh
	mov	rdi, [rbp - 112]
	mov	rsi, [rbp - 120]
	mov	rdx, [rbp - 40]
	mov	rcx, [rbp - 104]
	mov	r8, [rbp - 88]
	call	divloop
	mov	rdi, [rbp - 96]
	call	claim # TODO drop+leave AND THEN tail-call into claim
	pdrop	2
	leave
	ret
divloop:
	cmp	r8, 0
	js	divloop.done
	push_rdi_to_r8
	call	divstep.gte
	pop_r8_to_rdi
	test	rax, rax
	jz	divloop.endloop
	push_rdi_to_r8
	mov	rax, rdi
	mov	rdi, rsi
	mov	rcx, rdx
	call	bufsub
	pop_r8_to_rdi
	mov	r9, r8
	shr	r9, 6
	mov	r10, r8
	and	r10, 63
	mov	r11, [rcx + 8*r9]
	bts	r11, r10
	mov	[rcx + 8*r9], r11
divloop.endloop:
	push_rdi_to_r8
	mov	rdi, rsi
	mov	rsi, rdx
	call	bufshr1
	pop_r8_to_rdi
	mov	rdx, rax
	dec	r8
	jmp	divloop
divloop.done:
	ret
divstep.gte:
	dec	rdx
divstep.gte_loop:
	mov	rax, [rdi + rdx*8]
	cmp	rax, [rsi + rdx*8]
	jb	divstep.gte_cannot_subtract
	ja	divstep.gte_can_subtract
	dec	rdx
	cmp	rdx, 0
	jge	divstep.gte_loop
divstep.gte_can_subtract:
	mov	rax, 1
	ret
divstep.gte_cannot_subtract:
	mov	rax, 0
	ret
oplsh:                                      # oplsh: (rdi=x rsi=shift)
	ENAT	rdi, rsi                    #   eval+cast x
	ENAT	rsi, rdi                    #   eval+cast shift
	# fallthrough to fastlsh
.global fastlsh
fastlsh:                                    # fastlsh: (rdi=x, rsi=shift)
	lzcnt	r8, rdi                     #   num high zeros
	cmp	rsi, r8                     #   if (shift >= lzcount)
	jae	slowlsh                     #     goto slowlsh
	mov	rcx, rsi                    #   count = shift
	mov	rax, rdi                    #   res = x
	shl	rax, cl                     #   res <<= x
	ret                                 #   return res
slowlsh:                                    # slowlsh: (rdi=x, rsi=shift)
	jheap	rdi, biglsh                 #   indirect nat? slowlsh
	cmp	rsi, 64                     #   if (shift >= 64)
	jae	biglsh                      #     goto biglsh
	mov	rcx, rsi                    #   count=shift
	xor	edx, edx                    #   hi=0
	mov	rax, rdi                    #   lo=x
	shl	rax, cl                     #   rax = new lo
	shld	rdx, rdi, cl                #   rdx = new hi
	jmp	mkdouble                    #   goto mkdouble(rdx,rax)
biglsh:                                     # biglsh:
	ppush	rsi, rdi                    #
	push	rbp                         #
	mov	rbp,rsp                     #
	add	rsp, -96                    #
	call	unpack                      #
	mov	rax, [rbp-48]               #   load shift
	test	rax, rax                    #   if (shift==0)
	jz	biglsh.zero                 #     goto zero
	mov	rax, [rbp-64]               #   load shift.bits
	cmp	rax, 63                     #   if (shift.bits > 63)
	ja	biglsh.huge                 #     goto huge
	mov	rdi, [rbp-24]               #
	add	rdi, [rbp-48]               #
	btw	rdi                         #
	mov	[rbp-88], rdi               #
	call	reserve                     #
	mov	[rbp-96], rax               #
	call	unpack                      #
	mov	rdi, [rbp-96]               #
	mov	rsi, [rbp-32]               #
	mov	rdx, [rbp-40]               #
	mov	rcx, [rbp-48]               #
	call	buflsh                      #
	mov	rdi, [rbp-88]               #
	call	claim                       #
	pdrop	2                           #
	leave                               #
	ret                                 #
biglsh.zero:                                #
	pdrop	2                           #
	mov	rax, [rbp - 8]              #
	leave                               #
	ret                                 #
biglsh.huge:                                #
	ud2                                 #
buflsh:
	push	rbx
	push	r12
	mov	r9, rcx
	shr	r9, 6
	and	rcx, 63
	test	rcx, rcx
	jz	buflsh.word_only
	test	r9, r9
	jz	buflsh.basic
	xor	r8, r8
buflsh.zero_loop:
	cmp	r8, r9
	jge	buflsh.basic
	mov	qword ptr [rdi], 0
	add	rdi, 8
	inc	r8
	jmp	buflsh.zero_loop
buflsh.basic:
	xor	r8, r8
	xor	r9, r9
	mov	rbx, 64
	sub	rbx, rcx
	mov	r12, rcx
buflsh.loop:
	cmp	r8, rdx
	jge	buflsh.finalize
	mov	r10, [rsi + r8*8]
	mov	r11, r10
	mov	rcx, r12
	shl	r11, rcx
	or	r11, r9
	mov	[rdi + r8*8], r11
	mov	r9, r10
	mov	rcx, rbx
	shr	r9, rcx
	inc	r8
	jmp	buflsh.loop
buflsh.finalize:
	cmp	r9, 0
	je	buflsh.return
	mov	[rdi + r8*8], r9
buflsh.return:
	pop	r12
	pop	rbx
	ret
buflsh.word_only:
	lea	rdi, [rdi + r9*8]
	mov	rcx, rdx
	rep	movsq
	pop	r12
	pop	rbx
	ret
optest:                                      # optest: rdi=i rsi=nat
	ENAT	rdi, rsi                     #   eval+cast
	ENAT	rsi, rdi                     #   eval+cast
	jheap	rsi, optest.indirect         #   if rsi.big goto indirect
optest.direct:                               # direct: rdi=i rsi=nat
	mov	rdx, 63                      #
	cmp	rdi, rdx                     #   i = min(i, 63)
	cmova	rdi, rdx                     #
optest.directknown:                          # directknown
	xor	eax, eax                     #   res=0
	bt	rsi, rdi                     #   check bit
	mov	rdx, 1                       #   1
	cmovc	rax, rdx                     #   if set, res=1
	xor	esi, esi                     #   wipe nat (in case invalid)
	ret                                  #   return res
optest.indirect:                             # indirect:
	bitsiz_	rcx, rsi                     #   rcx=bits
	cmp	rdi, rcx                     #   if (i >= bits)
	jae	ret0                         #     return 0
	mov	rdx, rdi                     #
	shr	rdx, 6                       #   wordix = bitix/64
	ptr	rsi                          #   nat = PTR(nat)
	mov	rsi, [rsi+rdx*8]             #   nat = nat[wordix]
	and	rdi, 63                      #   idx = idx % 64
	jmp	optest.directknown           #   goto directknown
opset:                                      # opset: rdi=i rsi=x
	ENAT	rdi, rsi                    #   eval+cast i
	ENAT	rsi, rdi                    #   eval+cast x
fastset:                                    #
	mov	r8, 63                      #
	mov	r9, rdi                     #
	cmp	r9, r8                      #
	cmova	r9, r8                      #   tmp = min(i, 63)
	mov	rax, rsi                    #
	bts	rax, r9                     #   res = bts(x, tmp)
	test	rax, rax                    #   if (high bit set)
	js	slowset                     #     enter then slow path
	ret                                 #   return result
slowset:                                    # slowset: rdi=i rsi=x
	xor	eax, eax                    #   clear rax to avoid bad gc state
	mov	r8, rdi                     #   r8/i (avoid clobber)
	mov	r9, rsi                     #   r9/x (avoid clobber)
	lea	rsi, [r8+64]
	shr	rsi, 6                      #   minsz = ((i+1)+63)/64
	mov	rdi, r9
	call	reservecopyflex             #   rax/ptr rdx/wid
	mov	rdi, r8
	shr	rdi, 6                      #   wix = i / 64
	movzx	rsi, r8b                    #   bix = i % 64
	mov	r12, [rax+8*rdi]            #   tmp = buf[wix]
	bts	r12, rsi                    #   tmp = bts(tmp, bix)
	mov	[rax+8*rdi], r12            #   buf[wix] = tmp
	xor	r12, r12                    #   clear tmp (could be invalid)
	mov	rdi, rdx
	jmp	claim                       #   return claim(wid)
.global fastclear
opclear:                                    # opclear: rdi=i rsi=x
	ENAT	rdi, rsi                    #   eval+cast i
	ENAT	rsi, rdi                    #   eval+cast x
fastclear:                                  # fastclear:
	jheap	rsi, slowclear              #   If x is indirect, slowpath
	mov	rax, rsi                    #   result = x
	mov	rsi, 63                     #   rsi=63
	cmp	rdi, rsi                    #   if (i > 63)
	cmova	rdi, rsi                    #     i=63
	btr	rax, rdi                    #   clear bit
1:	ret                                 #   return result
slowclear:                                  # slowclear: rdi=i rsi=x
	mov	rax, rsi                    #   result = x
	bitsiz	rsi                         #   n = bitsiz(x)
	cmp	rdi, rsi                    #   if (i >= n) return
	jae	1b
	mov	r8, rdi                     #   r8/i (avoid clobber)
	mov	r9, rax                     #   r9/x (avoid clobber)
	mov	rdi, rax
	call	reservecopy                 #   rax/ptr rdx/wid
	mov	rdi, r8                     #
	shr	rdi, 6                      #   wix = i / 64
	movzx	rsi, r8b                    #   bix = i % 64
	mov	r12, [rax+8*rdi]            #   tmp = buf[wix]
	btr	r12, rsi                    #   tmp = btr(tmp, bix)
	mov	[rax+8*rdi], r12            #   buf[wix] = tmp
	xor	r12, r12                    #   clear tmp (could be invalid)
	mov	rdi, rdx                    #
	jmp	claim                       #   return claim(wid)
opload8:
	ENAT	rdi, rsi
	ENAT	rsi, rdi
fastload8:
	mov	rcx, rdi # TODO: reallocate registers
	mov	rax, rsi
	jheap	rax, fastload8.indirect
	cmp	rcx, 8
	jae	fastload8.zero
	shl	rcx, 3             # bits to drop
	shr	rax, rcx
	movzx	rax, al
	ret
fastload8.indirect:
	bitsiz_	r10, rax           # r10 = bitsize(rax/bignat)
	add	r10, 7
	shr	r10, 3             # bytesz
	cmp	rcx, r10           # if (i >= sz) return 0
	jae	fastload8.zero
	ptr	rax                # rax = natptr
	mov	al, [rax + rcx]    # rax = natptr[i]
	movzx	rax, al
	ret
fastload8.zero:
	xor	rax, rax
	ret
.global faststore8
opstore8:                                   # opstore8: rdi/i rsi/b rdx/n
	ENAT	rdi, rsi, rdx               #  eval+cast index
	ENAT	rsi, rdi, rdx               #  eval+cast byte
	ENAT	rdx, rsi, rdi               #  eval+cast nat
faststore8:                                 # faststore8:
	mov	r8, rdi                     #   r8/i
	mov	r9, rsi                     #   r9/b
	mov	r10, rdx                    #   r10/n
	mov	rdi, r10                    #   x = n
	mov	rsi, r8                     #
	shr	rsi, 3                      #
	inc	rsi                         #   y = (i/8)+1
	call	reservecopyflex             #   buf, wid = reservecopyflex(x,y)
	jdirect	r9, 1f                      #   if (indirect(r9))
	dref	r9                          #     r9 = r9.lsw()
1:	mov	[rax+r8], r9b               #   buf[i] = (u8) b
	xor	r9w, r9w                    #   clear r9
	mov	rdi, rdx                    #   size = wid
	jmp	claim                       #   return claim(wid)
.global faststore64
opstore64:                                  # opstore64: rdi/i rsi/w rdx/n
	ENAT	rdi, rsi, rdx               #   eval+cast index
	ENAT	rsi, rdi, rdx               #   eval+cast word
	ENAT	rdx, rsi, rdi               #   eval+cast nat
faststore64:                                # faststore64:
	mov	r8, rdi                     #   r8/i
	mov	r9, rsi                     #   r9/w
	mov	r10, rdx                    #   r10/n
	mov	rdi, r10                    #   x = n
	lea	rsi, [r8+7]                 #
	shr	rsi, 3                      #   ; y = min word size
	inc	rsi                         #   y = ((i+7)/8)+1
	call	reservecopyflex             #   buf, wid = reservecopyflex(x,y)
	jdirect	r9, 1f                      #   if (indirect(word))
	dref	r9                          #   word = word.lsw()
1:	mov	[rax+r8], r9                #   buf[i] = word
	xor	r9w, r9w                    #   wipe r9 register
	mov	rdi, rdx                    #   size = wid
	jmp	claim                       #   return claim(wid)
.global opload
opload:				# opload: rdi=offset, rsi=width, rdx=nat
	ENAT	rdi, rsi, rdx
	ENAT	rsi, rdi, rdx
	ENAT	rdx, rdi, rsi
	# fallthrough to fastload
.global fastload
fastload:                       # fastload: rdi=off rsi=wid rdx=num
	jheap	rdx, fastload.indirect
fastload.direct:
	mov	rax, 8
	cmp	rdi, rax        # if (offset >= 8)
	jae	ret0            #   return 0 (avoids lsh overflow)
	cmp	rsi, rax
	cmova	rsi, rax        # width = min(width, 8)  (avoids lsh overflow)
	shl	rdi, 3		# offset bytes to bits
	shl	rsi, 3		# width bytes to bits
	mov	rax, rdx        # result = nat
	mov	rcx, rdi        #
	shr	rax, rcx        #   result >>= offset
	bzhi	rax, rax, rsi	#   extract first n bits from rax.
	ret                     #
fastload.indirect:
	## Calculate capped size.
	wordsz_	rax, rdx	# rax = wordsz(rdx)
	shl	rax, 3		# bytes = words*8
	sub	rax, rdi	# rax=size(rdx) after offset
	jbe	ret0            # if (offset >= bytes) return 0
	cmp	rax, rsi
	cmovb	rsi, rax	# rsi=min(sizeof(nat), offset)

	ppush	rdi, rsi, rdx
	mov	rdi, rsi	# rdi=width (in bytes)
	add	rdi, 7
	shr	rdi, 3		# rdi=width (in words)
	call	reserve
	ppop	rdi, rsi, rdx

	xchg	rdx, rsi        # rdx/width rsi/nat
	mov	rcx, rdx	# count = width
	ptr	rsi             # src = PTR(input nat)
	add	rsi, rdi	# src += offset
	mov	rdi, rax	# dst = rax from rawreserve
	rep	movsb           # memcopy

	mov	rdi, rdx        # rdi = bytesize
	add	rdi, 7
	shr	rdi, 3		# rdi=width (in words)
	jmp	claim
opsnore:                                    # opsnore:
	ENAT	rdi, rsi, rdx, rcx, r8      #   eval+cast rdi/iof
	ENAT	rsi, rdi, rdx, rcx, r8      #   eval+cast rsi/bof
	ENAT	rdx, rdi, rsi, rcx, r8      #   eval+cast rdx/wid
	ENAT	rcx, rdi, rsi, rdx, r8      #   eval+cast rcx/inp
	ENAT	r8, rcx, rdi, rsi, rdx      #   eval+cast r8/buf
	# fallthrough to fastsnore
.global fastsnore
fastsnore:                                  # fastsnore:
	mov	rax, r8                     #   result = buffer
	jheap	r8, bufsnore                #   indirect buffer -> bufsnore
	dbytsz_	r10, r8                     #   r10 = buffer size
	dec	r10                         #   r10 = size of writable portion
	sub0	r10, rsi                    #   r10 = writable size after offset
	cap	rdx, r10                    #   rdx/wid = safe write-size
	jheap	rcx, funnelsnore            #   indirect input -> funnelsnore
	# fallthrough to smolsnore
smolsnore:                                  # smolsnore:
	dbytsz_	r10, rcx                    #   r10 = input size
	sub0	r10, rdi                    #   r10 = readable size after offset
	cap	rdx, r10                    #   rdx = safe read-size
smolsnore.bounded:                          # bounded: (used by funnelsnore)
	shl	rdx, 3                      #   rdx = wid in bits
	shl	rdi, 3                      #   rdi = iof in bits
	shl	rsi, 3                      #   rsi = bof in bits
	shrx	rcx, rcx, rdi               #   shr to apply input offset
	bzhi	rcx, rcx, rdx               #   bzhi to extract relevant bits
	mov	r9, -1                      #   r9/mask = -1
	bzhi	r9, r9, rdx                 #   r9/mask = (1 << wid) - 1
	shlx	r9, r9, rsi                 #   r9/mask = mask << bof
	andn	r9, r9, rax                 #   tmp = buf & ~mask
	shlx	rcx, rcx, rsi               #   input <<= bof
	or	r9, rcx                     #   tmp |= (input << bof)
	test	rdx, rdx                    #   if (wid != 0)
	cmovnz	rax, r9                     #     res = tmp
	ret                                 #   return res
	# rdi/iof rsi/bof rdx/wid rcx/inp rax/r8/buf
	# buf is direct, inp is indirect
funnelsnore:
	bytsiz_	r9, rcx                     #   max  = inp.bytes
	sub0	r9, rdi                     #   max -= input-offset
	cap	rdx, r9                     #   wid = min(wid,max)
	test	rdx, rdx                    #   if (wid==0)
	jz	funnelsnore.ret             #     return buf
	refo	rcx, rdi                    #   p = PTR(inp)+iof
	mov	r9, 8                       #   tmp = 8
	sub	r9, rdx                     #   tmp = 8 - wid
	sub	rcx, r9                     #   p -= tmp
	mov	rcx, [rcx]                  #   inp = *p
	shl	r9, 3                       #   tmp *= 8 (bits)
	shrx	rcx, rcx, r9                #   inp >>= tmp (slide back)
	xor	edi, edi                    #   iof = 0
	jmp	smolsnore.bounded           #   goto smolsnore.bounded
funnelsnore.ret:
	ret
	# rdi/iof rsi/bof rdx/wid rcx/inp rax/r8/buf
	# buf is indirect
bufsnore:                                   # bufsnore:
	bytsiz_	r10, r8                     #   max  = buffer byte-size
	dec	r10                         #   max  = writeable size
	sub0	r10, rsi                    #   max -= buffer-offset
	cap	rdx, r10                    #   constrain wid
	jdirect	rcx, pokesnore              #   indirect input -> bigsnore
	bytsiz_	r10, rcx                    #   max  = inp.bytes
	sub0	r10, rdi                    #   max -= input-offset
	cap	rdx, r10                    #   constrain width to input size
	ptr	rcx                         #
	add	rcx, rdi                    #   rcx = PTR(inp)+iof
	ptr	r8                          #
	add	r8, rsi                     #   r8 = PTR(buf)+bof
	mov	rdi, r8                     #   dst = r8
	mov	rsi, rcx                    #   src = rcx
	mov	rcx, rdx                    #   cnt = wid
	rep	movsb                       #   memcpy
	ret                                 #   return buf
	# rdi/iof rsi/bof rdx/wid rcx/inp rax/r8/buf
	# buf is indirect, inp is direct
pokesnore:                                  # pokesnore:
	dbytsz_	r9, rcx                     #   tmp  = inp.bytes
	sub0	r9, rdi                     #   tmp -= iof
	cap	rdx, r9                     #   wid = min(wid, tmp)
	test	rdx, rdx                    #   if (wid==0)
	jz	pokesnore.ret               #     return buf
	shl	rdi, 3                      #   iof*8
	shrx	rcx, rcx, rdi               #   inp >>= (iof*8)
	refo	r8, rsi                     #   p = PTR(buf)+bof
	mov	r9, 8                       #
	sub	r9, rdx                     #   tmp = [8-wid]
	sub	r8, r9                      #   p -= tmp
	mov	r10, [r8]                   #   wor = *p
	shl	r9, 3                       #   tmp <<= 3 (bit clif)
	bzhi	r10, r10, r9                #   clear high bits from wor
	shlx	rcx, rcx, r9                #   inp <<= tmp (slide position)
	or	r10, rcx                    #   wor |= inp
	mov	[r8], r10                   #   *p = wor
pokesnore.ret:                              # pokesnore.ret:
	ret                                 #   return buf
.global opsz, fastsz
opsz:                                       # opsz: rdi=arg
	EVAL	rdi                         #   eval arg
fastsz:                                     # fastsz:
	xor	rax, rax                    #   res = 0
	jnclz	rdi, 1f                     #   if !isapp(arg) return
	clzsz_	rax, rdi                    #   res = arg.sz()
1:	ret                                 #   return
.global ophd
ophd:                                       # ophd: rdi=arg
	mov	rax, rdi                    #   res = input
	ERAX                                #   eval res
	jnclzt	rax, 1f                     #   if (!isapp(res)) return
	dref	rax                         #   res = res.hd()
1:	ret                                 #   return
oplast:                                     # oplast: (rdi=thunk)
	EVAL	rdi                         #   eval rdi
	jnclz	rdi, ret0                   #   if (!rdi.isclz) return 0
	slots_	rcx, rdi                    #   get slots (sz+1)
	ptr	rdi                         #   p = PTR(rdi)
	mov	rax, [rdi+8*rcx-8]          #   res = p[slots-1]
	JRAX                                #   return eval(res)
opinit:                                     # opinit: (rdi=thunk)
	EVAL	rdi                         #   eval rdi
	jnclz	rdi, ret0                   #   if (!rdi.isclz) return 0
	dref_	rax, rdi                    #   hd = rdi.hd()
	clzsz_	rcx, rdi                    #   n = rdi.size
	cmp	rcx, 1                      #   if (n != 1)
	jne	opinit.copy                 #     goto copy
	ret                                 #   return hd
opinit.copy:                                # copy: rcx/n rdi/clz rax/hd
	ppush	rdi                         #   save oldclz
	mov	rdi, rax                    #   arg1 = hd
	lea	rsi, [rcx-1]                #   arg2 = n-1
	call	closure                     #   rax = newclz
	ppop	rsi                         #   src = restore(oldclz)
	ptr	rsi                         #   src = oldclz.ptr
	ptr_	rdi, rax                    #   dst = newclz.ptr
	mov	r8b, [rsi-8]                #   r8 = oldclz.heaprecord.type
	mov	[rdi-8], r8b                #   newclz.heaprecord.type = r8
	rep	movsq                       #   memcpy(newclz.ptr, oldclz.ptr, n)
	ret                                 #   return newclz
oprow:                                      # oprow:
	ENAT	rdi, rsi, rdx               #   eval+cast hd
	ENAT	rsi, rdi, rdx               #   eval+cast n
	mov	rax, rdi                    #   rax/hd
	test	rsi, rsi                    #   if (n == 0)
	jz	oprow.ret                   #     return hd
	call	closure                     #   row = closure(hd, sz)
	mov	r8, rax                     #   r8/row
	mov	r9, rdi                     #   r9/hd
	xor	eax, eax                    #   rax = 0
	ptr_	rdi, r8                     #   rdi = PTR(row)
	mov	[rdi], r9                   #   row.hd = hd
	add	rdi, 8                      #   rdi = &row.slot(0)
	mov	rcx, rsi                    #   count = n
	rep	stosq                       #   memset
	xor	ecx, ecx                    #   i = 0
	mov	rdi, rdx                    #   rdi/list
	mov	rdx, rsi                    #   rdx/n
oprow.loop:                                 # loop: r8=row rdx=n rcx=i rdi=list
	cmp	rcx, rdx                    #   if (i >= n)
	jae	oprow.break                 #     break
	EVAL	rdi, rdx, rcx, r8           #   evaluate list
	jnclz	rdi, oprow.break            #   if notapp(list) break;
	drefo_	rax, rdi, 8                 #   res = rdi.ix(0)
	ptr_	r9, r8                      #   r9      = PTR(row)
	mov	[r9 + rcx*8+8], rax         #   r9[i+i] = item
	slots_	rax, rdi                    #   get gcsz
	cmp	rax, 3                      #   if (gcsz < 3)
	jb	oprow.break                 #     break
	drefo	rdi, 16                     #   list = list.ix(1)
	inc	rcx                         #   i++
	jmp	oprow.loop                  #   continue
oprow.break:                                # break:
	mov	rax, r8                     #   res = buf
oprow.ret:                                  # ret:
	ret                                 #   return res
PeekOp:                                     # rdi=[bufptr off sz]
	mov	rsi, 3                      #
	call	unpackop.sized              #
	ENAT	rdi, rsi, rdx               #   rdi = c dst pointer as nat
	ENAT	rsi, rdi, rdx               #   rsi = offset inside rdi
	ENAT	rdx, rdi, rsi               #   rdx = length in bytes to copy
	# fallthrough to fastpeekop
.global fastpeekop
fastpeekop:                                 # fastpeekop: rdi=src rsi=off rdx=n
	add	rdi, rsi                    #   pointless offset logic
	lea	rcx, [rdx+8]                #
	shr	rcx, 3                      #   words = (bytes+8)/8
	ppush	rdi, rdx, rcx               #   save dest/bytes/words
	mov	rdi, rcx                    #
	call	reserve                     #   rax=buf=reserve(words)
	ppop	rsi, rcx, r8                #   rsi=ptr rcx=bytes r8=words
	mov	byte ptr [rax+rcx], 1       #   set high 1 byte
	mov	rdi, rax                    #   dst=buf
	rep	movsb                       #   copy bytes (src=ptr rcx=bytes)
	mov	rdi, r8                     #   rdi=words
	jmp	claim                       #   return claim(words)
PokeOp:                                     # PokeOp:
	mov	rsi, 4                      #   input = 0[ptr nat off count]
	call	unpackop.sized              #   load 4 operands
	ENAT	rdi, rsi, rdx, rcx          #   rdi = c dst pointer as nat
	ENAT	rsi, rdi, rdx, rcx          #   rsi = source nat
	ENAT	rdx, rdi, rsi, rcx          #   rdx = offset inside bufAddr
	ENAT	rcx, rdi, rsi, rdx          #   rcx = bytes to copy
	# fallthrough to fastpokeop
.global fastpokeop
fastpokeop:                                 # fastpokeop:
	ppush	rbx, rbp                    #   save rbx, rbp
	add	rdi, rdx                    #   dst = bufAddr + offset
	mov	rbx, rdi                    #   rbx=ptr
	mov	rdi, rsi                    #   arg1=src
	call	fastbytsz                   #   rax=bytes (clobbers r8)
	mov	rbp, rcx                    #   rbp=extras
	sub0	rbp, rax                    #   extra=(n-sz) (floor to 0)
	sub	rcx, rbp                    #   count -= extras
	mov	rdi, rbx                    #   dst=ptr
	add	rbx, rcx                    #   rbx=tailptr = ptr+count
	call	pokeraw                     #   pokeraw()
	mov	rdi, rbx                    #   dst=tailptr
	mov	rcx, rbp                    #   cnt=extras
	xor	eax, eax                    #   val=0
	rep	stosb                       #   memset(dst,cnt,0)
	ppop	rbx, rbp                    #   restore rbx/rbp
	ret                                 #   return 0
pokeraw:                                    # pokeraw:
	jdirect	rsi, pokeraw.direct         #   if direct, then goto direct()
pokeraw.indirect:                           # indirect:
	ptr	rsi                         #   src=ptr(nat)
	rep	movsb                       #   memcopy(dst=ptr,str,n=rcx)
	ret                                 #   return
pokeraw.direct:                             # direct:
	ppush	rsi                         #   write to memory
	mov	rsi, r15                    #   src = sp (addr of word)
	rep	movsb                       #   movsb(dst=ptr,src,n=rcx)
	ppop	rsi                         #   remove from stack
	ret                                 #   return
.set HEAP_MAX_RANK,	51
build_heap_table:			    # build_heap_table:
	mov	DWORD PTR [heap_sizes], 12  #   hs[0]=12, hs[1]=38
	mov	DWORD PTR [heap_sizes + 4], 38
	mov	r8, 2			    #   i = 2
build_heap_table.fibloop:		    # fibloop:
	mov	esi, [heap_sizes + r8*4-4]  #   esi = heap_sizes[i - 1]
	mov	edi, [heap_sizes + r8*4-8]  #   edi = heap_sizes[i - 2]
	add	esi, edi		    #   esi += edi
	mov	[heap_sizes + r8*4], esi    #   hs[i] = hs[i - 1] + hs[i - 2]
	inc	r8			    #   i++
	cmp	r8, 21			    #   if (i < 21)
	js	build_heap_table.fibloop    #   then keep doing fibonacci
build_heap_table.mulloop:		    # mulloop:
	mov	esi, [heap_sizes + r8*4-4]  #   esi = heap_sizes[i - 1]
	lea	eax, [rsi + rsi*2]	    #   eax = rsi * 3
	add	eax, eax		    #   eax = eax * 2 (or: rsi * 6)
	xor	edx, edx		    #   clear dividend
	mov	ecx, 5			    #   div by 5
	div	ecx			    #   eax = (6 * rsi) / 5 = 1.2 * rsi
	mov	[heap_sizes + r8*4], eax    #   heap_sizes[i] = 1.2 * hs[i - 1]
	inc	r8			    #   i++
	cmp	r8, HEAP_MAX_RANK	    #   if (i < HEAP_MAX_RANK)
	js	build_heap_table.mulloop    #   then loop
	ret				    #   return
asmgc:					    # asmgc(rdi=needed):
	mov	rbx, [heap_addr]	    #   fromspc.top = heap_addr
	mov	rbp, [mapd_size]	    #   old heap mapped size in words
	shl	rbp, 3			    #   convert to bytes
	add	rbp, [heap_addr]	    #   fromspc.end = start+bytes
	mov	esi, [heap_rank]	    #   get current rank
	mov	r8d, [heap_sizes + esi*4]   #   get current size
	add	r8d, edi		    #   requested = current + needed
asmgc.incrank:				    # incrank:
	inc	esi			    #   increase rank
	mov	eax, [heap_sizes + esi*4]   #   get new rank size
	cmp	eax, r8d		    #   if (new rank size < requested)
	jb	asmgc.incrank		    #	then increase rank again
	mov	[mapd_size], rax	    #   mapd_size = new_heap_size
	mov	[heap_size], rax	    #   heap_size = mapd_size
	push	rdi			    #   preserve incoming needed
	mov	rdi, rax		    #   rdi = large enough size
	call	heap_new		    #   heap_new(size to hold requested)
	mov	[heap_addr], rax	    #   heap_addr = heap_new(...)
	mov	r14, rax		    #   heap_next = heap_addr
	mov	rax, [heap_size]	    #   rax = heap_size
	lea	r13, [r14+rax*8]	    #   heap_end = heap_addr[heap_size]
	call	copystack		    #   copystack()
	call	copyheap		    #   copyheap()
	call	free_old_heap		    #   free_old_heap()
	pop	rdi			    #   restore incoming needed
asmgc.calcnextrank:			    # calculate next rank size:
	mov	r12, [heap_addr]	    #   r12 = heap_addr
	mov	rax, r14                    #   used_bytes = heap_next
	sub	rax, r12                    #   used_bytes = heap_next-heap_addr
	shr	rax, 3			    #   bytes to words
	add	rax, rdi		    #   bytes += needed
	mov	r9d, [heap_rank]	    #   get original rank
	mov	r8d, [heap_sizes + r9d*4]   #   get original size
	mov	rdi, rax		    #   rdi = used size
	shl	rdi, 2			    #   rdi = used size * 4
	cmp	rdi, r8			    #   if (used < 0.25*orig)
	jb	asmgc.shrink		    #   then shrink a rank
	mov	rcx, r8			    #   rcx = original size
	lea     rcx, [rcx + rcx*1]	    #   rcx = original size * 2
	cmp	rdi, rcx		    #   if (use >= 0.50*orig)
	jae     asmgc.grow		    #  	then grow ranks to fit
asmgc.keeprank:				    # keep rank:
	mov	[heap_size], r8d	    #   heap size is current rank size
	jmp	asmgc.done		    #   completed size setting
asmgc.shrink:				    # shrink rank:
	## TODO: Understand where Erlang BEAM sets "bigness" and how
	## to translate that to our world. 6 is a shot in the dark.
	cmp	r9d, 6			    #   if (rank < 6) (minimum)
	jb	asmgc.keeprank		    #   then keep current rank
	dec	r9d			    #   decrease rank
	mov	qword ptr [heap_rank], r9   #   set the decreased rank.
	mov	r8d, [heap_sizes+r9d*4]	    #   get shrunk rank size
	mov	qword ptr [heap_size], r8   #   set shrunk size
	jmp	asmgc.done		    #   goto done
asmgc.grow:				    # grow rank:
	inc	r9d			    #   rank++
	mov	r8d, [heap_sizes+r9d*4]	    #   get increased rank size
	cmp	rax, r8			    #   if (used size > rank++ size)
	ja	asmgc.grow		    #   then increase again
	mov	qword ptr [heap_rank], r9   #   set the increased rank.
	mov	qword ptr [heap_size], r8   #   set grown size.
asmgc.done:				    # done:
	mov	r13, [heap_addr]	    #   heap_end = heap_addr
	lea	r13, [r13+8*r8]		    #   heap_end = &heap_addr[heap_size]
	ret				    #   return
asmgc.toobig:				    # too big:
	ud2				    #   cannot fulfill this request
.global bucket_for_request
bucket_for_request:
	shl	rdi, 3		# convert to bytes
	mov	rax, [bucket_count]
	dec	rax
	mov	rsi, [min_alloc]
bucket_for_request.loop:
	cmp	rsi, rdi	#
	jnb	bucket_for_request.done
	dec	rax
	shl	rsi, 1
	jmp	bucket_for_request.loop
bucket_for_request.done:
	ret
.set	UNALLOCATED,	0
.set	INNER_NODE,	1
.set	ALLOC_GC,	2
.set	ALLOC_MANUAL,	3
.global nodeget
nodeget:			            # nodeget(rdi=index):
	mov	ecx, edi                    #   copy index for shift
	and	ecx, 0x03                   #   ecx = index % 4
	shl	ecx, 1                      #   shift = (index % 4) * 2
	shr	rdi, 2                      #   byteIndex = index / 4
	mov	rax, [node_state_ptr]       #   load current base pointer
	movzx	eax, byte ptr [rax + rdi]   #   load byte
	shr	eax, cl                     #   offset byte
	and	eax, 0x03                   #   mask for answer
	ret                                 #   return
.global nodeset
nodeset:                                    # nodeset(rdi=index, rsi=tristate):
	mov	ecx, edi                    #   copy index for shift
	and	ecx, 0x03                   #   ecx = index % 4
	shl	ecx, 1                      #   shift = (index % 4) * 2
	shr	edi, 2                      #   byteIndex = index / 4
	mov	r9d, 0x03                   #   r9d = 2 bit mask
	shl	r9d, cl                     #   shift mask into place
	and	esi, 0x03                   #   constrain to tristate
	shl	esi, cl                     #   shift state into place
	mov	rcx, [node_state_ptr]       #   load current base pointer
nodeset.try:                                # try:
	mov	al, [rcx + rdi]             #   load current full byte
	mov	r8b, al                     #   working copy
	and	r8b, r9b                    #   r8b = original & mask
	xor	r8b, al                     #   r8b = original with cleared bits
	or	r8b, sil                    #   r8b = new value inserted
	lock cmpxchg [rcx + rdi], r8b       #   CAS xchange al/[rcx+rdi]/r8b
	jnz	nodeset.try                 #   retry if contended
	ret                                 #   return
.global ptr_for_node
ptr_for_node:			# ptr_for_node: (rdi=index, rsi=bucket)
	mov	rax, 1		#   base one
	mov	rcx, rsi	#   shl requires rcx
	shl	rax, rcx	#   rax = (1 << bucket)
	inc	rdi  		#   + 1
	sub	rdi, rax	#   index = index - (1 << bucket) + 1

	mov	rcx, [max_alloc_log2]
	sub	rcx, rsi
	shl	rdi, rcx	# offset = index << (MAX_ALLOC_LOG2 - bucket)

	mov	rax, base_ptr
	add	rax, rdi	# rax = base_ptr + offset
	ret
.global node_for_ptr
node_for_ptr:			# node_for_ptr: (rdi=ptr, rsi=bucket)
	mov	rax, rdi
	sub	rax, base_ptr	# rax = ptr - base_ptr

	mov	rcx, [max_alloc_log2]
	sub	rcx, rsi
	shr	rax, rcx	# rax =>> MAX_ALLOC_LOG2 - bucket

	mov	rdx, 1
	mov	rcx, rsi
	shl	rdx, rcx	# rdx = 1 << bucket

	add	rax, rdx

	dec	rax
	ret
.global list_init
list_init:				    # list_init(rdi=list*):
	mov	[rdi], rdi		    #   list->prev = list;
	mov	[rdi+8], rdi		    #   list->next = list;
	ret				    #   return
.global list_push
list_push:				    # list_push(rdi=list*,rsi=entry):
	ablptr	rdi                         #   input list pointer valid
	ablptr	rsi                         #   input new entry pointer valid
	mov	r8, [rdi]		    #   list_t* prev = list->prev;
	ablptr	r8                          #   input prev pointer valid
	mov	[rsi], r8		    #   entry->prev = prev;
	mov	[rsi + 8], rdi		    #   entry->next = list;
	mov	[r8 + 8], rsi		    #   prev->next = entry;
	mov	[rdi], rsi		    #   list->prev = entry;
	ret				    #   return
.global list_remove
list_remove:				    # list_remove(rdi=entry*):
	ablptr	rdi                         #   input entry pointer valid
	mov	r8, [rdi]		    #   list_t* prev = entry->prev;
	ablptr	r8                          #   prev pointer valid
	mov	r9, [rdi + 8]		    #   list_t* next = entry->next;
	ablptr	r9                          #   next pointer valid
	mov	[r8 + 8], r9		    #   prev->next = next;
	mov	[r9], r8		    #   next->prev = prev;
	ret				    #   return
.global list_pop
list_pop:				    # list_pop(rdi=list*) -> rax:
	mov	r8, [rdi]		    #   list_t* back = list->prev;
	cmp	r8, rdi			    #   if back == list
	je	ret0			    #   then return null
	ablptr	r8                          #   assert pointer is in buddy heap
	mov	rdi, r8			    #   remove back from list
	call	list_remove		    #   list_remove(back)
	mov	rax, rdi		    #   set back as return value
	ret				    #   return
.global aligned_bit_set
aligned_bit_set:			    # aligned_bit_set(rdi=bit,rsi=v*):
	inc	rdi			    #   logical bit -> physical bit
	mov	rax, rdi		    #   rax = copy logical bit
	shr	rax, 6			    #   rax = byte_index
	and	rdi, 63			    #   rdi = bit_offset
	lock bts QWORD PTR [rsi+rax*8], rdi #   atomically set bit
	setc	al			    #   original value in CF to al
	movzx	eax, al			    #   al to return value
	ret				    #   return
.global aligned_bit_get
aligned_bit_get:			    # aligned_bit_get(rdi=bit,rsi=v*):
	inc	rdi			    #   logical bit -> physical bit
	mov	rax, rdi		    #   rax = copy logical bit
	shr	rax, 6			    #   rax = byte_index
	and	rdi, 63			    #   rdi = bit_offset
	bt	QWORD PTR [rsi+rax*8], rdi  #   test bit in byte
	setc	al			    #   original value in CF to al
	movzx	eax, al			    #   al to return value
	ret				    #   return
.global atomic_max
atomic_max:                                 # atomic_max(rdi=*, rsi=new_value):
	mov	rax, [rdi]                  #   load current value
atomic_max.retry:                           # retry:
	cmp	rsi, rax                    #   if new_value <= current
	jle	atomic_max.done             #   then don't change
	lock cmpxchg [rdi], rsi             #   if [rdi]==rax then [rdi]=rsi
	jne	atomic_max.retry            #   if ZF=0, exchange failed. retry.
atomic_max.done:                            # done:
	ret                                 #   return
.equ ODD_MASK, 0x5555555555555555
.global project_or
project_or:				    # project_or(rdi=dst,rsi=src,rdx=l):
	push	r15			    #   save r15
	mov	r15, ODD_MASK		    #   extraction mask
	xor	rcx, rcx		    #   i = 0
project_or.loop:			    # loop:
	mov	rax, [rsi+8*rcx]	    #   load the 64-bit mark word
	test	rax, rax                    #   if mark word is zero
	jz	project_or.next             #   then skip the expensive pext
	mov	r8, rax			    #   copy the word
	shr	r8, 1			    #   shift right by 1 bit
	or	rax, r8			    #   combine pairs
	pext	r9, rax, r15		    #   extract every other bit
	mov	[rdi+4*rcx], r9d	    #   store the result
project_or.next:                            # next:
	inc	rcx			    #   i++
	cmp	rcx, rdx		    #   if equals length
	jne	project_or.loop 	    #   then loop
	pop	r15			    #   restore r15
	ret				    #   return
.global boot_start
boot_start:
	## Stack layout (32 bytes total):
	##    [rbp - 8]   : exe file fd
	##    [rbp - 16]  : memfd fd
	##    [rbp - 24]  : ELF binary size
	##    [rbp - 32]  : temporary buffer for patching binary
	mov	rbp, rsp
	add	rsp, -32

	mov	rax, SYS_OPEN
	lea	rdi, [proc_self_exe]	# open current executable
	mov	rsi, O_RDONLY		# no O_CLOEXEC so hold across exec
	syscall
	mov	[rbp - 8], rax		# store exe file fd; should always be 3

	mov	rdi, rax     	 	# fd = exe file fd
	lea	rsi, [elf_header]	# buf
	mov	rdx, 64			# count = 64
	mov	r10, 0			# offset = null
	mov	rax, SYS_PREAD64	# Read ELF header into memory.
	syscall

	call	elf_binary_end
	mov	[rbp - 24], rax		# Set ELF binary size.

	lea	rdi, [plan_interpreter] # Anonymous memfd name
	mov	rsi, MFD_CLOEXEC	# Close the memffd on exec
	mov	rax, SYS_MEMFD_CREATE	# Create anonymous file descriptor
	syscall
	mov	[rbp - 16], rax		# store memfd fd

	mov	rdi, [rbp - 16]	 	# rdi=memfd fd
	mov	rsi, [rbp - 24]		# rsi=elf binary size
	mov	rax, SYS_FTRUNCATE	# Truncate memfd to ELF binary size
	syscall

	mov	rdi, [rbp - 16]		# out_fd = memfd
	mov	rsi, [rbp - 8]		# in_fd = exe file fd
	mov	rdx, 0	    		# offset = 0
	mov	r10, [rbp - 24]		# count = total elf binary size
	mov	rax, SYS_SENDFILE	# Copy ELF exe portion to memfd
	syscall

	mov	rdi, [rbp - 16]	  	# out_fd = memfd
	lea	rsi, [_restart]		# address of new entry point
	mov	[rbp - 32], rsi		# write address to buffer
	lea	rsi, [rbp - 32]		# buf/rsi = buffer with _restart
	mov	rdx, 8			# pointer is 8 bytes
	mov	r10, 24			# e_entry is at 24 byte offset
	mov	rax, SYS_PWRITE64	# Write patched entry point to memfd
	syscall

	mov	rdi, ADDR_NO_RANDOMIZE
	mov	rax, SYS_PERSONALITY	# Disable ASLR for _restart
	syscall

	lea	rbx, [rsp + 32]		# rbx = original stack frame pointer
	mov	rax, [rbx]  		# rax = argc

	mov	rdi, [rbp - 16]		# rdi = memfd fd
	lea	rsi, [empty_str]	# rsi = pointer to ""
	lea	rdx, [rbx + 8]		# rdx = argv
	lea	r10, [rbx+8+(rax+1)*8]  # r10 = envp
	mov	r8, AT_EMPTY_PATH	# r8 = flags
	mov	rax, SYS_EXECVEAT	# Restart program at _restart in memfd
	syscall
	## If we return here, execveat() failed.

	add	rsp, 32			# Restore the stack pointer.
	mov	rdi, 1			# exit code 1
	call	syscall_exit_group      # and call exit
_restart:
	lea	rax, boot_main
	mov	[chosen_main], rax

	lea	rdi, [proc_fd_path]	# rdi=/proc/self/fd/3
	mov	rsi, O_RDWR		# open for reading and writing
	mov	rax, SYS_OPEN		# Open the file for writing
	syscall
	mov	[self_fd], rax		# selfd = open(...)

	mov	rdi, rax		# fd = exe file fd
	lea	rsi, [elf_header]	# rsi = elf_header buffer
	mov	rdx, 64			# count = 64
	mov	r10, 0			# offset = null
	mov	rax, SYS_PREAD64	# Reread the ELF header.
	syscall

	jmp	sharedbegin
.global elf_binary_end
elf_binary_end:
	movzx   eax, WORD PTR elf_header[rip+58]
	movzx   edx, WORD PTR elf_header[rip+60]
	imul	eax, edx
	add     eax, DWORD PTR elf_header[rip+40]
	ret
.global calculate_crc32c
calculate_crc32c:			     # crc32c(rdi=data, rsi=bytesz):
	mov	eax, -1			     #   crc initialized to all ones
	test	rsi, rsi		     #   if size == 0
	jz	ret0			     #   then return zero
	mov	rcx, rsi		     #   rcx = len
	shr	rcx, 3			     #   blocks = len / 8
	and	rsi, 7			     #   rsi = len % 8 (tail size)
	test	rcx, rcx		     #   if blocks == 0
	jz	calculate_crc32c.tail4	     #   then jump past loop8
calculate_crc32c.loop8:			     # loop8:
	crc32	rax, qword ptr [rdi]	     #   process 8 bytes
	add	rdi, 8			     #   src += 8
	dec	rcx			     #   blocks--
	jnz	calculate_crc32c.loop8	     #   if remaining blocks, loop
calculate_crc32c.tail4:			     # tail4:
	test	rsi, 4			     #   if len & 4 == 0
	jz	calculate_crc32c.tail2	     #   then goto tail2
	crc32	eax, dword ptr [rdi]	     #   process 4 bytes
	add	rdi, 4			     #   src += 4
calculate_crc32c.tail2:			     # tail2:
	test	rsi, 2			     #   if len & 2 == 0
	jz	calculate_crc32c.tail1	     #   then goto tail1
	crc32	eax, word ptr [rdi]	     #   process 2 bytes
	add	rdi, 2			     #   src += 2
calculate_crc32c.tail1:			     # tail1:
	test	rsi, 1			     #   if len & 1 == 0
	jz	calculate_crc32c.done	     #   then goto done
	crc32	eax, byte ptr [rdi]	     #   process 1 byte
calculate_crc32c.done:			     # done:
	not	eax			     #   eax = eax xor 0xffffffff
	ret
match:                                      # match: rdi/law
	arity_	r8, rdi                     #   r8  = law.arity
	body_	rax, rdi                    #   rax = law.body
	jnkal	rax, match.none             #   if (body !~ 0[a b]) goto none
	ix0_	rdx, rax                    #   rdx = body.ix(0)
	jnpin	rdx, match.none             #   if (rdx !~ <i>) goto none
	unpin	rdx                         #   rdx = rdx.item
	jheap	rdx, match.none             #   if !direct(rdx) goto none
	ix1	rax                         #   x = body.ix(1)
	mov	rcx, r8                     #   i = arity
match.loop:                                 # loop: rdx/op r8/args rax/x rcx/i
	test	rcx, rcx                    #   if (i == 0)
	jz	match.head                  #     head
	jnkal	rax, match.none             #   if !(x ~ 0[a b]) goto none
	ix1_	r9, rax                     #   r9=b
	cmp	r9, rcx                     #   if (b != i)
	jne	match.none                  #     goto none
	ix0	rax                         #   x=a
	dec	rcx                         #   i--
	jmp	match.loop                  #   goto loop
match.head:                                 # head: rdx/op r8/arity rax/head
	jdirect	rax, match.noquote          #   if direct(x) goto noquote
	jnquo	rax, match.none             #   if !(x ~ 0[k]) goto none
	dref	rax                         #   x = k
match.found:                                # found:
	ret                                 #   return
match.noquote:                              # noquote: rdx/op r8/arity rax/hd
	cmp	rax, r8                     #   if (x > arity)
	ja	match.found                 #     then goto found
match.none:                                 # none:
	xor	r8d, r8d                    #   arity=0
	xor	eax, eax                    #   key=0
	xor	edx, edx                    #   op=0
	ret                                 #   return
.macro PTIME60 dst, src
	mov	\dst, [\src+0]		     #   rax = tv_sec (time_t)
	mov	r12, [\src+8]		     #   r12 = tv_nsec (long)
	imul	\dst, \dst, 125000000	     #   ticks = tv_sec * (1e9 / 8)
	shr	r12, 3			     #   tv_nsec >>= 3
	add	\dst, r12		     #   sum ticks
	mov	r12, 60			     #   literal 60
	bzhi	\dst, \dst, r12		     #   mask 60 bits.
.endm
.set	PROFILE_SIZE,	4088
push_profile:				    # push_profile:
	mov	rdi, PROFILE_SIZE	    #   rdi = PROFILE_SIZE
	mov	rsi, 0x004		    #   rsi = closure tag
	call	buddy_malloc		    #   buddy_malloc(PROFILE_SIZE, 4)
	test	rax, rax		    #   if malloc returned null
	js	push_profile.oom	    #   then handle oom
push_profile.prepareclosure:		    # prepareclosure:
	mov	r8, [profile_base]	    #   check current profile_base
	test	r8, r8			    #   if profile_base == NULL
	jz	push_profile.write	    #   then skip cleanup
	push	rax			    #   save rax
	call	zero_remaining_profile	    #   zero out remaining profile page
	pop	rax			    #   restore rax
	mov	r9, 0xD050000000000000	    #   write correct header
	or	r8, r9			    #   or header with profile_base ptr
push_profile.write:			    # write
	mov	qword ptr [rax], 5	    #   Set hd to 5
	mov	[rax + 8], r8		    #   5[profile_base a1 a2 b1 b2...]
	mov	[profile_base], rax	    #   save new profile_base
	mov	rcx, rax		    #   end = start
	add	rcx, PROFILE_SIZE*8	    #   end = start + size
	mov	[profile_end], rcx	    #   set end
	add	rax, 16			    #   increment to first entry
	mov	[profile_next], rax	    #   set next write location
	ret
push_profile.oom:
	ud2
.global zero_remaining_profile
zero_remaining_profile:			    # zero_remaining_profile:
	mov	rdi, [profile_next]	    #   rdi = profile_next
	mov	rcx, [profile_end]	    #   rcx = profile_end
	sub	rcx, rdi		    #   rcx = remaining space
	shr	rcx, 3 			    #   bytes to words
	xor	eax, eax		    #   store 0
	rep	stosq			    #   memset
	ret				    #   return
### TODO: This calls the SYS_clock_gettime call. YOU REALLY DO NOT WANT TO DO
### THIS IN PRODUCTION CODE. This introduces SYSCALL OVERHEAD to a common
### action we're doing all the time. The correct thing to do is to call the
### VDSO version of this, but that's going to require parsing the auxiliary
### vector at startup, then do minimal ELF parsing to find lookup the pointer
### to `__vdso_clock_gettime`. That's all hard. So for getting from 0 to 1, we
### punt and do the wrong thing. But the data is going to be quasi-invalid
### until we do that.
.global recordevent
recordevent:                                # recordevent(rdi=tag, rsi=2nd val):
	sub	rsp, 32			    #   stack space for timespec*
	mov	[rsp + 16], rdi		    #   save tag
	mov	[rsp + 24], rsi		    #   save 2nd val
recordevent.check:			    # check free space:
	mov	rax, [profile_next]	    #   get current next pointer
	add	rax, 16			    #   increment by 2 64bit words.
	mov	rcx, [profile_end]	    #   get end pointer
	cmp	rax, rcx		    #   if (next >= profile_end)
	jae	recordevent.overflow	    #   then handle overflow
recordevent.askfortime:			    # ask kernel for time:
	mov	rax, 228		    #   SYS_clock_gettime
	mov	rdi, 1			    #   CLOCK_MONOTONIC
	mov	rsi, rsp		    #   rsi = &timespec
	syscall				    #   call kernel
recordevent.record:			    # record record:
	PTIME60	rax, rsp		    #   timespec to 60-bit 8ns time.
	shl	rax, 3			    #   make room for tag
	mov	rdi, [rsp + 16]		    #   rdi = input tag
	or	rax, rdi		    #   OR time with tag
	mov	rcx, [profile_next]	    #   get current pointer
	mov	[rcx], rax		    #   low word: time+type tag
	mov	rdi, [rsp + 24]		    #   retrieve 2nd val
	mov	[rcx + 8], rdi		    #   high word: law pointer
	lea	rcx, [rcx + 16]		    #   advance next pointer by 16
	mov	[profile_next], rcx	    #   save updated pointer
	add	rsp, 32			    #   restore stack frame
	ret                                 #
recordevent.overflow:                       # overflow:
	ppush	rdi, rsi                    #   save input during allocation
	call	push_profile		    #   new profile frame
	ppop	rdi, rsi                    #   restore input
	jmp	recordevent.askfortime	    #   continue in before path
opprofile:				    # opprofile(rdi=law rsi=ret val):
	cmp	qword ptr [profile_next], 0 #   if profiling is enabled
	jne	opprofile.start		    #   then jump to profile handling
opprofile.default:                          # default:
	mov	rax, rsi		    #   setup 2nd argument
	JRAX				    #   evaluate 2nd argument
opprofile.start:			    # start:
	jdirect	rdi, opprofile.savespace    #   if direct, start immediately
	ptr_	rcx, rdi                    #   rcx = PTR(rdi)
	cmp	rcx, r14                    #   if (1st < heap_end)
	jb	opprofile.checkincheney     #   then check if in cheney heap
opprofile.savespace:                        # save space:
	sub	rsp, 16			    #   stack space for two
	mov	[rsp], rdi		    #   save law pointer
	mov	[rsp + 8], rsi		    #   save eval thunk
opprofile.recordbegin:                      # record begin:
	mov	rsi, rdi		    #   rsi = "2nd val"
	mov	rdi, 0			    #   rdi = type tag 0 (begin)
	call	recordevent		    #   recordevent(0, law ptr)
opprofile.eval:				    # eval:
	mov	rsi, [rsp + 8]		    #   get saved thunk
	EVAL	rsi			    #   evaluate it
	mov	[rsp + 8], rsi		    #   put saved evaluation
opprofile.recordend:                        # record end:
	mov	rsi, [rsp]		    #   rsi = "2nd val"
	mov	rdi, 1			    #   rdi = type tag 1 (end)
	call	recordevent		    #   recordevent(0, law ptr)
opprofile.cleanup:                          # cleanup:
	mov	rax, [rsp + 8]		    #   retrieve result
	add	rsp, 16			    #   undo stack space
	JRAX				    #   evaluate 2nd argument
opprofile.checkincheney:                    # check in cheney:
	cmp	rcx, [heap_addr]            #   if (PTR(rdi) >= heap_addr)
	jae	opprofile.default           #   then can't record value
	jmp	opprofile.savespace         #   otherwise continue recording
SetProfEnabledOp:			    # SetProfEnabledOp(rdi=enabled):
	test	rdi, rdi		    #   if (rdi == 0)
	jz	SetProfEnabledOp.disable    #   then disable requested
SetProfEnabledOp.enable:		    # enable:
	cmp	qword ptr [profile_next], 0 #   if profiling already enabled
	jne	ret0			    #   then just return 0
	call	push_profile		    #   push a profile frame to start
	ret				    #   done
SetProfEnabledOp.disable:		    # disable:
	mov	qword ptr [profile_base], 0 #   profile_base = null
	mov	qword ptr [profile_next], 0 #   profile_next = null
	mov	qword ptr [profile_end], 0  #   profile_end = null
	ret				    #   let gc collect dead frames
GetProfOp:				    # GetProfOp(rdi=()):
	cmp	qword ptr [profile_next], 0 #   if profiling is enabled
	jne	GetProfOp.enabled	    #   then jump to profile handling
	mov	rax, 0			    #   else return 0
	ret				    #   return
GetProfOp.enabled:			    # enabled:
	call	zero_remaining_profile	    #   zero the remaining profile
	mov	rax, [profile_base]	    #   get current closure base ptr.
	mov	r8, 0xD050000000000000	    #   write correct header
	or	rax, r8			    #   make rax tagged pointer
	mov	qword ptr[profile_base], 0  #   reset profile_base stack
	push	rax			    #   save result
	call	push_profile		    #   push a new profile buffer
	pop	rax			    #   restore result
	ret				    #   return
.global mmapfile
mmapfile:                                   # rdi=path rsi=statbuf
	push	r12
	push	rbx
	mov	r12, rsi                    # r12=statbuf
	xor	esi, esi                    # flags=0
	xor	edx, edx                    # mode=0
	call	syscall_open_chk            # open(path, flags, mode)
	mov	rbx, rax                    # fd to global
	mov	rdi, rax                    # fd
	mov	rsi, r12                    # statbuf = &seedstat
	call	syscall_fstat_chk           # fstat(fd, statbuf)
	xor	edi, edi                    # addr=0
	mov	rsi, [r12+48]               # len=file.size
	mov	edx, 1                      # prot=1
	mov	ecx, 2                      # flags=2
	mov	r8, rbx                     # fd = *seedfd;
	xor	r9d, r9d                    # off=0
	call	syscall_mmap_chk            # mmap(ptr,len,prot,flags,fd,off)
	mov	r12, rax                    # save buffer
	mov	rdi, rbx                    # fd = *seedfd
	call	syscall_close_chk           # close(rd)
	mov	rax, r12                    # restore result=buffer
	pop	rbx
	pop	r12
	ret                                 # return
.global seedfile # RPN testing system calls this.
seedfile:
	sub	rsp, 160                 # 144+padding
	mov	rsi, rsp                 # rsi = &stat (stack local)
	call	mmapfile                 # rax = ptr
	mov	[rsp+152], rax           # save buf
	mov	rdi, rax                 # buf
	call	seed                     # seed(buf)
	mov	rdi, [rsp+152]           # ptr = buf
	mov	rsi, [rsp+48]            # len = stat.st_size
	mov	[rsp+48], rax            # save result
	call	syscall_munmap_chk       # munmap(ptr, len)
	mov	rax, [rsp+48]            # restore result
	add	rsp, 160                 # restore stack
	ret                              # return
seed:
	mov	r12, rdi
	push	r12
	call	seed.inner
	pop	r12
	ret
seed.inner:
	mov	rcx, [r12]
	lea	rsi, [r12 + 40]
seed.holeloop:
	test	rcx, rcx
	jz	seed.sizes
	xor	edi, edi
	push	rsi
	push	rcx
	push	r12
	call	mkpin
	pop	r12
	pop	rcx
	pop	rsi
	ppush	rax
	dec	rcx
	jmp	seed.holeloop
seed.sizes:
	mov	rcx, [r12 + 8]
seed.sizeloop:
	test	rcx, rcx
	jz	seed.bigs
	mov	r8, [rsi]
	sub	r15, 8
	mov	[r15], r8
	add	rsi, 8
	dec	rcx
	jmp	seed.sizeloop
seed.bigs:
	mov	rcx, [r12 + 8]
seed.bigloop:
	test	rcx, rcx
	jz	seed.words
	dec	rcx
	mov	r8, [r15 + rcx*8]
	mov	rdi, r8
	push	rcx
	push	r8
	push	r12
	call	reserve
	pop	r12
	pop	r8
	mov	rdi, rax
	mov	rcx, r8
	rep	movsq
	push	rsi
	mov	rdi, r8
	call	claim
	pop	rsi
	pop	rcx
	mov	[r15 + rcx*8], rax
	jmp	seed.bigloop
seed.words:
	mov	rcx, [r12 + 16]
seed.wordloop:
	test	rcx, rcx
	jz	seed.bytes
	mov	rax, [rsi]
	push	r12
	call	mkword
	pop	r12
	sub	r15, 8
	mov	[r15], rax
	add	rsi, 8
	dec	rcx
	jmp	seed.wordloop
seed.bytes:
	mov	rcx, [r12 + 24]
seed.byteloop:
	test	rcx, rcx
	jz	seed.frags
	movzx	r8, byte ptr [rsi]
	sub	r15, 8
	mov	[r15], r8
	inc	rsi
	dec	rcx
	jmp	seed.byteloop
seed.frags:
	mov	r8, rsi
	shr	r8, 3
	shl	r8, 3
	mov	rdx, [r12 + 24]
	mov	rdi, rdx
	add	rdi, [r12 + 0]
	add	rdi, [r12 + 8]
	add	rdi, [r12 + 16]
	shl	rdx, 61
	shr	rdx, 58
	mov	rcx, [r12 + 32]
	call	frags
	mov	rax, [r15]
	mov	rcx, [r12]
	add	rcx, [r12 + 8]
	add	rcx, [r12 + 16]
	add	rcx, [r12 + 24]
	add	rcx, [r12 + 32]
	lea	r15, [r15 + rcx*8]
	ret
frags:                                      # rdi=en rsi=e rdx=boff r8=wp rcx=n
	push	rbx
	push	rbp
	push	r12
	xor	rbx, rbx                    #   i=0 in rbx
	mov	rbp, rdi                    #   en in rdi
	mov	r12, rcx                    #   n in r12
frags.loop:                                 # continue:
	cmp	rbx, r12                    #   if (i >= n)
	jae	frags.ret                   #     break
	lea	rdi, [rbp-1]
	lzcnt	r9, rdi
	mov	rdi, 64
	sub	rdi, r9                     #   bitsz = 64 - lzcnt(sz-1)
	mov	rsi, r15                    #   rsi = sp
	call	frag                        #   frag(bitsz, rsi)
	inc	rbp                         #   i++
	inc	rbx                         #   en++
	jmp	frags.loop                  #   contiue
frags.ret:                                  # break:
	pop	r12
	pop	rbp
	pop	rbx
	ret                                 #   return
# state: rdi=refsz rsi=env rdx=used r8=ptr
# local: r10=word rcx=tmp r9=params
frag:                                       # frag:
	push	r12                         #   avoid clobbering r12
	call	frag.recur                  #   enter recursive logic
	pop	r12                         #   restore r12
	ret                                 #   return
frag.recur:                                 # recur:
	sub	rsp, 32                     #   frame=(params, ptr, refsz, env)
	mov	[rsp+16], rdi               #   save refsz
	mov	[rsp+24], rsi               #   save env
	mov	r10, [r8]                   #   word = ptr[0]
	test	rdx, rdx                    #   if (used == 0)
	jz	frag.size                   #   goto size
	mov	rcx, rdx                    #   (shr only words with rcx)
	shr	r10, rcx                    #   word <<= used
	mov	rcx, 64                     #
	sub	rcx, rdx                    #   remain = 64-used
	mov	r9, [r8 + 8]                #   next = ptr[1]
	shl	r9, rcx                     #
	or	r10, r9                     #   word = (word | next<<remain)
frag.size:                                  # size:
	xor	r9, r9                      #   params = 0
	tzcnt	rcx, r10                    #   szsz = ctz(word)
	shr	r10, rcx                    #
	shr	r10, 1                      #   word >>= szsz+1 (advance)
	inc	rdx                         #   used++ (for 0 case)
	test	rcx, rcx                    #   if (!szsz)
	jz	frag.head                   #     goto head (0 case has no size)
frag.args:                                  # args:
	dec	rdx                         #   used-- (undo inc)
	add	rdx, rcx                    #
	add	rdx, rcx                    #   used += szsz*2
	dec	rcx                         #   swid = (szsz-1)
	xor	r9, r9                      #
	bts	r9, rcx                     #
	dec	r9                          #
	and	r9, r10                     #   params = word&((2**swid)-1)
	bts	r9, rcx                     #   set high bit
	shr	r10, rcx                    #   word>>swid
frag.head:                                  # head: (r10=word r9=params)
	xor	rcx, rcx                    #
	bts	rcx, rdi                    #
	dec	rcx                         #
	and	rcx, r10                    #   rix = word & ((2**refsz)-1)
	mov	rcx, [rsi + rcx*8]          #   ref = env[rix]
	ppush	rcx                         #   push rcx
	add	rdx, rdi                    #   used += refsz
	cmp	rdx, 64                     #   if (used < 64)
	jb	frag.loop                   #     goto loop
	sub	rdx, 64                     #   used -= 64
	add	r8, 8                       #   ptr++
frag.loop:                                  # loop:
	test	r9, r9                      #   if (params == 0)
	jz	frag.break                  #     break
	mov	[rsp], r9                   #   save params
	call	frag.recur                  #   recur()
	mov	[rsp+8], r8                 #   save ptr
	call	mkapp                       #   mkapp()
	mov	r9, [rsp]                   #   restore params
	mov	r8, [rsp+8]                 #   restore ptr
	mov	rdi, [rsp+16]               #   restore refsz
	mov	rsi, [rsp+24]               #   restore env
	dec	r9                          #   params--
	jmp	frag.loop                   #   continue
frag.break:                                 # break:
	add	rsp, 32                     #   pop frame
	ret                                 #   return
.equ	MUTEX_UNLOCKED, 0		    # Mutex is available
.equ	MUTEX_LOCKED, 1			    # Mutex is locked, no waiters
.equ	MUTEX_LOCKED_WAITERS, 2		    # Mutex is locked with waiters
.global mutex_init
mutex_init:				    # mutex_init(rdi=int*):
    mov DWORD PTR [rdi], MUTEX_UNLOCKED	    #   *rdi = MUTEX_UNLOCKED
    ret					    #   return
.global mutex_lock
mutex_lock:				    # mutex_lock(rdi=int*):
	mov	eax, MUTEX_UNLOCKED	    #   eax = expected value (0)
	mov	ecx, MUTEX_LOCKED	    #   ecx = new value (1)
	lock cmpxchg DWORD PTR [rdi], ecx   #   try atomically change 0 to 1
	je	mutex_lock.success	    #   if success, we're done
	push	r12			    #   slow path when locked
	mov	r12, rdi		    #   save the mutex pointer
mutex_lock.wait_loop:			    # wait_loop:
	cmp	eax, MUTEX_LOCKED_WAITERS   #   if already marked with waiters
	je	mutex_lock.futex_wait	    #   then go wait for kernel futex
	mov	eax, MUTEX_LOCKED	    #   eax = expected value (1)
	mov	ecx, MUTEX_LOCKED_WAITERS   #   ecx = new value (2)
	lock cmpxchg DWORD PTR [r12], ecx   #   try atomically change 1 to 2
	jne	mutex_lock.check_unlocked   #   if failed, check if unlocked
mutex_lock.futex_wait:			    # futex_wait:
	mov	rdi, r12		    #   restore rdi
	mov	edx, MUTEX_LOCKED_WAITERS   #   edx = 2
	call	futex_wait_private	    #   futex_wait(mutex*, WAITERS)
mutex_lock.check_unlocked:		    # check unlocked:
	mov	eax, MUTEX_UNLOCKED	    #   eax = expected value (0)
	mov	ecx, MUTEX_LOCKED_WAITERS   #   ecx = new value (2)
	lock cmpxchg DWORD PTR [r12], ecx   #   try atomically change 0 to 2
	jne	mutex_lock.wait_loop	    #   if failed, go back to wait loop
	pop	r12			    #   lock acquired, restore
mutex_lock.success:			    # success:
	ret				    #   return
.global mutex_unlock
mutex_unlock:				    # mutex_unlock(rdi=int*):
	mov	eax, -1			    #   -1 to subtract 1
	lock	xadd DWORD PTR [rdi], eax   #   xadd returns old value in eax
	cmp	eax, MUTEX_LOCKED	    #   if old value was 1 (now 0)
	je	mutex_unlock.done	    #   then no waiters to wake
	mov DWORD PTR [rdi], MUTEX_UNLOCKED #   unlock completely
	mov	edx, FUTEX_WAKE_ONE	    #   call futex to wake one waiter
	call	futex_wake_private	    #   rdi already contains mutex ptr
mutex_unlock.done:
	ret
.global condition_init
condition_init: 			    # condition_init(rdi=cv*, rsi=mx*):
	mov	qword ptr [rdi], rsi	    #   [rdi] = mutex pointer
	mov	dword ptr [rdi + 8], 0	    #   [rdi+8] = sequence var, 0 or 1
	ret
.global condition_signal
condition_signal:			    # condition_signal(rdi=cvar*):
	lock	inc dword ptr [rdi + 8]	    #   increment number of signals
	lea	rdi, [rdi + 8]		    #   wake the sequence var location
	mov	edx, FUTEX_WAKE_ONE	    #   wake only one waiter
	call	futex_wake_private	    #   call futex wake
	ret				    #   done
.global condition_broadcast
condition_broadcast:			    # condition_broadcast(rdi=cvar*):
	mov	r9d, 1			    #   increment by one
	lock xadd dword ptr [rdi + 8], r9d  #   prev value returned in eax
	inc	r9d			    #   r9d is now the new value.
	mov	r8, [rdi]		    #   requeue to the mutex
	lea	rdi, [rdi + 8]		    #   wake on the sequence var
	jmp	futex_cmp_requeue_private   #   tail call futex requeue
.global condition_wait
condition_wait:				    # condition_wait(rdi=cvar*):
	push	r12			    #   save register
	mov	r12d, [rdi + 8]		    #   save current sequence var
	push	rdi			    #   save input cvar*
	mov	rdi, [rdi]		    #   set up mutex pointer
	call	mutex_unlock		    #   mutex_unlock(cvar->mutex)
	pop	rdi			    #   restore cvar*
	push	rdi			    #   save cvar*
	add	rdi, 8			    #   rdi = cvar->seq
	mov	edx, r12d		    #   edx = sequence var expected val
	call	futex_wait_private	    #   call futex wait
	pop	rdi			    #   restore cvar*
	mov	rdi, [rdi]		    #   rdi is now mutex for rest of fun
condition_wait.wakeup_loop:		    # wakeup_loop:
	mov	eax, MUTEX_LOCKED_WAITERS   #   prepare locked+waiters state
	lock xchg DWORD PTR [rdi], eax	    #   grab mutex, mark contended
	test	eax, eax		    #   if mutex was unlocked before
	jz	condition_wait.done	    #   then we now hold mutex
	push	rdi			    #   save mutex pointer
	mov	edx, MUTEX_LOCKED_WAITERS   #   expect mutex to be locked
	call	futex_wait_private	    #   call futex wait
	pop	rdi			    #   restore mutx pointer
	jmp	condition_wait.wakeup_loop  #   continue
condition_wait.done:			    # done:
	pop	r12			    #   restore r12
	ret				    #   return
.global rwlock_init
rwlock_init:				    # rwlock_init(rdi=rwlock*):
	push	r12			    #   save r12
	mov	r12, rdi		    #   save rwlock pointer
	call	mutex_init		    #   initialize front mutex
	lea	rdi, [r12 + 8]		    #   rdi = &rwlock->readers_cv
	lea	rsi, [r12]		    #   rsi = &rwlock->mutex
	call	condition_init		    #   initialize readers cvar
	lea	rdi, [r12 + 24]		    #   rdi = &rwlock->writers_cv
	lea	rsi, [r12]		    #   rsi = &rwlock->mutex
	call	condition_init		    #   initialize writers cvar
	mov	DWORD PTR [r12+40], 0	    #   active_readers = 0
	mov	DWORD PTR [r12+44], 0       #   writer_active = 0
	mov	DWORD PTR [r12+48], 0	    #   waiting_writers = 0
	mov	rdi, r12		    #   restore rwlock pointer
	pop	r12			    #   restore r12
	ret				    #   return
.global rwlock_read_lock
rwlock_read_lock:			    # rwlock_read_lock(rdi=rwlock*):
	push	r12			    #   save r12
	mov	r12, rdi		    #   save rwlock pointer
	call	mutex_lock		    #   lock our mutex
rwlock_read_lock.wait_loop:		    # wait_loop:
	cmp	DWORD PTR [r12 + 44], 0     #   if writer_active == 0
	jz	rwlock_read_lock.no_writers #   then advance to next step
	lea	rdi, [r12 + 8]		    #   rdi = rwlock->readers_cv
	call	condition_wait		    #   wait on readers_cv
	mov	rdi, r12		    #   restore rwlock in rdi
	jmp	rwlock_read_lock.wait_loop  #   try again
rwlock_read_lock.no_writers:		    # no_writers:
	inc	DWORD PTR [r12 + 40]	    #   increment readers atomically
	mov	rdi, r12		    #   rdi = rwlock*
	call	mutex_unlock		    #   mutex_unlock(rwlock*)
	pop	r12			    #   restore r12
	ret				    #   return
.global rwlock_read_unlock
rwlock_read_unlock:			    # rwlock_read_unlock(rdi=rwlock*):
	push	r12			    #   save r12
	mov	r12, rdi		    #   save rwlock pointer
	call	mutex_lock		    #   mutex_lock(rwlock*)
	dec	DWORD PTR [r12 + 40]	    #   decrement active_readers
	jnz	rwlock_read_unlock.done     #   if active_readers, then done
	cmp	DWORD PTR [r12 + 48], 0     #   if waiting_writers == 0
	jz	rwlock_read_unlock.done     #   then done
	lea	rdi, [r12 + 24]		    #   rdi = &mutex->writers_cv
	call	condition_signal	    #   signal one writer
rwlock_read_unlock.done:		    # done:
	mov	rdi, r12		    #   rdi = rwlock*
	call	mutex_unlock		    #   unlock the mutex
	pop	r12			    #   restore r12
	ret
.global rwlock_write_lock
rwlock_write_lock:			    # rwlock_write_lock(rdi=rwlock*):
	push	r12			    #   save r12
	mov	r12, rdi		    #   save rwlock pointer
	call	mutex_lock		    #   lock the mutex
	inc	DWORD PTR [r12 + 48]	    #   increment waiting writers
rwlock_write_lock.wait_loop:		    # wait_loop:
	cmp	DWORD PTR [r12 + 40], 0     #   if there are active readers
	jnz	rwlock_write_lock.wait	    #   then wait
	cmp	DWORD PTR [r12 + 44], 0     #   if there's no writer active
	jz	rwlock_write_lock.done	    #   then finish up and take lock
rwlock_write_lock.wait:			    # wait:
	lea	rdi, [r12 + 24]		    #   rdi = &rwlock->writers_cv
	call	condition_wait		    #   wait on writers_cv
	mov	rdi, r12		    #   restore rdi
	jmp	rwlock_write_lock.wait_loop #   try again
rwlock_write_lock.done:			    # done:
	mov	DWORD PTR [r12 + 44], 1     #   mark writer as active
	dec	DWORD PTR [r12 + 48]	    #   decrement waiting writers
	mov	rdi, r12		    #   unlock mutex
	call 	mutex_unlock		    #   mutex_unlock(rdi)
	pop	r12			    #   restore r12
	ret				    #   return
.global rwlock_write_unlock
rwlock_write_unlock:			    # rwlock_write_unlock(rdi=rwlock*):
	push	r12			    #   save r12
	mov	r12, rdi		    #   save rwlock pointer
	call	mutex_lock		    #   lock the mutex
	mov	DWORD PTR [r12 + 44], 0	    #   mark writer as inactive
	cmp	DWORD PTR [r12 + 48], 0     #   if waiting_writers == 0
	jz	rwlock_write_unlock.signal  #   then signal readers
	lea	rdi, [r12 + 24]		    #   rdi = &rwlock->writers_cv
	call	condition_signal	    #   signal one writer
	jmp	rwlock_write_unlock.done    #   we woke the next writer
rwlock_write_unlock.signal:		    # signal:
	lea	rdi, [r12 + 8]		    #   rdi = &rwlock->readers_cv
	call	condition_broadcast	    #   broadcast to all readers
rwlock_write_unlock.done:		    # done:
	mov	rdi, r12		    #   rdi = rwlock*
	call	mutex_unlock		    #   unlock the mutex
	pop	r12			    #   restore r12
	ret
.global barrier_set
barrier_set:				    # barrier_set(rdi=barrier*,rsi=cnt):
    lock xchg [rdi], rsi		    #   *barrier = count
    ret					    #   return
.global barrier_done
barrier_done:				    # barrier_done(rdi=barrier*):
	mov	eax, -1			    #   decrement by 1
	lock xadd dword ptr [rdi], eax	    #   locked perform subtract
	cmp	eax, 1			    #   if we aren't last thread
	jne	barrier_done.done	    #   then don't wake anyone
	mov	edx, FUTEX_WAKE_ALL	    #   otherwise wake everyone
	call	futex_wake_private	    #   call futex
barrier_done.done:
	ret
.global barrier_wait
barrier_wait:				    # barrier_wait(rdi=barrier*):
	mov	edx, [rdi]		    #   current number of waiters
	test	edx, edx		    #   if this is zero
	jz	barrier_wait.done	    #   then we have don't have to wait
	push	rdi			    #   save barrier*
	call	futex_wait_private	    #   call futex_wait
	pop	rdi			    #   restore barrier*
	jmp	barrier_wait		    #   try again
barrier_wait.done:
	ret
.equ    CLONE_FLGS, CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND | CLONE_THREAD
.equ    EIGHTMB,    8 * 1024 * 1024

.global thread_create
thread_create:                              # thread_create(rdi, rsi, rdx):
	push	rbp			    #   frame header
	mov	rbp, rsp		    #   establish frame
	push	r12                         #   save callee-saved
	mov	r12, rdi                    #   r12 = thread*
	mov	qword ptr [r12], 0          #   tid = 0 (init structure)
	mov	qword ptr [r12 + 8], 0      #   futex exit_flag = 0
	mov	qword ptr [r12 + 16], rsi   #   function pointer to run
	mov	qword ptr [r12 + 24], rdx   #   argument to function pointer
	mov	qword ptr [r12 + 32], 0     #   return value storage
	mov	qword ptr [r12+48], EIGHTMB #   stack size
	mov	rax, SYS_MMAP		    # 	 set up MMAP call for stack
	mov	rdi, 0                      #   addr = NULL
	mov	rsi, [r12 + 48]             #   size = stack_size
	mov	rdx, PROT_READ|PROT_WRITE   #   prot = read/write
	mov	r10, MAP_PRIVATE|MAP_ANONYMOUS|MAP_STACK
	mov	r8, -1                      #   no file descriptor backing
	mov	r9, 0                       #   offset = 0
	syscall				    #   mmap(...)
	cmp	rax, 0			    #   if return mmap < 0
	jl	thread_create.error	    #   then handle error
	mov	[r12 + 40], rax             #   stack_base = mmap addr
thread_create.do:			    # do:
	mov	rsi, [r12 + 40]		    #   base child stack pointer
	add	rsi, [r12 + 48]             #   base + size = top of stack
	mov	rax, SYS_CLONE		    #   SYS_CLONE
	mov	rdi, CLONE_FLGS             #   set clone flags
	xor	rdx, rdx                    #   ptid = NULL
	xor	r10, r10                    #   ctid = NULL
	xor	r8, r8                      #   newtls = 0
	syscall				    #   clone(flags, child_stack, ...)
	cmp	rax, 0			    #   test clone return value
	jl	thread_create.error	    #   rax < 0: error
	je	thread_create.start         #   child path (clone returns 0)
	mov	[r12], rax		    #   parent path:store rax=chld tid
	pop	r12			    #   restore r12
	mov	rax, 0			    #   return success
	leave				    #   restore stack
	ret				    #   return
thread_create.error:			    # error:
	leave				    #   restore stack
	ud2				    #   kill process
thread_create.start:			    # start:
	xor	rbp, rbp		    #   clear frame pointer for new thrd
	and	rsp, -16		    #   ensure 16-byte stack alignemnt
	mov	rdi, [r12 + 24]		    #   rdi = arg to function pointer
	push	r12			    #   save r12
	call	[r12 + 16]		    #   call function pointer
	pop	r12			    #   restore r12
	mov	[r12 + 32], rax		    #   save function result
	mov	eax, 1			    #   set exit mark
	lock xchg dword ptr [r12 + 8], eax  #   mark thread as exited
	lea	rdi, [r12 + 8]		    #   rdi = atomic exit_flag
	mov	edx, FUTEX_WAKE_ONE	    #   wake a single waiter
	call	futex_wake_private	    #   futex_wake()
	mov	rdi, rax		    #   function return val as exit code
	call	syscall_exit     	    #   and call exit
.global thread_join
thread_join:				    # thread_join(rdi, rsi):
	cmp 	dword ptr [rdi + 8], 0      #   if futex exit_flag is not set
	jz	thread_join.futex_wait	    #   then wait for thread
	test	rsi, rsi		    #   if no result pointer
	jz	thread_join.munmap_stack    #   then skip setting result
	mov	rax, [rdi + 32]		    #   get result
	mov	[rsi], rax		    #   set result
thread_join.munmap_stack:		    # munmap_stack:
	mov	rdi, [rdi + 40]		    #   rdi = stack pointer
	mov	rsi, [rdi + 48]		    #   rsi = stack size
	mov	rax, SYS_MUNMAP		    #   munmap
	syscall				    #   munmap(rdi, rsi)
	ret				    #   return
thread_join.futex_wait:			    # futex_wait:
	push	rdi			    #   save rdi
	push	rsi			    #   save rsi
	lea	rdi, [rdi + 8]		    #   wait on futex exit_flag
	mov	edx, 0			    #   expect 0
	call	futex_wait_private	    #   futex_wait()
	pop	rsi			    #   restore rsi
	pop	rdi			    #   restore rdi
	jmp	thread_join		    #   try again after futex wake
futex_wait_private:			    # futex_wait_private(rdi, edx):
	mov	rax, SYS_FUTEX		    #   rax = futex syscall
	mov	esi, FUTEX_WAIT|FUTEX_PRIVATE_FLAG
	xor	r10, r10		    #   timeout = NULL (wait forever)
	xor	r8, r8			    #   r8 unused
	xor	r9, r9			    #   r9 unused
	syscall                             #   rdi and edx already set
	ret                                 #   return
futex_wake_private:			    # futex_wake_private(rdi, edx):
	mov	rax, SYS_FUTEX		    #   rax = futex syscall
	mov	esi, FUTEX_WAKE|FUTEX_PRIVATE_FLAG
	xor	r10, r10		    #   r10 ununused
	xor	r8, r8			    #   r8 unused
	xor	r9, r9			    #   r9 unused
	syscall				    #   rdi and edx already set
	ret				    #   return
futex_cmp_requeue_private: 		    # futex_cmp_requeue_private(rdir8r9)
	mov	rax, SYS_FUTEX		    #   rax = futex syscall
	mov	esi, FUTEX_CMP_REQUEUE|FUTEX_PRIVATE_FLAG
	mov	edx, 1			    #   wake exactly one waiter
	mov	r10, 0x7fffffff		    #   requeue all other waiters
	syscall				    #   r8 and r9d already set
	ret				    #   return
