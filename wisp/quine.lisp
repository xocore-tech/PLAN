(require "lambda")
;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

; This linearizes a PLAN tree into a flat list.  The resulting list
; is essential a list of RPN commands which can be used to reconstruct
; the tree.
;
; This is a somewhat unconventional structure, but the goal here is to
; make it easier to traverse a PLAN tree in a state monad using tail
; recursive code in order to have tighter control over memory usage by
; maintaining an explicit stack
;
; Earlier attempts at interning a PLAN value into a seed struggled with
; memory usage, and this approach solved that.

; <0> -> H[0]
; <3> -> (<0> 3) -> H[0] A[3] C
; <x> -> (<0> 3) -> [H[0] (ival x ~[C])]
;
; {n a b} -> (| H | A 1 | C | go n | A a | go b
; <3> -> (<0> 3) -> H A[3] C
; <x> -> (<0> 3) -> [H (ival x ~[C])]
;
; 0 _[x y z] -> x C y C z C
;
; f[x y z] -> f x C y C z C

(def E 0)
(def Z 1)
(def C 2)
(def A 3)

(defun (ipin ival k item)
  (Ifz item (Z k)
    (Z (ival item (C k)))))

(defun (ilaw ival k n a b)
  (Z (A 1 (C (A n (C (A a (C (ival b (C k))))))))))

(defun (irow ival o i k)
  (If (Ge i (Sz o)) k
    (ival (Ix i o) (C (irow ival o (Inc i) k)))))

(defun (iapp ival o k _ _)
  (ival (Hd o) (irow ival o 0 k)))

(defun (ival o k)
  (Eat
    (ipin ival k)
    (ilaw ival k)
    (iapp ival o k)
    (A o k)
    {_ -> (A o k)}
    o))

(def ##0 (Pin 0))
(def ##2 (Pin 2))
(def ##3 (Pin 3))

(defun (succ x)
  (##2 x))

(tests
  ((ival 3          E)
   (A 3 E))

  ((ival ##3        E)
   (Z (A 3 (C E))))

  ((ival succ       E)
   (Z (Z (A 1 (C (A "succ" (C (A 1 (C (A 0 (Z (A 2 (C (C (A 1 (C (C (C E))))))))))))))))))

  ((ival (3 4)      E)
   (A 3 (A 4 (C E))))

  ((ival (3 4 5)    E)
   (A 3 (A 4 (C (A 5 (C E))))))

  ((ival {0 x => x} E)
   (Z (A 1 (C (A 0 (C (A 1 (C (A 1 (C E)))))))))))

(defun (cell [x [f more]])
  (let ((fx (f x)))
    (Seq fx [fx more])))

(defun (spair a b)
  (Seq2 a b
    [a b]))

(defun (rebuild stack code)
  (Seq stack
    (Ifz (Hd code) stack
      (If (Eq 1 (Hd code))   (rebuild (spair ##0 stack)        (Ix0 code))
        (If (Eq 2 (Hd code)) (rebuild (cell stack)             (Ix0 code))
                             (rebuild (spair (Ix0 code) stack) (Ix1 code)))))))

(tests
  ( (rebuild 0 (ival (3 3)           E))  [(3 3) 0]           )
  ( (rebuild 0 (ival (##3 1)         E))  [(##3 1) 0]         )
  ( (rebuild 0 (ival (##3)           E))  [##3 0]             )
  ( (rebuild 0 (ival {0 x => x}      E))  [{0 x => x} 0]      )
  ( (rebuild 0 (ival Inc             E))  [Inc 0]             )
  ( (rebuild 0 (ival MkPin{I x => x} E))  [MkPin{I x => x} 0] ))

(tests
  ([main 0] (rebuild 0 (ival main E))))

(defun (count acc code)
  (Seq acc
    (Ix (Hd code)
      [acc
       (count acc       (Ix0 code))
       (count (Inc acc) (Ix0 code))
       (count acc       (Ix1 code))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tests
  ( (rebuild 0 (ival env0 E))       [env0 0]   )
  ( (rebuild 0 (ival read E))       [read 0]   )
  ( (rebuild 0 (ival opeval E))     [opeval 0] )
  ( (rebuild 0 (ival main E))       [main 0]   )
  ( (count 0 (ival main E))         31691      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sbEmpty 0)
(def sbSing 1)   ; (sbSing k v) => 1[k v]
(def sbSz Hd)

(defun (sb k v l r)
  (let* ((sz (Inc (Add (sbSz l) (sbSz r)))))
    (sz k v l r)))

(def N sb)
(def E 0)

(defun (L k v) (1 k v 0 0))

(defun (singleL key val a right[koy vol b c])
  (sb koy vol (sb key val a b) c))

(defun (singleR key val [koy vol a b] c)
  (sb koy vol a (sb key val b c)))

(defun (doubleL key val a right[rkey rval son[skey sval b c] d])
  (sb skey sval (sb key val a b) (sb rkey rval c d)))

(defun (doubleR key val left[lkey lval a son[skey sval b c]] d)
  (sb skey sval (sb lkey lval a b) (sb key val c d)))

(tests
  ((singleL 7 7 E (L 8 8))
   (N 8 8 (L 7 7) E))

  ((singleR 7 7 (L 6 6) E)
   (N 6 6 E (L 7 7)))

  ((singleL 7 7 E (N 8 8 E (L 9 9)))
   (N 8 8 (L 7 7) (L 9 9)))

  ((singleL 5 5 E (N 8 8 (L 7 7) (L 9 9)))
   (N 8 8 (N 5 5 E (L 7 7)) (L 9 9)))

  ((doubleL 5 5 E (N 8 8 (L 7 7) (L 9 9)))
   (N 7 7 (L 5 5) (N 8 8 E (L 9 9))))

  ((doubleR 5 5 (N 2 2 (L 1 1) (L 3 3)) E)
   (N 3 3 (N 2 2 (L 1 1) E) (L 5 5))))

(defun (rotateL k v l r[_ _ rl rr])
  (If (Lt (sbSz rl) (Mul 2 (sbSz rr)))
    (singleL k v l r)
    (doubleL k v l r)))

(defun (rotateR k v l[_ _ ll lr] r)
  (If (Lt (sbSz lr) (Mul 2 (sbSz ll)))
    (singleR k v l r)
    (doubleR k v l r)))

(defun (balance k v l r)
  (let* ((lsz (sbSz l))
         (rsz (sbSz r))
         (xsz (Inc (Add lsz rsz)))
         (top (xsz k v l r)))
    (If (Le (Add lsz rsz) 1) top
      (If (Ge rsz (Mul 4 lsz)) (rotateL k v l r)
        (If (Ge lsz (Mul 4 rsz)) (rotateR k v l r)
          top)))))

(defun (sbInsert key val x[xkey xval left right])
  (Ifz x (sbSing key val 0 0)
    (Ix (Cmp key xkey)
      [(balance xkey xval (sbInsert key val left) right)
       ((sbSz x) key val left right)
       (balance xkey xval left (sbInsert key val right))])))

(defun (sbSearch key tree[k v l r])
  (Ifz tree 0
    (Case3 (Cmp key k)
      (sbSearch key l)
      [v]
      (sbSearch key r))))

(defun (sbFromListLoop acc list[[k v] ls])
  (Seq acc
    (Ifz list acc
      (sbFromListLoop (sbInsert k v acc) ls))))

(def sbFromList (sbFromListLoop 0))

(defun (derp n)
  (sbFromList (stream (gen n {x -> (let ((xx (Mul (Inc x) 11)))
                                     [xx xx])}))))

(tests
  ((sbSearch 11 (derp 0)) [])
  ((sbSearch 11 (derp 1)) [11])
  ((sbSearch 11 (derp 2)) [11])
  ((sbSearch 11 (derp 3)) [11])
  ((sbSearch 11 (derp 4)) [11])
  ((sbSearch 11 (derp 5)) [11])
  ((sbSearch 11 (derp 6)) [11])
  ((sbSearch 66 (derp 6)) [66])
  ((sbSearch 77 (derp 6)) []))

(tests
  ((derp 2) (N 11 11 E (L 22 22)))
  ((derp 3) (N 22 22 (L 11 11) (L 33 33)))
  ((derp 4) (N 22 22 (L 11 11) (N 33 33 E (L 44 44))))
  ((derp 5) (N 22 22 (L 11 11) (N 44 44 (L 33 33) (L 55 55)))))

(defun (NN x l r) (N x x l r))
(defun (LL x)     (L x x))

(tests
  ((NN 44 (NN 22 (LL 11) (LL 33))
          (NN 55 E (LL 66)))
   (derp 6))

  (((NN 44) ((NN 22) (LL 11) (LL 33)) ((NN 66) (LL 55) (LL 77)))
   (derp 7))

  ((sbInsert 2 22 0)
   (sbSing 2 22 0 0)))

(def ex (NN 4 (LL 2) (LL 6)))

(tests
  ((sbInsert 4 "four" ex)
   (N 4 "four" (L 2 2) (L 6 6)))

  ((sbInsert 2 "two" ex)
   (N 4 4 (L 2 "two") (L 6 6)))

  ((sbInsert 6 "six" ex)
   (N 4 4 (L 2 2) (L 6 "six")))

  ((sbInsert 1 "one" ex)
   (N 4 4 (N 2 2 (L 1 "one") E) (L 6 6))))

(tests
  ((sbInsert 2 "two" ex)   (NN 4 (L 2 "two")              (LL 6)))
  ((sbInsert 3 "three" ex) (NN 4 ((NN 2) E (L 3 "three")) (LL 6)))
  ((sbInsert 5 "fiv" ex)   (NN 4 ((NN 2) E E)             (NN 6 (L 5 "fiv") E)))
  ((sbInsert 6 "six" ex)   (NN 4 (LL 2)                   (L 6 "six")))
  ((sbInsert 7 "sev" ex)   (NN 4 (LL 2)                   (NN 6 E (L 7 "sev")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (deleteFindMax t[k v l r])
  (let* (([km r2] (deleteFindMax r)))
    (Ifz r [[k v] l]
      [km (balance k v l r2)])))

(defun (deleteFindMin t[k v l r])
  (let* (([km l2] (deleteFindMin l)))
    (Ifz l [[k v] r]
      [km (balance k v l2 r)])))

(defun (glue l r)
  (let* (([[lkm lm] l2] (deleteFindMax l))
         ([[rkm rm] r2] (deleteFindMin r)))
    (Ifz l r
      (Ifz r l
        (If (Gt (sbSz l) (sbSz r))
          (balance lkm lm l2 r)
          (balance rkm rm l r2))))))

(defun (sbAlter f key x[xsz xkey xval l r])
  (let ((f0  (f 0))
        (fxv (f [xval])))
    (Ifz x
      (Ifz f0
        sbEmpty
        (sbSing key (Ix0 f0)))
      (Case3 (Cmp key xkey)
        (balance xkey xval
          (sbAlter f key l) r)
        (Ifz fxv (glue l r)
          (xsz xkey (Ix0 fxv) l r))
        (balance xkey xval l
          (sbAlter f key r))))))

; alterKnown alters the value associated with a key which is known to
; be present, so the function is not passed a maybe.

(defun (sbAlterKnown f key x[xkey xval l r])
  (let ((xsz (Hd x)))
    (Ifz x sbEmpty
      (Case3 (Cmp key xkey)
        (balance xkey xval (sbAlterKnown f key l) r)
        (xsz xkey (f xval) l r)
        (balance xkey xval l (sbAlterKnown f key r))))))

(defun (sbDel key tree[xkey xval l r])
  (let ((xsz (Hd tree)))
    (Ifz tree tree
      (Case3 (Cmp key xkey)
        (balance xkey xval (sbDel key l) r)
        (glue l r)
        (balance xkey xval l (sbDel key r))))))

(defun (sbSearchCase k t nf f)
  (let ((r (sbSearch k t)))
    (Ifz r nf
      (f (Ix0 r)))))

(defun (sbSearchKnown k sb)
  (Ix0 (sbSearch k sb)))

(defun (lcatRows list-of-rows)
  (lcat (lmap stream list-of-rows)))

(tests ((lcatRows ~[[3 4] 0 [5 6 7]])
        ~[3 4 5 6 7] ))

(defun (sbWalk sb[k v l r])
  (And sb
  (lweld (sbWalk l)
  (lcons sb
  (sbWalk r)))))

(defun (sbToPairsList sb)
  (lmap (take 2) (sbWalk sb)))

(defun (sbSave tree) ; to [k v k v k v] format
  (Row 0 (Lsh (sbSz tree) 1)
    (lcatRows (sbToPairsList tree))))

(tests
  ( [1 1 5 5 9 9]
    (sbSave (sb 5 5 (L 1 1) (L 9 9)))))

(defun (sb-keys-list tree) (lmap Ix0 (sbWalk tree)))
(defun (sb-vals-list tree) (lmap Ix1 (sbWalk tree)))
(defun (sb-vals-row tree)  (Row 0 (sbSz tree) (sb-vals-list tree)))
(defun (sb-keys-row tree)  (Row 0 (sbSz tree) (sb-keys-list tree)))

(tests
  ([1 5 9] (sb-vals-row (sb 1 5 (L 0 1) (L 6 9))))
  ([0 1 2] (sb-keys-row (sb 1 5 (L 0 1) (L 2 9)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (pack f x)
  (Add (Lsh f 32) x))

(defun (unpack v)
  [(Rsh v 32) (Trunc32 v)])

(defun (newNat igo key nats apps table stack nat code)
  (igo (Force [(Inc key)
               (sbInsert nat key nats)
               apps
               (sbInsert key nat table)
               [key stack]])
    code))

(defun (oldNat igo key nats apps table stack nat code)
  (igo
    (Force [key nats apps table [(sbSearchKnown nat nats) stack]])
    code))

(defun (insNat igo [key nats apps table stack] nat code)
  (Ifz (sbSearch nat nats)
    (newNat igo key nats apps table stack nat code)
    (oldNat igo key nats apps table stack nat code)))

(defun (newApp igo key nats apps table h t stack code)
  (igo
    (Force [(Inc key)
            nats
            (sbInsert (pack h t) key apps)
            (sbInsert key (h t) table)
            [key stack]])
    code))

(defun (oldApp igo key nats apps table h t stack code)
  (igo
    (Force [key nats apps table [(sbSearchKnown (pack h t) apps) stack]])
    code))

(defun (rawInsApp igo key nats apps table h t stack code)
  (Ifz (sbSearch (pack h t) apps)
    (newApp igo key nats apps table h t stack code)
    (oldApp igo key nats apps table h t stack code)))

(defun (insApp igo [key nats apps table [t [h stack]]] code)
  (rawInsApp igo key nats apps table h t stack code))

(defun (insPin igo [key nats apps table [i stack]] code)
  (rawInsApp igo key nats apps table 0 i stack code))

(defun (igo st code)
  (Seq (Force st)
    (Case4 (Hd code)
      st
      (insPin igo st (Ix0 code))
      (insApp igo st (Ix0 code))
      (insNat igo st (Ix0 code) (Ix1 code)))))

(defun (intern x)
  (igo [1 0 0 0 0] (ival x E)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (mapi f row)
  (array (lmapi f (stream row))))

(tests
  ([ 2
     (L 3 1)
     0
     (L 1 3)
     ~[1]
   ]
   (intern 3)))

(tests
  ([
     3
     (L 3 1)
     (L 4294967297 2)
     (sb 1 3 0 (L 2 (1 1)))
     ~[2]
   ]
   (intern (3 3))))

; (tests
;   (3 (intern MkPin{Foo x y z => "Hello World!" })))

(defun (summarize [keys nats apps table stack])
  [keys (Hd nats) (Hd apps) (Hd table) stack])

(def foo (summarize (Force (intern Add))))

(tests
  12(count 0 (ival Add E))
  foo[21 5 15 20 ~[20]])

(def CopyUniq Copy) ; TODO: we jet this, but we don't expose it yet.

(defmacro (# env x)
  (let ((sym (Ix0 (Ix1 x))))
    (Ifz (Nat sym)
      Error("#: bad input" x)
      [env [(1 sym) sym]])))

(defun (rowTakeKnown n row)
  (CopyUniq n 0 0 row
    (Rep 0 0 n)))

(defun (rowTake n row)
  (If (Ge n (Sz row))
    row
    (rowTakeKnown n row)))

(tests
  ([1 2 3]   (rowTake 3 [1 2 3 4]))
  ([1 2 3 4] (rowTake 4 [1 2 3 4]))
  ([1 2 3 4] (rowTake 5 [1 2 3 4])))

; =bump= increments a refcount in a unique table of refcounts.  Note that,
; since we are mutating the table with UpUniq, we need to make sure
; to perform the calculation right away, before we do the update.

(defun (bump i counts)
  (let ((c (Inc (Ix i counts))))
    (Seq c
      (UpUniq i c counts))))

; Given a node graph, count the number of incoming edges to each node.
;
; =counts= is a unique array of numbers all initialized to zero.

(defun (incoming-edges n graph counts)
  (let* ((i  (Dec n))
         (v  (Ix i graph))
         (c1 (Inc (Ix i counts))))
    (Ifz n counts
      (Seq counts
        (incoming-edges i graph
          (Ifz (IsApp v) counts
            (bump (Ix0 v) (bump (Hd v) counts))))))))

; =shatter-filter= is a foldri step function used in =shatter=.
;
; If a node is a cell and has multiple incoming edges, include it's
; index in the result, otherwise ignore it.

(defun (shatter-filter table i x acc)
  (If (Le x 1) acc
    (Ifz (IsApp (Ix i table)) acc
      [i acc])))

; =shatter= takes a graph description of a table and calculates the set
; of nodes which should be fragments.  A fragment is a cell which is
; references from multiple other cells.  The top-level node, assuming
; it is a cell, is always include as a fragment.

(defun (shatter table)
  (let ((counts (UpUniq (Dec (Sz table)) 2
                  (incoming-edges (Sz table) table
                    (Rep 0 0 (Sz table))))))
    (array
      (lfoldri (shatter-filter table) [] (stream counts)))))

(defun (shattered internResult)
  (shatter (cons Pin (sb-vals-row (Ix 3 internResult)))))

(tests
  ([]      (shattered (intern 5)))
  ([2]     (shattered (intern (5 5))))
  ([2]     (shattered (intern (0 0))))
  ([2 3]   (shattered (intern ((0 0) (0 0)))))
  ([3 4]   (shattered (intern ((0 1) (0 1)))))
  ([3 4]   (shattered (intern ((0 0 0) (0 0 0)))))
  ([2 3 5] (shattered (intern ((0 0 0) (0 0 0) (0 0))))))

; next we need to calculate the size of the result, so we can pre-allocate
; a buffer and then fill it in-place.
;
; We can get a list of bit-size easily, and then we just need to bin
; them into the appropriate size classes.  We can
;
; byt    -> 0
; word   -> 1
; big(n) -> n
;
; The size of the header is 40 bytes.
; Each byte costs a byte
; Each word costs a 8
; Each n-word nat costs 8*(1+n)
; Each n-word nat costs 8*(1+n)
;
; Then we need to sum up the bit-size of each fragment.
;
; For this, we need to actually do an incremental loop, since maxRef is
; a function of how many things are in scope *before* each fragment.
;
; Then we just traverse the tree and sum up the size of each node.
; A pin/nat/frag costs refSz.  A closure costs (TODO: equation).
;
; Finally, since the result is a bar, we also set the high bit.
;
; Once we have the size, we need to actually write the seed.
;
; The first step is to allocate the result buffer.
;
; Then, we need to fill out the header (ez)
;
; Then the bignum sizes, the bignum data, the words, the bytes.  All very
; easy, just a bunch of Store64Uniq, and Store8Uniq.
;
; And finally, we need to fill out the actual tree data.

; (tests
;   (224 (Sz (shattered (intern main)))))

(tests
  (8 (Bits 255)))

(defun (atom-size-bin bits)
  (Sub 2 (Add (Gt bits 8) (Gt bits 64))))

(tests
  (2 (atom-size-bin (Bits 255)))
  (1 (atom-size-bin (Bits 256)))
  (0 (atom-size-bin (Bits "Hello world!"))))

(defun (bits-to-words bits)
  (Rsh (Add 63 bits) 6))

(defun (add-big-size bits table)
  (let* ((words (bits-to-words bits))
         (old-list (Ix 3 table))
         (new-list [words old-list]))
    (Seq3 words old-list new-list
      (UpUniq 3 new-list table))))

(defun (atom-sizes-step table bits)
  (let* ((bin    (atom-size-bin bits))
         (count  (Inc (Ix bin table)))
         (table2 (UpUniq bin count table)))
    (Seq2 count table2
      (If bin table2
        (add-big-size bits table2)))))

(defun (atom-sizes nats-list)
  (lfoldl
    atom-sizes-step
    (Rep 0 0 4)
    (lmap Bits nats-list)))

(tests
  ([1 2 3 ~[2]]
   (atom-sizes ~[0 1 255 256 "Hello!" "Hello World!"])))

(defun (mul8 x)  (Lsh x 3))
(defun (mul64 x) (Lsh x 6))

(defun (sum row)
  (foldl Add 0 row))

(tests
  (0 (sum 9))
  (9 (sum [2 3 4])))

(defun (seed-prefix-size [nbigs nwords nbytes big-sizes])
  (sum [40 (mul8 nbigs) (mul8 (sum big-sizes)) (mul8 nwords) nbytes]))

(defun (test-seed-prefix-size noun)
  (let ((intern-table[count atoms cells table top]
         (intern noun)))
    (DeepSeq intern-table
      (seed-prefix-size (atom-sizes (sb-keys-list atoms))))))

(tests
  41(test-seed-prefix-size 0)
  48(test-seed-prefix-size "Hello!")
  64(test-seed-prefix-size "Hello World!")
  41(test-seed-prefix-size (0 0)))


;;; New Interning Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (i-nat-new st[key ptrs nats apps table] n)
  (let* ((nats2  (sbInsert n key nats))
         (table2 (sbInsert key n table)))
    [[(Inc key) ptrs nats2 apps table2] key]))

(defun (i-nat st[_ _ nats] x)
  (let* ((r (sbSearch x nats)))
    (If r [st (Ix0 r)]
      (i-nat-new st x))))

(defun (i-app2 st1[nex ptrs nats apps table] f x)
  (let* ((k (pack f x))
         (r (sbSearch k apps)))
    (If r [st1 (Ix0 r)]
      [ [(Inc nex) ptrs nats (sbInsert k nex apps) (sbInsert nex (f x) table)]
        nex
      ])))

(defun (i-pin i-noun st1 x)
  (let* (([st2 i] (i-noun st1 (Unpin x))))
    (i-app2 st2 0 i)))

(defun (i-law i-noun st1 x)
  (let* (([st2 l]    (i-noun st1 Law))
         ([st3 n]    (i-nat st2 (Name x)))
         ([st4 ln]   (i-app2 st3 l n))
         ([st5 a]    (i-nat st4 (Arity x)))
         ([st6 lna]  (i-app2 st5 ln a))
         ([st7 b]    (i-noun st6 (Body x))))
    (i-app2 st7 lna b)))

(defun (i-app i-noun st1 app)
  (let* (([st2 f]    (i-noun st1 (Init app)))
         ([st3 x]    (i-noun st2 (Last app))))
    (i-app2 st3 f x)))

(defun (iptr p out[st[_ ptrs] key])
  (Force
    (Ifz p out
      (Up 0
        (Up 1 (sbInsert p key ptrs) st)
        out))))

(defun (i-noun st[key ptrs nats apps table] x)
  (let* ((p      (XPtrOp x))
         (cached (sbSearch p ptrs)))
    (If cached [st (Ix0 cached)]
      (iptr p
        (Case4 (Type x)
          (i-nat st x)
          (i-pin i-noun st x)
          (i-law i-noun st x)
          (i-app i-noun st x))))))

(defun (intern2 x)
  (i-noun [1 (sbSing (XPtrOp Pin) 0) 0 0 (sbSing 0 Pin)] (Force x)))

(defun (escape-loop row i)
  (let* ((e (Ix i row)))
    (Ifz (IsApp e) e
      ( (escape-loop row (Hd e))
        (escape-loop row (Ix0 e))))))

(defun (escape itable)
  (let* ((row (sb-vals-row itable)))
    (escape-loop row (Dec (Sz row)))))

(defun (escape-roundtrip x)
  (let* ((y (escape (Ix4 (Ix0 (intern2 x))))))
    (If (planEq x y) 1
      Error[x "!=" y])))

assert(escape-roundtrip 9)
assert(escape-roundtrip Pin)
assert(escape-roundtrip Law)
assert(escape-roundtrip (0 1 2))
assert(escape-roundtrip {x y -> x})
assert(escape-roundtrip Add)
assert(escape-roundtrip main) ; 0.3s, not bad


;;; Some Basic ArraySet Operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (bsearch key row low end)
  (let* ((ix (Rsh (Add low end) 1)))
    (If (Ge low end) (Lsh ix 1)
      (Case3 (Cmp key (Ix ix row))
        (bsearch key row low ix)
        (Inc (Mul ix 2))
        (bsearch key row (Inc ix) end)))))

(defun (set-has k s)
  (Test 0 (bsearch k s 0 (Sz s))))

(tests
  0(set-has 0 [1 3 5])
  1(set-has 1 [1 3 5])
  0(set-has 2 [1 3 5])
  1(set-has 3 [1 3 5])
  1(set-has 5 [1 3 5])
  0(set-has 7 [1 3 5]))


;;; Count Leaves ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (count-leaves-loop frags graph i)
  (let* ((n (Ix i graph)))
    (If (Or (Nil (IsApp n)) (set-has i frags)) 1
      (Add (count-leaves-loop frags graph (Hd n))
           (count-leaves-loop frags graph (Ix0 n))))))

(defun (count-leaves graph frags frag-id)
  (let* ((node (Ix (Ix frag-id frags) graph)))
    (Add
      (count-leaves-loop frags graph (Hd node))
      (count-leaves-loop frags graph (Ix0 node)))))

(def ex [Pin 1 2 (0 1) (2 3)])

(tests
  2(count-leaves ex [3] 0)
  3(count-leaves ex [4] 0)
  2(count-leaves ex [3 4] 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Alright, so we know the size of the header.
; And we know the number of leaves.

; This should be enough information in order to calculuate the bit-size
; of the resulting seed.
;
; From that, we can allocate a buffer of appropriate size.

; (defun (writeHeader nHol bignatSizes nWords nBytes nFrags atoms)
;   (ByteFill
;     (Cat
;       [[(byteSliceFromW64 nHol)
;         (byteSliceFromW64 (Sz bignatSizes))
;         (byteSliceFromW64 nWords)
;         (byteSliceFromW64 nBytes)
;         (byteSliceFromW64 nFrags)]
;        (Map [0 8] bignatSizes)
;        (: x < foreach atoms
;          (If (Lte x u8max)  (byteSliceFromW8 x)
;            (If (Lte x u64max) (byteSliceFromW64 x)
;              [0 (Mul 8) (wordSz x) x])))
;        [257]])))

(defun (store-sizes off buf sizes[s ss])
  (Seq2 off buf
    (Ifz sizes [buf off]
      (store-sizes (Add 8 off) (Store64 off s buf) ss))))

(defun (atom-size a)
  (let ((b (Bytes a)))
    (If (Le b 1) 1
      (If (Le b 8) 8
        (Lsh (Rsh (Add b 7) 3) 3)))))

(tests
  1(atom-size 0)
  1(atom-size 1)
  1(atom-size 255)
  8(atom-size 256)
  8(atom-size "Hello")
  16(atom-size "Hello World!"))

(defun (store-atoms off buf atoms[a as])
  (Seq2 off buf
    (Ifz atoms [buf off]
      (let* ((w (atom-size a))
             (buf (Store off w a buf)))
        (store-atoms (Add w off) buf as)))))

(defun (writeHeader num-params bignat-sizes num-words num-bytes num-frags atoms)
  (let* ((num-bigs (Sz bignat-sizes))
         (prefix-size (seed-prefix-size [num-bigs num-words num-bytes bignat-sizes]))
         (buf       (Bex prefix-size))
         (buf       (Store64 0  num-params buf))
         (buf       (Store64 8  num-bigs   buf))
         (buf       (Store64 16 num-words  buf))
         (buf       (Store64 24 num-bytes  buf))
         (buf       (Store64 32 num-frags  buf))
         ([buf off] (store-sizes 40 buf (stream bignat-sizes)))
         ([buf off] (store-atoms off buf (stream atoms)))
         )
    [buf off]))

(writeHeader 1 [2 1] 2 3 1 ["Hello World!" (Dec (Bex 64)) "Hi World" "Hi" 7 3 2 1])
