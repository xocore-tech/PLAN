(require "wisp")
;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

;;;; Balanced Binary Search Trees
;;;; ============================

(def bal-empty 0)
(def bal-sing 1)   ; (bal-sing k v) => 1[k v]
(def bal-sz Hd)

(defun (bal-sing- k v)
  (1 k v 0 0)) ; (bal-sing- k v) => 1[k v 0 0]

(defun (bal k v l r)
  (@ sz (Inc (Add (bal-sz l) (bal-sz r))))
  (sz k v l r))

(def N bal)
(def E 0)

(defun (L k v) (1 k v 0 0))

(defun (single-l key val a right[koy vol b c])
  (bal koy vol (bal key val a b) c))

(defun (single-r key val [koy vol a b] c)
  (bal koy vol a (bal key val b c)))

(defun (double-l key val a right[rkey rval son[skey sval b c] d])
  (bal skey sval (bal key val a b) (bal rkey rval c d)))

(defun (double-r key val left[lkey lval a son[skey sval b c]] d)
  (bal skey sval (bal lkey lval a b) (bal key val c d)))

(tests
  ((single-l 7 7 E (L 8 8))
   (N 8 8 (L 7 7) E))

  ((single-r 7 7 (L 6 6) E)
   (N 6 6 E (L 7 7)))

  ((single-l 7 7 E (N 8 8 E (L 9 9)))
   (N 8 8 (L 7 7) (L 9 9)))

  ((single-l 5 5 E (N 8 8 (L 7 7) (L 9 9)))
   (N 8 8 (N 5 5 E (L 7 7)) (L 9 9)))

  ((double-l 5 5 E (N 8 8 (L 7 7) (L 9 9)))
   (N 7 7 (L 5 5) (N 8 8 E (L 9 9))))

  ((double-r 5 5 (N 2 2 (L 1 1) (L 3 3)) E)
   (N 3 3 (N 2 2 (L 1 1) E) (L 5 5))))

(defun (rotate-l k v l r[_ _ rl rr])
  (If (Lt (bal-sz rl) (Mul 2 (bal-sz rr)))
    (single-l k v l r)
    (double-l k v l r)))

(defun (rotate-r k v l[_ _ ll lr] r)
  (If (Lt (bal-sz lr) (Mul 2 (bal-sz ll)))
    (single-r k v l r)
    (double-r k v l r)))

(defun (balance k v l r)
  (@ lsz (bal-sz l))
  (@ rsz (bal-sz r))
  (@ xsz (Inc (Add lsz rsz)))
  (@ top (xsz k v l r))
  (If (Le (Add lsz rsz) 1) top
    (If (Ge rsz (Mul 4 lsz)) (rotate-l k v l r)
      (If (Ge lsz (Mul 4 rsz)) (rotate-r k v l r)
        top))))

(defun (bal-insert key val x[xkey xval left right])
  (Ifz x (bal-sing key val 0 0)
    (Ix (Cmp key xkey)
      [(balance xkey xval (bal-insert key val left) right)
       ((bal-sz x) key val left right)
       (balance xkey xval left (bal-insert key val right))])))

(defun (bal-search key tree[k v l r])
  (And tree
    (Case3 (Cmp key k)
      (bal-search key l)
      [v]
      (bal-search key r))))

(defun (bal-idx k t)
  (Ix0 (bal-search k t)))

(defun (bal-has k t)
  (Truth (bal-search k t)))

(defun (bal-from-pairs-list-loop acc list[[k v] ls])
  (Seq acc
    (Ifz list acc
      (bal-from-pairs-list-loop (bal-insert k v acc) ls))))

(defun (bal-from-pairs-list ls)
  (bal-from-pairs-list-loop 0 ls))

(defun (bal-from-pairs-row row)
  (bal-from-pairs-list (stream row)))

(defun (derp n)
  (bal-from-pairs-list (stream (gen n {x -> (@ xx (Mul (Inc x) 11))
                                      [xx xx]}))))

(tests
  ((bal-search 11 (derp 0)) [])
  ((bal-search 11 (derp 1)) [11])
  ((bal-search 11 (derp 2)) [11])
  ((bal-search 11 (derp 3)) [11])
  ((bal-search 11 (derp 4)) [11])
  ((bal-search 11 (derp 5)) [11])
  ((bal-search 11 (derp 6)) [11])
  ((bal-search 66 (derp 6)) [66])
  ((bal-search 77 (derp 6)) []))

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

  ((bal-insert 2 22 0)
   (bal-sing 2 22 0 0)))

(def ex (NN 4 (LL 2) (LL 6)))

(tests
  ((bal-insert 4 "four" ex)
   (N 4 "four" (L 2 2) (L 6 6)))

  ((bal-insert 2 "two" ex)
   (N 4 4 (L 2 "two") (L 6 6)))

  ((bal-insert 6 "six" ex)
   (N 4 4 (L 2 2) (L 6 "six")))

  ((bal-insert 1 "one" ex)
   (N 4 4 (N 2 2 (L 1 "one") E) (L 6 6))))

(tests
  ((bal-insert 2 "two" ex)   (NN 4 (L 2 "two")              (LL 6)))
  ((bal-insert 3 "three" ex) (NN 4 ((NN 2) E (L 3 "three")) (LL 6)))
  ((bal-insert 5 "fiv" ex)   (NN 4 ((NN 2) E E)             (NN 6 (L 5 "fiv") E)))
  ((bal-insert 6 "six" ex)   (NN 4 (LL 2)                   (L 6 "six")))
  ((bal-insert 7 "sev" ex)   (NN 4 (LL 2)                   (NN 6 E (L 7 "sev")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (delete-find-max t[k v l r])
  (@ [km r2] (delete-find-max r))
  (Ifz r [[k v] l]
    [km (balance k v l r2)]))

(defun (delete-find-min t[k v l r])
  (@ [km l2] (delete-find-min l))
  (Ifz l [[k v] r]
    [km (balance k v l2 r)]))

(defun (glue l r)
  (@ [[lkm lm] l2] (delete-find-max l))
  (@ [[rkm rm] r2] (delete-find-min r))
  (Ifz l r
    (Ifz r l
      (If (Gt (bal-sz l) (bal-sz r))
        (balance lkm lm l2 r)
        (balance rkm rm l r2)))))

(defun (bal-alter f key x[xsz xkey xval l r])
  (@ f0  (f 0))
  (@ fxv (f [xval]))
  (Ifz x
    (Ifz f0
      bal-empty
      (bal-sing key (Ix0 f0)))
    (Case3 (Cmp key xkey)
      (balance xkey xval
        (bal-alter f key l) r)
      (Ifz fxv (glue l r)
        (xsz xkey (Ix0 fxv) l r))
      (balance xkey xval l
        (bal-alter f key r)))))

; alter-known alters the value associated with a key which is known to
; be present, so the function is not passed a maybe.

(defun (bal-alter-known f key x[xkey xval l r])
  (@ xsz (Hd x))
  (Ifz x bal-empty
    (Case3 (Cmp key xkey)
      (balance xkey xval (bal-alter-known f key l) r)
      (xsz xkey (f xval) l r)
      (balance xkey xval l (bal-alter-known f key r)))))

(defun (bal-del key tree[xkey xval l r])
  (@ xsz (Hd tree))
  (Ifz tree tree
    (Case3 (Cmp key xkey)
      (balance xkey xval (bal-del key l) r)
      (glue l r)
      (balance xkey xval l (bal-del key r)))))

(defun (bal-search-case k t nf f)
  (@ r (bal-search k t))
  (Ifz r nf
    (f (Ix0 r))))

(defun (bal-search-known k bal)
  (Ix0 (bal-search k bal)))

(defun (lcat-rows list-of-rows)
  (lcat (lmap stream list-of-rows)))

(tests ((lcat-rows ~[[3 4] 0 [5 6 7]])
        ~[3 4 5 6 7] ))

(defun (bal-walk bal[k v l r])
  (And bal
  (lweld (bal-walk l)
  (lcons bal
  (bal-walk r)))))

(defun (bal-to-pairs-list bal)
  (lmap (take 2) (bal-walk bal)))

(defun (bal-save tree) ; to [k v k v k v] format
  (Row 0 (Lsh (bal-sz tree) 1)
    (lcat-rows (bal-to-pairs-list tree))))

(tests
  ( [1 1 5 5 9 9]
    (bal-save (bal 5 5 (L 1 1) (L 9 9)))))

(defun (bal-keys-list tree) (lmap Ix0 (bal-walk tree)))
(defun (bal-vals-list tree) (lmap Ix1 (bal-walk tree)))
(defun (bal-vals-row tree)  (Row 0 (bal-sz tree) (bal-vals-list tree)))
(defun (bal-keys-row tree)  (Row 0 (bal-sz tree) (bal-keys-list tree)))

(tests
  ([1 5 9] (bal-vals-row (bal 1 5 (L 0 1) (L 6 9))))
  ([0 1 2] (bal-keys-row (bal 1 5 (L 0 1) (L 2 9)))))
