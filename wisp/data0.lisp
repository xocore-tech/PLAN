(require "struct")
;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

;;;; Basic ADT Macros
;;;; ================
;;;;
;;;; This defines some basic Wisp macros for working with
;;;; tag-discriminated unions.
;;;;
;;;; Since this is defined before the lambda macros, we don't have any
;;;; good way to introduce bindings in nested contexts, so we only
;;;; provide macros for switching on tags, you still have to pull out
;;;; the fields by hand.


;;; Switching on ADT Tags

(def .0 Ix0)
(def .1 Ix1)
(def .2 Ix2)

(defun (validate-expression x k)
  (Ifz (And (Nil (Hd x)) (Ge (Sz x) 1))
    Error("expected-expression" x)
    k))

(defun (validate-constructor-name x k)
  (Ifz (is-caps-symbol x)
    Error("expected-constructor-name" x)
    k))

(defun (validate-type-name x k)
  (Ifz (And x (strAll isAlnum x))
    Error("bad-type" x)
    k))

(defun (validate-type-expr x k)
  (If (IsNat x)
    (validate-type-name x k)
    (validate-expression x k))) ; TODO: recursive

(defun (validate-type-exprs list[x xs] k)
  (Ifz list k
    (validate-type-expr x
      (validate-type-exprs xs k))))

(defun (validate-branch b k)
  (validate-expression b
    (validate-constructor-name (.0 b)
      (validate-type-exprs (stream (drop 1 b))
        k))))

(defun (validate-union-branches bs k)
  (lfoldr {b k -> (validate-branch b k)} k bs))

(defun (bind-union-constructor i [nm])
  [nm i])

(defmacro (deftype e x)
  (@ branches (drop 2 x))
  (validate-union-branches (stream branches)
    [ (multi-bind e (lmapi bind-union-constructor (stream branches)))
      0 ]))

(defun (get-cnstr-tag e nm)
  ; TODO: support literal numbers also.
  (@ r[_ val] (getenv nm e))
  (Ifz r Error("unbound-constructor" nm)
    val))

(defun (verify-constructor-is-number n k)
  (Ifz (IsNat n)
    Error("constructor-not-nat" n)
    k))

(defun (verify-constructors-are-numbers tags k)
  (lfoldr verify-constructor-is-number k tags))

(defun (dup x)
  [x x])

(defun (verify-constructors-are-unique cs k)
  (If (Ne (Sz cs) (bal-sz (bal-from-pairs-list (lmap dup (stream cs)))))
    Error("non-unique-constructors" cs)
    k))

(defun (bal-sort-row row)
  (bal-keys-row (bal-from-pairs-list (lmap dup (stream row)))))

(defun (verify-constructors-are-contiguous cs k)
  (@ sorted (bal-sort-row cs))
  (Ifz (planEq sorted (gen (Sz sorted) id))
    Error("non-contiguous-constructors" sorted)
    k))

(defun (case-codegen scrut bs)
  (@ head-expr [(1 Hd) scrut])
  (Case5 (Sz bs)
    Error("empty switch")
    [(1 Seq) scrut (.0 bs)]
    [(1 Ifz) head-expr (.0 bs) (.1 bs)]
    [(1 Case3) head-expr (.0 bs) (.1 bs) (.2 bs)]
    [(1 Case) head-expr (cons (1 0) (Init bs)) (Last bs)]))

(defun (switch-codegen scrut branches)
  (case-codegen scrut
    (bal-vals-row (bal-from-pairs-row branches))))

(defun (lzip xlist[x xs] ylist[y ys])
  (And xlist
    (And ylist
      [[x y] (lzip xs ys)])))

(defun (zip x y)
  (Row 0 (min (Sz x) (Sz y))
    (lzip (stream x) (stream y))))

(tests
  ( [[3 "three"] [4 "four"] [5 "five"]]
    (zip [3 4 5] ["three" "four" "five" "six"])))

(defun (valid-switch-branch-shape branch)
  (And (Eq 2 (Sz branch))
    (And (Nil (Hd branch))
      (IsNat (.0 branch)))))

(defun (verify-switch-branch-shape branch k)
  (Ifz (valid-switch-branch-shape branch)
    Error("invalid-branch" branch)
    k))

(defun (verify-switch-branch-shapes bs k)
  (lfoldr verify-switch-branch-shape k (stream bs)))

(defmacro (union-switch e x[_ scrut])
  (@ branches (drop 2 x))
  (@ cnstrs   (map .0 branches))
  (@ tags     (Force (map (get-cnstr-tag e) cnstrs)))
  (verify-switch-branch-shapes branches
    (verify-constructors-are-numbers (stream tags)
      (verify-constructors-are-unique tags
        (verify-constructors-are-contiguous tags
          [e (switch-codegen scrut (zip tags (map .1 branches)))])))))


;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Noun
  (ATOM Nat)
  (CELL Silly Silly))

(tests
  (ATOM 0)
  (CELL 1))

(tests
  4(union-switch (ATOM 9) (ATOM 4) (CELL 5))
  5(union-switch (CELL 9) (ATOM 4) (CELL 5)))

(defun (silly-sum x)
  (@ a (.0 x))
  (@ b (.0 x))
  (union-switch x
    ATOM(a)
    CELL(Add (silly-sum a) (silly-sum b))))

(tests
  6(silly-sum (CELL (ATOM 3) (CELL (ATOM 2) (ATOM 1)))))

(deftype Tri
  (X Unit) (Y Unit) (Z Unit))

(deftype Many
  (A Unit) (B Unit) (C Unit) (D Unit) (E Unit) (F Unit)
  (G Unit) (H Unit) (I Unit) (J Unit) (K Unit) (L Unit))

(defun (tripped x)
  (union-switch x
    (X "X")
    (Y "Y")
    (Z "Z")))

(tests
  ("X" (tripped X))
  ("Y" (tripped Y))
  ("Z" (tripped Z)))

(defun (manied x)
  (union-switch x
    (A "a")
    (B "b")
    (C "c")
    (D "d")
    (E "e")
    (F "f")
    (G "g")
    (H "h")
    (I "i")
    (J "j")
    (K "k")
    (L "l")))

(Unpin manied)

(tests
  ("a" (manied A))
  ("l" (manied L))
  ("l" (manied 99)))

(tests
  ("abcdefghijkl"
   (lstrcat (lmap manied ~[A B C D E F G H I J K L]))))
