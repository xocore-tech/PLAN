(require "op")
;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

; Wisp is an incredibly simple Lisp-like language for bootstrapping and
; testing PLAN.
;
; It has only five built-in bindings, all of which have very
; straightforward behavior:
;
; - `def' and `macro' are macros which modify the namesapce.
;
; - `law' is a macro which expands into a law value.
;
; - `Pin' is the PLAN value `<0>'.
;
; - `eval' is the Wisp evaluator.
;
; `Pin' is just the built-in PLAN primop `<0>' for constructing pins.
; All other values can be constructed from this one, since we can combine
; this with numbers to construct the other primops.
;
; `eval' the Wisp evaluator.  It takes a namespace and an expression,
; and returns a new namespace and the resulting value.
;
; Evaluation is extremely simple, it just does three things:
;
; 1.  Expand all macros.
;
; 2.  Dereference all variables.
;
; 3.  Convert the resulting tree of values into a PLAN thunk and
;     evaluate it.
;
; Wisp macros can update the namespace, so `def' and `macro' are
; implemented as macros just insert into the environment and with a
; dummy-expansion of `0`.  These macros call into `eval' in order to
; convert the given expression into a value.
;
;     (def name expr)
;     (macro name expr)
;
; The `law' macro constructs a law.  Since Wisp does not have nested
; scopes, this macro simply constructs the PLAN law, and expands into
; a constant value.
;
; The `law' macro offers no additional functionality on top of the PLAN
; law structure except names, everything is translated exactly as
; specified.
;
; -   (law (nm args..) body)
; -   (law (nm args..) (@ v1 e1) body)
; -   (law (nm args..) (@ v1 e1) (@ v2 e2).. body)
;
; Expressions are represented as PLAN values with the following encoding.
;
;     foo   -> "foo"
;     ()    -> 0
;     (x)   -> 0[x]
;     (x y) -> 0[x y]
;     123   -> 1[123]
;     "foo" -> 1["foo"]
;
; The only numbers supports are decimal numbers and strings do not have
; any support for escaping.  However, strings are just numbers, so you
; can also fallback to that notation.
;
;     (def singleQuoteChar "'")
;     (def doubleQuoteChar 10)
;
; In addition to this basic syntax, Wisp offers the following syntactic
; sugar:
;
;     {x..}    => (CURLED (x..))
;     [x..]    => (BRACED (x..))
;     foo(x..) => (foo (x..))
;
; The first two make it possible to extend the syntax with {} and []
; forms using the macro system.
;
; This last piece of sugar makes it possible to build quotation and
; quasi-quotation as macros without special reader support.  The result
; is uglier, but this is just a bootstrapping language, so it doesn't
; really matter.
;
;     '(foo)
;     `((foo ,bar zaz))
;     #(a b c)
;
; The namespace is an unbalanced binary search tree with the following
; representation:
;
;     0[key val macro left right]
;
; The key and val are the binding name (a string) and the coresponding
; value.  `macro` is a flag (0 or 1) which indicates if the binding is
; a macro.  `left` and `right` are subtrees where all of the keys are
; smaller or greater respectively.
;
; This is not abstract, and macros are free to manipulate this structure
; directly.
;
; It is also possible to replace the built-in binding forms with your own,
; and have a balanced binary search tree instead, as the evaluator itself
; only reads the tree.
;
; The unbalanced form was choosen to minimize the size of the seed,
; but it will not scale well to large use-cases.
;
; The head of the BST nodes are unused (always 0 by default), so a
; subtree-size can be stored there without changing the representation.

; (pinlaw (name args..) body..) =>
;
;     (Pin (law (name args..) body..))

(macro pinlaw
  Pin(law (pinlaw ws x)
    (0 ws (0 (1 Pin) (Up 0 "law" x)))))

(def const (pinlaw (const x y) x))

; assert env e :=
;   let x = Ix1(e)
;   if (Ne 1 Hd(x)):
;     [env ["eval" [1[Force] x]]]
;   if (Ne 1 (Ix1 x)):
;     Error("assertion-failed" val "!=" 1 "in" e)
;   else:
;     [env 0]

(macro assert
  (law (assert oldws x)
    (@ out Force(eval oldws (Ix1 x)))
    (@ ws  (Ix0 out))
    (@ val (Ix1 out))
    (If (Ne val 1)
      Error("assertion-failed" val "!=" 1 "in" x)
      (0 oldws 0))))

assert(1)

assert(Eq 1 (const 1 2))

assert(Lt 3 4)
assert(Le 3 4)
assert(Ne 3 4)
assert(Lt 3 4)
assert(Le 3 4)
assert(Ne 3 4)
assert(Le 4 4)
assert(Eq 4 4)
assert(Ge 4 4)

assert(Nil (Eq 3 4))
assert(Nil (Ge 3 4))
assert(Nil (Gt 3 4))
assert(Nil (Eq 3 4))
assert(Nil (Ge 3 4))
assert(Nil (Gt 3 4))
assert(Nil (Lt 4 4))
assert(Nil (Ne 4 4))
assert(Nil (Gt 4 4))

assert(Eq 2 Sz(0 1 2))
assert(Eq 0 (And 0 3))
assert(Eq 3 (And 1 3))
assert(Eq 3 (Or 3 4))
assert(Eq 4 (Or 0 4))

assert(Eq 9 (If 1 9 (Error 4)))
assert(Eq 0 (And 0 (Error 4)))
assert(Eq 3 (Or 3 (Error 5)))

assert(Eq 3 (Ifz 0 3 4))
assert(Eq 4 (If 0 3 4))
assert(Eq 4 (Inc 3))
assert(Eq 4 (Seq 3 4))

assert(Eq 0 (Type 0))              ; nat
assert(Eq 1 (Type Inc))            ; pin
assert(Eq 2 Type(law (f x y) x)) ; law
assert(Eq 3 Type(0 0))          ; app
assert(Eq 0 (Type "Hello World!")) ; nat

(def const    (pinlaw (const x y) x))

(def constLaw (law (const x y) x))

assert(Eq 1       (const 1 3)      )
assert(Eq 1       (constLaw 1 3)   )
assert(Eq 2       (Arity constLaw) )
assert(Eq 1       (Body constLaw)  )
assert(Eq "const" (Name constLaw)  )
assert(Eq 0       (Name const)     )

(def streamed
  (pinlaw (streamed i end row)
    (And (Lt i end)
      (0 (Ix i row) (streamed (Inc i) end row)))))

(def stream
  (pinlaw (stream row)
    (streamed 0 (Sz row) row)))

(def drop
  (pinlaw (drop n row)
    (@ wid (Sz row))
    (Row 0 (Sub wid n) (streamed n wid row))))

(def lweld
  (pinlaw (lweld a b)
    (Ifz a b
      (0 (Ix0 a) (lweld (Ix1 a) b)))))

(def weld
  (pinlaw (weld a b)
    (Row 0 (Add (Sz a) (Sz b))
      (lweld (stream a) (stream b)))))

; (defmacro (name args..) body) =>
;
;     (macro name (pinlaw (name args..) body))

(macro defmacro
  (pinlaw (defmacro st x)
    (0 st
      (0 "macro" Ix0(Ix1 x)
        (Up 0 "pinlaw" x)))))

; (defun (name args..) body) =>
;
;     (def name (pinlaw (name args..) body))

(defmacro (defun st x)
  (0 st
    (0 "def" Ix0(Ix1 x)
      (Up 0 "pinlaw" x))))

(defun (cons item row)
  (Row 0 Inc(Sz row)
    (0 item (stream row))))

; [a b c] =>
;   (BRACED (a b c)) =>
;     (0 a b c)

(macro BRACED
  (Pin
    (law (BRACED st x)
      (0 st (cons (1 0) (Ix1 x))))))

assert(Eq 4 (Sz [1 2 3 4]))

assert(Eq 5 Sz(cons 0 [1 2 3 4]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (pinEq eql x y)
  (eql (Unpin x) (Unpin y)))

(defun (lawEq eql x y)
  (And (Eq (Name x) (Name y))
    (And (Eq (Arity x) (Arity y))
      (eql (Body x) (Body y)))))

(defun (rowEqLoop eql x y xsz i)
  (Or (Ge i xsz)
    (And (eql (Ix i x) (Ix i y))
      (rowEqLoop eql x y xsz (Inc i)))))

(defun (rowEq eql x y)
  (@ xsz (Sz x))
  (@ ysz (Sz y))
  (And (Eq xsz ysz)
    (And (eql (Hd x) (Hd y))
      (rowEqLoop eql x y xsz 0))))

(defun (planEq x y)
  (@ xt (Type x))
  (And (Eq xt (Type y))
    (Ix xt
      (0
       (Eq x y)
       (pinEq planEq x y)
       (lawEq planEq x y)
       (rowEq planEq x y)))))

assert(Nil (planEq 3 4))
assert(Nil (planEq 3 planEq))
assert(planEq planEq planEq)

; TODO: replacing the final 0 with a crash triggers the crash.  But this
; doesn't happen when I run the same tests in Sire.  Tf is going on?

assert(planEq (Row 3   4   [1 [2 [3 [4 0]]]])  (3 1 2 3 4) )
assert(planEq (Row 3   3   [1 [2 [3 [4 0]]]])  (3 1 2 3)   )
assert(planEq (Row 3   2   [1 [2 [3 [4 0]]]])  (3 1 2)     )
assert(planEq (Row 3   1   [1 [2 [3 [4 0]]]])  (3 1)       )
assert(planEq (Row 3   0   [1 [2 [3 [4 0]]]])  3           )
assert(planEq (Row 3   4   [1 [2 [3]]])        (3 1 2 3 0) )
assert(planEq (Row 3   4   [1 [2 Inc]])        (3 1 2 0 0) )
assert(planEq (Row Row 4   [1 [2 3 4]])        [1 2 0 0]   )
assert(planEq (Row 3   Row [1 [2 3 4]])        3           )

assert(Eq 1 (Ix0 [1 2 3 4 5 6 7 8]))
assert(Eq 2 (Ix1 [1 2 3 4 5 6 7 8]))
assert(Eq 3 (Ix2 [1 2 3 4 5 6 7 8]))
assert(Eq 4 (Ix3 [1 2 3 4 5 6 7 8]))
assert(Eq 5 (Ix4 [1 2 3 4 5 6 7 8]))
assert(Eq 6 (Ix5 [1 2 3 4 5 6 7 8]))
assert(Eq 7 (Ix6 [1 2 3 4 5 6 7 8]))
assert(Eq 8 (Ix7 [1 2 3 4 5 6 7 8]))

assert(Eq 0 (Ix0 Ix0))
assert(Eq 0 (Ix1 Ix0))
assert(Eq 0 (Ix2 Ix0))
assert(Eq 0 (Ix3 Ix0))
assert(Eq 0 (Ix4 Ix0))
assert(Eq 0 (Ix5 Ix0))
assert(Eq 0 (Ix6 Ix0))
assert(Eq 0 (Ix7 Ix0))

assert(Eq 1 (Cmp Ix7 Ix7))
assert(Eq 0 (Cmp 7 8))
assert(Eq 1 (Cmp 8 8))
assert(Eq 2 (Cmp 9 8))

assert(Eq 2 (Last [1 2]))
assert(Eq 2 (Last [1 (Add 1 1)]))
assert(Eq 0 (Last 0))
assert(Eq 0 (Last Last))

assert(planEq (1 2 3) Init(1 2 3 4))
assert(planEq (1 2)   Init(1 2 3))
assert(planEq Add     Init(Add 1))
assert(planEq 0       (Init Add))

assert(planEq (5 4 2)   (Coup 5       (2 4 2)))
assert(planEq (Add 4)   (Coup Add     (2 4)))
assert(planEq (Add 4 2) (Coup Add     (2 4 2)))
assert(planEq (5 4)     (Coup Add     (1 2 3 4)))
assert(planEq (3 3 4)   (Coup (Add 1) (1 2 3 4)))

assert(Eq 0   (Trunc8 0))               ;  zero
assert(Eq 0   (Trunc8 Trunc8))          ;  case
assert(Eq "H" (Trunc8 "Hello"))         ;  direct
assert(Eq "H" (Trunc8 "Hello world!"))  ;  indirect

assert(Eq 0    (Trunc16 0))               ;  zero
assert(Eq 0    (Trunc16 Trunc16))         ;  case
assert(Eq "He" (Trunc16 "Hello"))         ;  direct
assert(Eq "He" (Trunc16 "Hello world!"))  ;  indirect

assert(Eq 0      (Trunc32 0))               ;  zero
assert(Eq 0      (Trunc32 Trunc32))         ;  case
assert(Eq "Hell" (Trunc32 "Hello"))         ;  direct
assert(Eq "Hell" (Trunc32 "Hello world!"))  ;  indirect

assert(Eq 0          (Trunc64 0))               ;  zero
assert(Eq 0          (Trunc64 Trunc64))         ;  case
assert(Eq "Hello wo" (Trunc64 "Hello wo"))      ;  direct
assert(Eq "Hello wo" (Trunc64 "Hello world!"))  ;  indirect


;;; Nicer Syntax for Assertions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; `test' is a nicer version of assert which supports multiple tests in a
; single block and which uses planEq (which works with any PLAN value)
; instead of Eq (which only works with numbers).

(defun (assert-failure a b expr)
  (xtrace "ASSERTION FAILURE"
  (xtrace "EXPR"
  (xtrace expr
  (xtrace "LEFT VALUE"
  (xtrace a
  (xtrace "RIGHT VALUE"
  (xtrace b
  (Error "assertion-failure")))))))))

; testsLoop will need to be rewritten to use `eval` monadically as well.

(defun (testsLoop env expr n i)
  (@ exp (Ix i expr))
  (@ foo Ix1(eval env (Ix0 exp)))
  (@ bar Ix1(eval env (Ix1 exp)))
  (If (Ge i n) [env 0]
    (Ifz (planEq foo bar)
      (assert-failure foo bar exp)
      (testsLoop env expr n  (Inc i)))))

(defmacro (tests env expr)
  (testsLoop env expr (Sz expr) 1))


;; More Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tests
  (0             (Rsh 0 0))               ;  zero
  (1             (Rsh 1 0))               ;  zero
  (0             (Rsh 0 1))               ;  zero
  (3             (Rsh 7 1))               ;  tiny
  ("ello"        (Rsh "Hello" 8))         ;  direct
  ("ello world!" (Rsh "Hello world!" 8))) ;  indirect


;; Test Test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tests ; Test (bit-index) for direct atoms
  0(Test 0 0)
  1(Test 0 1)
  0(Test 0 2)
  1(Test 0 3)
  0(Test 0 (Bex 62))
  1(Test 0 Inc(Bex 62))
  0(Test 0 (Add 2 (Bex 62)))
  0(Test 64 "Hello")
  1(Test 0 Dec(Bex 63))
  1(Test 9 Dec(Bex 63))
  1(Test 62 Dec(Bex 63)))

(tests ; Test (bit-index) for indirect atoms
  0(Test 8888 "hello world!")
  0(Test 256 Dec(Bex 128))
  0(Test 128 Dec(Bex 128))
  1(Test 127 Dec(Bex 128)))


;; Test Writting Bits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tests
  (1           (Set 0 0))
  (2           (Set 1 0))
  (3           (Set 1 1))
  (2           (Set 1 2))
  (Inc(Bex 62) (Set 62 1)))

(tests
  (Bex(63)     (Set 63 0)))

(tests
  (1                   (Set 0   0))
  (2                   (Set 1   0))
  (3                   (Set 1   1))
  (2                   (Set 1   2))
  (Inc(Bex 62)         (Set 62  1))
  (Bex(63)             (Set 63  0))
  ("01234567@"         (Set 70  "01234567"))           ; direct -> indirect
  ("Iello World!"      (Set 00  "Hello World!"))       ; modify word
  ("Hello world!"      (Set 53  "Hello World!")))      ; modify word

(tests
  ("0123456701234567@" (Set 134 "0123456701234567")))


;; Test Writing Bytes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tests
  (0               (Store8 0  0 0))
  (0               (Store8 7  0 0))
  (0               (Store8 16 0 0))
  (1               (Store8 0 1 0))
  (256             (Store8 1 1 0))
  (65536           (Store8 2 1 0))
  (257             (Store8 1 1 1))
  ("_1234567"      (Store8 0 "_" "01234567"))
  ("01_34567"      (Store8 2 "_" "01234567"))
  ("0123456_"      (Store8 7 "_" "01234567"))
  ("hello World!"  (Store8 0  "h" "Hello World!"))
  ("Hello World?"  (Store8 11 "?" "Hello World!"))
  ("Hello World!?" (Store8 12 "?" "Hello World!")))


;; Even More Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tests
  1(Case3 0 1 2 3)
  2(Case3 1 1 2 3)
  3(Case3 2 1 2 3)
  3(Case3 3 1 2 3))

(tests
  1(Case4 0 1 2 3 4)
  2(Case4 1 1 2 3 4)
  3(Case4 2 1 2 3 4)
  4(Case4 3 1 2 3 4)
  4(Case4 4 1 2 3 4))

(tests
  1(Case5 0 1 2 3 4 5)
  2(Case5 1 1 2 3 4 5)
  3(Case5 2 1 2 3 4 5)
  4(Case5 3 1 2 3 4 5)
  5(Case5 4 1 2 3 4 5)
  5(Case5 5 1 2 3 4 5))

;;; Nicer Syntax for Lambdas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (max x y)
  (If (Ge x y) x y))

(defun (min x y)
  (If (Le x y) x y))

(defun (take n row)
  (Row 0 n (streamed 0 n row)))

(defun (splitArrow i items)
  (@ x (Ix i items))
  (Ifz x
    ["_" (Init items) [(Last items)]]
    (If (Eq x ".")
      ["_" (take i items) (drop (Inc i) items)]
      (If (Eq x "->")
        (["_" (take i items) (drop (Inc i) items)])
        (If (Eq x "=>")
          [(Ix0 items) (drop 1 (take i items)) (drop (Inc i) items)]
          (splitArrow (Inc i) items))))))

(assert
  (planEq
    ["_" ["a" "b"] ["b"]]
    (splitArrow 0 ["a" "b" "->" "b"])))

(defun (splitCurly items)
  (@ hed   (Ix0 items))
  (@ rest  (drop 1 items))
  (@ ccurl (Ix0 hed))
  (If (Eq "CURLED" ccurl)
    (cons (Ix1 hed) (splitArrow 0 (drop 1 items)))
    (cons 0 (splitArrow 0 items))))

; {..} =>
;
;   (CURLED (..))
;
; (CURLED (args.. -> body..)) =>
;
;    (law (0 args..) body..)
;
; (CURLED (self args.. => body..)) =>
;
;   (law (self args..) body..)
;
; (CURLED ((CURLED (captures..)) self args.. => body..)) =>
;
;    ((law (self captures.. args..) body..) captures..)

(defmacro (CURLED ws x)
  (@ split splitCurly(Ix1 x))
  (@ caps  (Ix0 split))
  (@ self  (Ix1 split))
  (@ args  (Ix2 split))
  (@ body  (Ix3 split))
  [ws
    (Ifz caps
      (weld ["law" (cons self args)] body)
      (cons (weld ["law" (cons self (weld caps args))] body)
        caps))])

assert(Eq 3 ({a b -> b}   2 3))
assert(Eq 3 ({f a b => b} 2 3))

(defun (capture-test a b c d)
  ({{a b} f x y => (b y)} c d))

(assert (planEq (4 6) (capture-test 3 4 5 6)))

(assert
  (Eq 7 (Ix1 (eval ["Add" Add] ["Add" (1 3) (1 4)]))))

; Note that, while Wisp doesn't support nested closures, this macro
; makes it somewhat bearable to do lambda-lifting by hand, by providing
; some sugar for it.
;
; In Sire, you could write:
;
;     = (foo x y xs)
;     | map i&(Add x (Mul y i)) xs
;
; But in Wisp, with this syntax, you would instead write:
;
;     = (foo x y xs)
;     | map {{x y} i -> (Add x (Mul y i))} xs
;
; Which is just a short-hand version of the explicitly lambda-lifted
; version:
;
;     = (foo x y xs)
;     | map ({x y i -> (Add x (Mul y i))} x y) xs
;
; However, the implementation still does not need to parse code to find
; free variables, which keeps the implementation a lot lighter.


;;; Conveniences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defun (odd x)         (Test 0 x)    )
( defun (even x)        (Nil (odd x)) )
( defun (flip f x y)    (f y x)       )
( defun (id x)          x             )
( defun (const x y)     x             )
( defun (compose f g x) (f (g x))     )


;;; Common Data Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def NONE 0) ; NONE=[]
(def SOME 0) ; (SOME x)=[x]


;;; Closure Updates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def foo [3 4 5])

(tests
  ([9 4 5] (Up 0 9 foo))
  ([3 9 5] (Up 1 9 foo))
  ([3 4 9] (Up 2 9 foo))
  ([3 4 5] (Up 3 9 foo)))

; This violates the invariant that the input to UpUniq must never be
; used again, and observes the underlying mutation.

(tests ([3 4 5] (UpUniq 4 9 foo)))
(tests ([9 4 5] (UpUniq 0 9 foo)))
(tests ([9 9 5] (UpUniq 1 9 foo)))
(tests ([9 9 9] (UpUniq 2 9 foo)))

(def foo 0) ; hide the invalidated reference in shame.


;;; Closure Copy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (Copy count src-offset dst-offset src dst) => dst-update

(tests
  (("a" 7 8 3 4)  (Copy 2 0 0 (0 7 8 9)    ("a" 1 2 3 4)))  ; partial
  (("a" 1 7 8 4)  (Copy 2 0 1 (0 7 8 9)    ("a" 1 2 3 4)))  ; output offset
  (("a" 8 9 3 4)  (Copy 2 1 0 (0 7 8 9)    ("a" 1 2 3 4)))  ; input offset
  (("b" 1 8 9 4)  (Copy 2 1 1 (0 7 8 9)    ("b" 1 2 3 4)))  ; partial+offset
  (("c" 1 7 8 9)  (Copy 3 0 1 (0 7 8 9)    ("c" 1 2 3 4)))  ; full+offset
  (("d" 7 8 9 10) (Copy 4 0 0 (0 7 8 9 10) ("d" 1 2 3 4)))  ; full
  (("e" 7 8 9 4)  (Copy 3 0 0 (0 7 8 9)    ("e" 1 2 3 4)))  ; partial
  (("f" 7 8 9 0)  (Copy 4 0 0 (0 7 8 9)    ("f" 1 2 3 4)))  ; over-copy
  ("f"            (Copy 4 0 0 (0 7 8 9)    "f"))            ; dst not clz
)


;;; Store Uniq ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tests
  ("Hella W#" (Snore 0 0 5 "Hella#" "Hello W#")))


;;; Repeated Value ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (Rep hd val count) => hd[val..]

(tests
  ("a"               (Rep "a" "v" 0))
  (("a" "v")         (Rep "a" "v" 1))
  (("a" "v" "v")     (Rep "a" "v" 2))
  ("a"               (Rep "a" "v" Rep))
  (("a" Rep Rep Rep) (Rep "a" Rep 3))
  ("a"               (Hd (Rep "a" (Error 0) 3)))) ; value is lazy


;;; Pointer Equality ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (silly-refeq-test x)
  (@ tmp [x x])
  (RefEq (Ix0 tmp) (Ix1 tmp)))

(tests
  (0 (RefEq foo [1 2 3]))
  (1 (silly-refeq-test [1 2 3])))

(defun (streamed-rev rem row)
  (@ i (Dec rem))
  (And rem
    (0 (Ix i row) (streamed-rev i row))))

(defun (stream-rev row)
  (streamed-rev (Sz row) row))

(tests
  ( [3 [2 [1 []]]]
    (stream-rev [1 2 3])))
