(require "lambda")
;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

;;; Plan To Wisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (pinWisp planWisp deep x)
  (let* ((full (planWisp deep x))
         (n full)
         (p full)
         (l (["CURLED" [(1 (Name x)) (1 (Arity x)) ".."]]))
         (a (cons (planWisp deep (Hd x)) (map (const "..") x))))
    (0 "PIN"
      (If deep full
        (Case4 (Type x) n p l a)))))

(tests
  ((pinWisp "x" NO 5)        ["PIN" ("x" 0 5)]                     )
  ((pinWisp "x" NO (Pin 5))  ["PIN" ("x" 0 (Pin 5))]               )
  ((pinWisp "x" NO {x -> x}) ["PIN" ["CURLED" [(1 0) (1 1) ".."]]] ))

(defun (planWisp deep x)
  (let* ((r   (planWisp deep))
         (nat (1 x))
         (law (0 "CURLED" [(r (Name x)) (r (Arity x)) (r (Body x))]))
         (pin (pinWisp planWisp deep (Unpin x)))
         (app (cons (planWisp deep (Hd x)) (map (planWisp deep) x))))
    (Case4 (Type x) nat pin law app)))

(tests
  ((planWisp NO 5)             '(5))
  ((planWisp NO "asdf")        '("asdf"))
  ((planWisp NO (3 4))         '((3 4)))
  ((planWisp NO {x y -> x})    '({0 2 1}))
  ((planWisp NO {x y --> x})   '(PIN{0 2 ..}))
  ((planWisp NO ((3 4) (5 6))) '((3 4 (5 6)))))


;;; Line Printer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (wispShowWrap t rest)
  (let* ((az (Case3 t "()" "[]" "{}"))
         (a  (Load8 0 az))
         (z  (Load8 1 az)))
    (Seq2 a z
      (lcons a (lsnoc rest z)))))

(defun (wispShowVal deep wispShowLazy v)
  (Ifz (IsNat v)
    (lcons "#"
      (wispShowLazy deep (planWisp deep v)))
    (If (Or (Lt v 256) (Nil (strAll isPrint v)))
      (lsing (showNat v))
      [dquote [v [dquote []]]])))

(defun (*(wispCase) x[v] val sym exp)
  (let
    ((exprCase
      (Ifz (Eq 2 (Sz x))        (exp 0 x)
        (If (Eq "BRACED" (Ix0 x)) (exp 1 (Ix1 x))
          (If (Eq "CURLED" (Ix0 x)) (exp 2 (Ix1 x))
            (exp 0 x))))))
    (Case3 (Hd x) exprCase (*(val) v) (*(sym) x))))

(defun (wispShowLazy deep x)
  (wispCase x (wispShowVal deep wispShowLazy) lsing
    {t ps ->
      (wispShowWrap t
        (lcat
          (lintersperse (lsing " ")
            (lmap (wispShowLazy deep) (stream ps)))))}))

(defun (wispShow deep x)
  (lstrcat (wispShowLazy deep x)))

(tests
  ( (wispShow 1 '(()))        "()"       )
  ( (wispShow 1 '(a))         "a"        )
  ( (wispShow 1 '([f x]))     "[f x]"    )
  ( (wispShow 1 '({f x}))     "{f x}"    )
  ( (wispShow 1 '((f x)))     "(f x)"    )
  ( (wispShow 1 '((f x)))     "(f x)"    )
  ( (wispShow 1 '((f 3)))     "(f 3)"    )
  ( (wispShow 1 (1 (Pin 2)))  "#(PIN 2)" )
  ( (wispShow 1 (1 {x -> x})) "#{0 1 1}" ))


;;; Pretty Print ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (wispPrettyWrap t rest)
  (let* ((az (Case3 t "()" "[]" "{}"))
         (a  (Load8 0 az))
         (z  (Load8 1 az)))
    (Seq2 a z
      (lcons a (lsnoc rest z)))))

(defun (wispPrettyVal deep wispPrettyLazy d v)
  (Ifz (IsNat v)
    (lcons "#"
      (wispPrettyLazy deep d (v)))
    (If (Or (Lt v 256) (Nil (strAll isPrint v)))
      (lsing [(showNat v)])
      [dquote [v [dquote 0]]])))

(defun (fits rem more[s ss])
  (And rem
    (Ifz more 1
      (fits (Sub rem (Bytes s)) ss))))

(defun (indent n)
  (lstrcat (lcons 10 (lrep " " n))))

(tests
  ((indent 3) (strWeld 10 "   ")))

(defun (wispPrettyLazyExp wispPrettyLazy deep d x t ps)
  (let* ((d (Inc d))
         (z (Sz ps)))
    (Ifz z         (Error ("PrettyEr" "impossible" d z x))
      (If (Eq 1 z) (Error ("PrettyEr" "impossible" d z x))
        (wispPrettyWrap t
          (lcat
            (lintersperse (lsing (indent d))
              (lmap (wispPrettyLazy deep d) (stream ps)))))))))

(defun (wispPrettyLazy deep d x)
  (let ((oneline (wispShowLazy deep x)))
    (lsing
      (lstrcat
        (If (fits 60 oneline) oneline
          (wispCase x (wispPrettyVal deep wispPrettyLazy d) lsing
            (wispPrettyLazyExp wispPrettyLazy deep d x)))))))

(defun (wispPretty deep x)
  (lstrcat (wispPrettyLazy deep 0 x)))

(tests
  ( (wispPretty 1 '(()))            "()"       )
  ( (wispPretty 1 '(a))             "a"        )
  ( (wispPretty 1 '([f x]))         "[f x]"    )
  ( (wispPretty 1 '({f x}))         "{f x}"    )
  ( (wispPretty 1 '((f x)))         "(f x)"    )
  ( (wispPretty 1 '((f x)))         "(f x)"    )
  ( (wispPretty 1 '((f 3)))         "(f 3)"    )
  ( (wispPretty 1 (1 (Pin 2)))      "#(PIN 2)" )
  ( (wispPretty 1 (1 {x -> x}))     "#{0 1 1}" )
  ( (wispPretty 1 (1 drop))         "#()"      )) ; TODO: wtf?

(defun (print-sexpr x)
  (xputstrLn (wispPretty 1 x)))

(defun (print x)
  (xputstrLn (wispPretty 0 (planWisp 0 x))))

(defun (__PRINT x)
  (If (Eq 1 (Hd x))
    (print (.0 x))
    (print-sexpr x)))

lgenfrom
