;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

(= (wispPretty x)
  (lstrcat (wispPrettyLazy 0 x)))

(tests
  ( (wispPretty (EXP PAR 0))                  "()"       )
  ( (wispPretty (SYM "a"))                    "a"        )
  ( (wispPretty (EXP BAR (SYM "f", SYM "x"))) "[f x]"    )
  ( (wispPretty (EXP CUR (SYM "f", SYM "x"))) "{f x}"    )
  ( (wispPretty (EXP PAR (SYM "f", SYM "x"))) "(f x)"    )
  ( (wispPretty (EXP PAR (SYM "f", SYM "x"))) "(f x)"    )
  ( (wispPretty (EXP PAR (SYM "f", VAL 3)))   "(f 3)"    )
  ( (wispPretty (VAL Inc)                     "#<2>"     ))
  ( (wispPretty (VAL {x x}))                  "#{0 1 1}" ))

;;; Reader ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (xreadc fd)
  (@ r (xread fd 256 1 0))
  (If (Ne 1 (Hd r)) 0
    (Word8 (Ix0 r))))

(= (loadNat acc str)
  (@ n (Sub Word8-str "0"))
  (Seq acc
    (Ifz str acc
      (loadNat (Add n (Mul 10 acc)) (Rsh str 8)))))

(assert-equal 1230
  (loadNat 0 "01230"))

(= (wispReadSym o fd acc)
  (@ c   (xreadc fd))
  (@ acc (Add acc (Lsh c o)))
  (Strict 2 o acc
    (Ifz (isSymChr c)
        (Sap c
          (If (strAll isNatChr acc) (Sap VAL (loadNat 0 acc))
            (Sap SYM acc)))
      (wispReadSym (Add o 8) fd acc))))

(= (wispEatSpace fd c)
  (Ifz (isSpcChr c) c
    (wispEatSpace fd (xreadc fd))))

(= (wispReadStrLoop fd o acc)
  (@ c (xreadc fd))
  (Ifz c (Throw ("ReadErr" "unterminated string"))
    (If (Eq c dquote) (Sap (xreadc fd) (VAL acc))
      (loop (Add 8 o) (Add acc (Lsh c o))))))

(= (wispReadStr fd)
  (wispReadStrLoop fd 0 0))

(= (eatComment fd)
  (@ c (xreadc fd))
  (Ifz c (Throw ("ReadErr" "eof"))
    (If (Eq 10 c) c
      (eatComment fd))))

(= (wispReadExprLoop exprRead ty fd acc c)
  (@ c (wispEatSpace fd c))
  (@ r (expRead fd c))
  (@ x (fst r))
  (Strict 2 acc r
    (If (Eq c end) (Sap (xreadc fd) (EXP ty acc))
      (wispReadExprLoop fd (acc x) (Hd r)))))

(= (wispReadExpr expRead fd ty end acc)
  (wispReadExprLoop exprRead ty fd acc (xreadc fd)))

(= (expRead fd c)
  (@ c (wispEatSpace fd c))
  (Seq c
    (cond ((Eq c dquote) (wispReadStr fd))
          ((Eq c ";")    (expRead fd (eatComment fd)))
          ((Eq c "(")    (wispReadExpr expRead fd PAR ")" []))
          ((Eq c "[")    (wispReadExpr expRead fd BAR "]" []))
          ((Eq c "{")    (wispReadExpr expRead fd CUR "}" []))
          ((Eq c "<")    (wispReadExpr expRead fd POI ">" []))
          ((isSymChr c)  (wispReadSym 8 fd c))
     (Throw ("ReadErr" "unexpected" c)))))


;;; REPL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (wispRepl fd c)
  (@ c     (wispEatSpace fd c))
  (@ r[!x] (expRead fd c))
  (@ !_    (xputstrLn (wispPretty x)))
  (And c (wispRepl fd Hd-r)))

(= (main cli) (wispRepl 0 (xreadc 0)))
