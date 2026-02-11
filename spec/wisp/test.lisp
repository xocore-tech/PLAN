; This is an include, it needs to be the first form in the file, but a
; comment before it is no problem.

<base


; Number literals

(0 45 "hi")


; (PIN x) is a special form

(PIN 0)
((PIN 0) 1)


; (DEF sym x) is a special form

(DEF x 3)
(DEF PIN (PIN 0))
(DEF lawOp (PIN 1))
(PIN x)


; Build and run some functions using primops

(DEF const PIN(lawOp (0 1 "Const" 1)))
(const 3 4)

; Exercise the (LAW (nm arg...) bind.. body) special form

(DEF eat    (LAW (eat a b)    eat))
(DEF const  (LAW (const a b)  a))
(DEF ignore (LAW (ignore a b) b))

(DEF c 9)
(DEF const9 (LAW (const9 b) c))
(const9 3)

(DEF x 9)
(LAW (f x)      x)        ; {2 102 1}
(LAW (f x)      (EVAL x)) ; {2 102 0[9]}
(LAW (f EVAL x) (EVAL x)) ; {2 102 0[1 2]}

(LAW (0 a b) a)      ; self is unbound, but the tag is 0
(LAW ("asdf" a b) a) ; self is unbound, but the tag is "asdf"

; Wisp accepts unicode symbols, but doesn't validate them.

(DEF ø (LAW (ø x y z) (z (y x))))
(ø 3 4 5)


; Empty applications is zero?  Maybe it should just crash.

()


; Macros

(MAC ' (LAW (' e x) (0 e (1 x))))
'{x y => ~[x]}


;; WIP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEF mkLaw
  PIN(LAW (mkLaw a m b)
       (lawOp (0 a m b))))

(mkLaw 1 2 3)

(DEF primCase (PIN 2))

(DEF Case
  (PIN (LAW (Case p l a z m o)
         (primCase (0 p l a z m o)))))

(DEF getArity
  (PIN
    (LAW (getArity a m b)
      a)))

(DEF Arity
  (PIN
    (LAW (Arity x)
      (Case (const 0) getArity (const 0) (const 0) (const 0) x))))

; TODO: macros need to be expanded in LAW bodies as well.

(DEF Inc
  PIN(LAW (Inc x)
       (Arity (mkLaw x))))
 Inc
 (Inc 3)

; Uncomment the following and see that we have overwritten the core
; WISP evaluation logic (because the last input crashes).

;(MAC EVAL
;  (PIN
;    (LAW (EVAL env x)
;       ((PIN "DIE") ("fake eval" x)))))
;(3 4 5)
