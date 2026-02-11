;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

(= Law    (Pin 1))
(= Inc    (Pin 2))
(= Eat    (Pin 3))
(= XLog   (Pin 4))
(= XTry   (Pin 8))
(= XWrite (Pin 9))
(= XRead  (Pin 10))
(= XDie   (Pin "die"))

(= (xtrace m x)           (XLog   (0 m x)))
(= (xdbg x)               (XLog   (0 x x)))
(= (xtry f x)             (XTry   (0 f x)))
(= (xwrite fd nat sz off) (XWrite (0 fd nat sz off)))
(= (xread fd nat sz off)  (XRead  (0 fd nat sz off)))

(= (Force x) (0 x))
(= (Force x) Law)
(= (Force x) (Law 0 1 0 0))
(= (Force x) (0 0 1 (0 0) 0))
(= (Force x) (Law 0 1 (0 x) 0))

(= (Seq x y)
  (Eat {{y} _ y} {{y} _ _ _ y} {{y} _ _ y} y {{y} _ y} x))

(= (Strict n x)
  (Eat {{x} _ x}
       {{x} _ _ _ x}
       {{x} _ _ x}
       x
       {{Strict x} m (Seq x (Strict m))}
       n))

(= (IsPin x) (Eat {_ 1} {_ _ _ 0} {_ _ 0} 0 {_ 0} x))
(= (IsLaw x) (Eat {_ 0} {_ _ _ 1} {_ _ 0} 0 {_ 0} x))
(= (IsApp x) (Eat {_ 0} {_ _ _ 0} {_ _ 1} 0 {_ 0} x))
(= (IsNat x) (Eat {_ 0} {_ _ _ 0} {_ _ 0} 1 {_ 1} x))
(= (Type x)  (Eat {_ 1} {_ _ _ 2} {_ _ 3} 0 {_ 0} x))
(= (Truth x) (Eat {_ 1} {_ _ _ 1} {_ _ 1} 0 {_ 1} x))
(= (Nil x)   (Eat {_ 0} {_ _ _ 0} {_ _ 0} 1 {_ 0} x))
(= x 3)

(= x 99)

(= (Unpin x) (Eat {x x} {_ _ _ 0} {_ _ 0} 0 {_ 0} x))
(= (Name x)  (Eat {_ 0} {n _ _ n} {_ _ 0} 0 {_ 0} x))
(= (Arity x) (Eat {_ 0} {_ a _ a} {_ _ 0} 0 {_ 0} x))
(= (Body x)  (Eat {_ 0} {_ _ b b} {_ _ 0} 0 {_ 0} x))
(= (Init x)  (Eat {_ 0} {_ _ _ 0} {h _ h} 0 {_ 0} x))
(= (Last x)  (Eat {_ 0} {_ _ _ 0} {_ t t} 0 {_ 0} x))

(= (If x t e)
  (Eat
    {{t} _ t}
    {{t} _ _ _ t}
    {{t} _ _ t}
    e
    {{t} _ t}
    x))

;= (Ifz x t e)   (Eat {_ e} {_ _ _ e} {_ _ e} t {_ e}                  x))

(= (Nat x) (Eat {_ 0} {_ _ _ 0} {_ _ 0} 0 {{x} _ x} x))
(= (Dec x) (Eat {_ 0} {_ _ _ 0} {_ _ 0} 0 {x x}     x))
(= x 3)

(= (times f z x)
  (Eat
    {{z} _ z}
    {{z} _ _ _ z}
    {{z} _ _ z}
    z
    (times f (f z))
    x))

(= x 3)

(= (Fresh x) (Eat {_ 0} {_ _ _ 0} {f x (f x)} 0 Inc x))

; (tests
;   ( (Fresh 77)    77               )
;   ( (4 5)         (Strict 1 3 4 5) )
;   ( 5             (Strict 2 3 4 5) )
;   ( (Strict 0)    (Strict 3 3 4 5) )
;   ( (Strict 1)    (Strict 4 3 4 5) )
;   ( (Fresh (0 1)) (0 1)            ))

(= NO 0)(= YES 1)

(= (And x y)   (If x y x))       ; x,y
(= (Or x y)    (If x x y))       ; x,y
(= (Xor x y)   (If x (Nil y) y)) ; x,y
(= (Nand x y)  (If x (Nil y) 1)) ; x,y
(= (Nor x y)   (If x 0 (Nil y))) ; x,y
(= (Xnor x y)  (If x y (Nil y))) ; x,y
(= (** else x) x)                ; x,y

(= not Nil)

(= (Add x y) (times Inc (Nat x) y)) ; y,x
(= (Sub x y) (times Dec (Nat x) y)) ; y,x
(= (Mul x y) (times (Add x) 0 y))   ; y,x
(= (Pow b p) (times (Mul b) 1 p))   ; p,b
(= (Bex p)   (Pow 2 p))             ; p

;;;;

(= (Hd x)
  (Eat {{x} _ -> x}
       {{x} _ _ _ -> x}
       {{Hd} h _ -> (Hd h)}
       x
       {{x} _ -> x}
       x))

(= sbSize Hd)

(= (sb k v l r)
  (@ sz (Inc (Add (sbSize l) (sbSize r))))
  (sz k v l r))

(= (Sz v)
  (Eat {_ 0} {_ _ _ 0} {{Sz} h t (Inc (Sz h))} 0 {_ 0} v))

(= (case r i)
  (Last (times Init r i)))

(= (Case i r f)
  (Eat {{f} _ f}
       {{f} _ _ _ f}
       {{f} _ _ f}
       f
       (case r)
       (Sub (Sz r) i)))

(= (Ix i r)
  (Case i r 0))

(= (sbOpen x k)
  (k (Hd x) (Ix 0 x) (Ix 1 x) (Ix 2 x) (Ix 3 x)))

(= (singleR key val left c)
  (@ _ {{key val c} _ koy vol a b ->
         (sb koy vol a (sb key val b c))})
  (sbOpen left _))

(= (Ge x y) (Nil (Sub y x)))
(= (Eq x y) (Nor (Sub x y) (Sub y x)))

(= (testsLoop expr n acc i)
  (@ itm (Ix i expr))
  (@ chk (0 "assert" (0 (1 Eq) (Ix 0 itm) (Ix 1 itm))))
  (If (Ge i n) acc
    (testsLoop expr n (acc chk) (Inc i))))

(define-macro (tests ws expr)
  (0 ws
    (testsLoop expr (Sz expr) (0 "block") 1)))

(tests
  (0 (Type 0))          ; nat
  (1 (Type Inc))        ; pin
  (2 (Type {x y -> x})) ; law
  (3 (Type (0 0))))     ; app

(= (planArity o)
  (@ item (Unpin o))
  (@ p (If (IsNat item)
         (Case item (0 1 3 1 6) 1)
         (planArity item)))
  (@ l (Arity o))
  (@ a (Sub (planArity (Hd o)) (Sz o)))
  (@ n 0)
  (Ix (Type o) (0 n p l a)))

(assert (Eq 1 (planArity planArity)))
(assert (Eq 0 (planArity 5)))
(assert (Eq 1 (planArity (Pin 5))))

= (x 1)

; '(= (ingestAst compile s x (= st [env nex]))
;   (@ go (ingestAst compile))
;
;   (case x (###{ingestAst: bad input} x)
;     (V i [st (listIdx i s)])
;     (M x (go s x st))
;     (G g [st (VAL (** bindValue (PinItem g)))])
;     (K x [st (VAL x)])
;
;     (A f x
;       (= [st f] (go s f st))
;       (= [st x] (go s x st))
;       [st (foldingApp f x)])
;
;     (L v b
;       (= [[env nex] vr]
;         (go s v [env nex]))
;       (If (Eq APP (Hd vr))
;          (let
;            ((k   nex)
;             (nex (Inc nex))
;             (env (bstPut env k vr)))
;           (go (lcons (VAR k) s) b [env nex]))
;         (go (lcons vr s) b [env nex])))
;
;     (R vs b
;       (= nBinds (Sz vs))
;       (= ks     (Gen nBinds Add-nex))
;       (= nex    (Add nex nBinds))
;       (= ss     (lweld (stream (map VAR ks)) s))
;       (= st     (^ foldl _ [env nex] (Zip vs ks)
;                  {st [vx k] =>
;                    (= [[env nex] vr] (go ss vx st))
;                    [(bstPut env k vr) nex]}))
;       (go ss b st))
;
;     (F lam
;       (= LAM[pin _mark _rec tag lArg lBod] lam)
;       (= slf             (nex))
;       (= nex             (Inc nex))
;       (= arg             (listGen lArg (Add nex)))
;       (= nex             (Add nex lArg))
;       (= s2              (listWeld (lmap VAR listRev-arg) (VAR slf :: s)))
;       (= [[bin nex] bod] (go s2 lBod [bstEmpty nex]))
;       (= [cns free]      (compile nex (FUN pin tag slf arg bin bod)))
;       (= _               (lfoldl APP (VAL cns) (lmap VAR free)))
;       (Seq nex [[env nex] _]))))

(= x 0)
