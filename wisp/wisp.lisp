(require "base")
;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (xnewline fd)
  (xwrite fd 10 1 0))

(defun (xwriteAll fd nat rem0 off0)
  (@ wrote (xwrite fd nat rem0 off0))
  (@ off1  (Add off0 wrote))
  (@ rem1  (Sub rem0 off1))
  (And rem1
    (Seq3 wrote off1 rem1
      (xwriteAll fd nat rem1 off1))))

(def NIL 0)(def CONS 0)(def lcons 0)

(defun (xputstr nat)    (xwriteAll 1 nat (Bytes nat) 0))
(defun (xputstrLn nat)  (Seq (xputstr nat) (xnewline 1)))
(defun (lgenfrom i n f) (And (Lt i n) (CONS (f i) (lgenfrom (Inc i) n f))))
(defun (lgen n f)       (lgenfrom 0 n f))
(defun (gen n f)        (Row 0 n (lgen n f)))
(defun (min x y)        (If (Gt x y) y x))
(defun (slash v s e)    (gen (Sub e s) {{s v} i -> (Ix (Add s i) v)}))
(defun (slice v s e)    (slash v s (min e (Sz v))))
(defun (drop n v)       (slice v n (Sz v)))
(defun (take n v)       (slice v 0 n))
(defun (map f v)        (gen (Sz v) {{f v} x -> (f (Ix x v))}))

(defun (weld x y)
  (@ xw (Sz x))
  (@ yw (Sz y))
  (gen (Add xw yw)
    {{x y xw} i ->
      (If (Lt i xw) (Ix i x)
        (Ix (Sub i xw) y))}))

(tests
  ([1 2 3 4] (weld [1 2] [3 4]))
  ([1 2]     (weld [1 2] 0))
  ([3 4]     (weld 0 [3 4])))

(defun (linterLoop sep list)
  (And list
    [sep [ (Ix0 list)
           (linterLoop sep (Ix1 list)) ]]))

(defun (lintersperse sep list)
  (@ x  (Ix0 list))
  (@ xs (Ix1 list))
  (And list (lcons x (linterLoop sep xs))))

(tests
  ( (lintersperse 3 [0 [1 [2 0]]])
    [0 [3 [1 [3 [2 0]]]]]))

(defun (lfoldl f z0 l)
  (@ z1 (f z0 (Ix0 l)))
  (Ifz l z0
    (Seq z1
      (lfoldl f z1 (Ix1 l)))))

(defun (lfoldr f z l)
  (Ifz l z
    (f (Ix0 l) (lfoldr f z (Ix1 l)))))

( defun (lrev xs)      (lfoldl {x y -> [y x]} NIL xs) )
( defun (cons x xs)    (weld [x] xs)                  )
( defun (lsing x)      [x 0]                          )
( defun (lcat ls)      (lfoldr lweld NIL ls)          )
( defun (foldl f z xs) (lfoldl f z (stream xs))       )
( defun (cat vs)       (foldl weld 0 vs)              )
( def   ~              stream                         )

(tests
  ( ~[1 2 3]
    [1 [2 [3 0]]] )

  ( (lrev ~[1 2 3])
    ~[3 2 1] )

  ( ~[1 2 3 4 5 6]
    (lcat ~[~[1] ~[2 3] ~[] ~[4 5 6]])))

(defun (strGenLoop n f acc i)
  (Seq acc
    (If (Ge i n) acc
      (strGenLoop n f (Store8 i (f i) acc) (Inc i)))))

(defun (strGen n f)
  (strGenLoop n f 0 0))

(defun (lexplode str)
  (lgen (Bytes str) {{str} i -> (Load8 i str)}))

(defun (limpLoop i acc list)
  (@ x  (Ix0 list))
  (@ xs (Ix1 list))
  (Seq2 i acc
    (Ifz list acc
      (limpLoop (Inc i) (Store8 i x acc) xs))))

(defun (limplode list)
  (limpLoop 0 0 list))

(tests
  ("abc" (limplode (lexplode "abc"))))

(defun (lfilter f l)
  (@ x (Ix0 l))
  (@ rest (lfilter f (Ix1 l)))
  (And l
    (If (f x) [x rest] rest)))

(defun (lsz l)
  (lfoldr {x acc -> (Inc acc)} 0 l))

(defun (array l)
  (Row 0 (lsz l) l))

(defun (lall f l)
  (Nil (lfilter {{f} x -> (Nil (f x))} l)))

(tests
  (1 (lall even ~[0 2 4]))
  (0 (lall even ~[0 1 2 4])))

(defun (strAll f str)
  (lall f (lexplode str)))

(tests
  ((lfilter even ~[1 2 3 4]) ~[2 4])
  ((lfilter odd ~[1 2 3 4])  ~[1 3]))

(defun (ltake n l)
  (And n
    (And l
      [(Ix0 l) (ltake (Dec n) (Ix1 l))])))

(defun (lenumFrom n)
  (Seq n
    [n (lenumFrom (Inc n))]))

(tests
  ( (ltake 5 (lenumFrom 0)) ~[0 1 2 3 4] )
  ( (lsz ~[1 2 3 4])        4            )
  ( (array ~[1 2 3 4])      [1 2 3 4]    ))

(defun (ldropWhile f list)
  (@ x (Ix0 list))
  (@ xs (Ix1 list))
  (And list
    (If (f x) (ldropWhile f xs)
      list)))

(defun (ltakeWhile f list)
  (@ x (Ix0 list))
  (@ xs (Ix1 list))
  (And list
    (And (f x)
      (0 x (ltakeWhile f xs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (lfoldrix f z l0 i)
  (@ x (Ix0 l0))
  (@ l (Ix1 l0))
  (Seq i
    (Ifz l0 z
      (f i x (lfoldrix f z l (Inc i))))))

(defun (lfoldri f z l)
  (lfoldrix f z l 0))

(lfoldri "f" "END" ~[1 2 3])

(defun (lmap f l)
  (lfoldr {{f} x xs -> [(f x) xs]} 0 l))

(defun (lmapi f l)
  (lfoldri {{f} i x xs -> [(f i x) xs]} 0 l))

(defun (lindy i list)
  (And list
    (Seq i
      [[i (Ix0 list)] (lindy (Inc i) (Ix1  list))])))

(tests
  ( (lmapi 0 ~[3 4 5])
    ~[[0 3] [1 4] [2 5]] )
  ( (lindy 0 ~[3 4 5])
    ~[[0 3] [1 4] [2 5]] ))


;;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (strWeld a b)
  (Add (Lsh b (Lsh (Bytes a) 3)) a))

(defun (lstrcat xs)
  (lfoldl strWeld 0 xs))

(defun (decimal n)
  (And n
    [(Mod n 10) (decimal (Div n 10))]))

(defun (showNat n)
  (Ifz n "0"
    (lstrcat (lrev (lmap (Add "0") (decimal n))))))

(tests
  ("abcdefg" (lstrcat ~["a" "bc" "d" "" "efg"])))

(tests
  ("0"   (showNat 0))
  ("1"   (showNat 1))
  ("100" (showNat 100)))


;;; Quoting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro (' env x)
  [env (1 (Ix0 (Ix1 x)))])

(tests
 ("x"                  '(x))
 (["a" "b"]            '((a b)))
 (["BRACED" ["a" "b"]] '([a b])))


;;; Destructuring ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BaseLawEnv = (Row Sym, Row (Sym, Exp))

; BindAst = (Row Sym, Row (Sym, Exp))
;   | NAM Str
;   | TUP (Maybe Sym) (Row Bind)

(defun (gensym i)
  (strWeld "__" (showNat i)))

(defun (ixExp n x)
  [ (1 (Case3 n Ix0 Ix1 (Ix n)))
    x
  ])

(tests
  ((ixExp 0 "a") [(1 Ix0) "a"]))

(defun (tupBind nm i bind)
  [bind (ixExp i nm)])

; tupBinds : Uniq -> List Bind -> List (Bind, Exp)
(defun (tupBinds u binds)
  (lmapi (tupBind u) binds))

; destroyBind : Bind -> Uniq -> (Maybe Sym, List (Bind, Exp))

(defun (destroyBind bind u)
  (@ name  (Or (Ix0 bind) (gensym u)))
  (@ elems (Ix1 bind))
  (If (IsNat bind)
    [bind NIL]
    [name (tupBinds name (stream elems))]))

(tests
  ( (destroyBind "hi"         5) ["hi"  ~[]                    ] )
  ( (destroyBind ["a" []]     5) ["a"   ~[]                    ] )
  ( (destroyBind [0 []]       5) ["__5" ~[]                    ] )
  ( (destroyBind ["a" ["x"]]  5) ["a"   ~[["x" [(1 Ix0) "a"]]] ] )

  ( (destroyBind ["a" ["x" "y" "z"]] 5)
    ["a" ~[["x" [(1 Ix0)    "a"] ]
           ["y" [(1 Ix1)    "a"] ]
           ["z" [(1 (Ix 2)) "a"] ]]] ))

; destroyLets : List (Bind, Exp) -> Uniq -> List (Bind, Exp)

(defun (destroyLets lets i)
  (@ l    (Ix0 lets))
  (@ ls   (Ix1 lets))
  (@ bind (Ix0 l))
  (@ expr (Ix1 l))
  (@ temp (destroyBind bind i))
  (@ name (Ix0 temp))
  (@ more (Ix1 temp))
  (And lets
    [[name expr] (destroyLets (lweld more ls) (Inc i))]))

; destroyVars : List Bind -> -> (Uniq, List Sym, List (Bind, Exp))

(defun (destroy args nuvars lets i)
  (@ step (destroyBind (Ix0 args) i))
  (Ifz args
    [nuvars (array (destroyLets lets i))]
    (destroy
      (Ix1 args)
      (nuvars (Or (Ix0 step) "__unused"))
      (lweld (Ix1 step) lets)
      (Inc i))))

(tests
  ((destroy ~["aa" ["bb" ["xx" "yy" [0 ["pp" "qq"]]]]] 0 0 0)
   [["aa" "bb"]
    [[ "xx"  [ (1 Ix0)    "bb"  ]]
     [ "yy"  [ (1 Ix1)    "bb"  ]]
     [ "__4" [ (1 (Ix 2)) "bb"  ]]
     [ "pp"  [ (1 Ix0)    "__4" ]]
     [ "qq"  [ (1 Ix1)    "__4" ]]]]))

(defun (bindType arg)
  (@ hed (Ix0 arg))
  (If (IsNat arg) 0
    (If (Ne 2 (Sz arg)) Error("badarg" arg)
      (If (Eq "BRACED" hed) 1
        2))))

(tests
  0(bindType '(x))
  1(bindType '([x y]))
  2(bindType '(x[x y])))

(defun (bindParseVec parseBind bind)
  [0 (map parseBind (Ix1 bind))])

(defun (bindParseAlias parseBind bind)
  (Up 0 (Ix0 bind)
    (parseBind (Ix1 bind))))

(defun (parseBind bind)
  (Case3 (bindType bind)
    bind                              ; symbol
    (bindParseVec parseBind bind)     ; [v e c]
    (bindParseAlias parseBind bind))) ; alias[v e c]

(tests
  ( (parseBind '(res[pp qq]))
    ["res" ["pp" "qq"]]
  )
)

(defun (parseLet let)
  (If (Or (Ne 3 (Sz let))
          (Ne "@" (Ix0 let)))
    Error("badlet" let)
    [ (parseBind (Ix1 let)) (Ix2 let) ]))

(tests
  ( (parseLet '((@ x 3)))      ["x"           (1 3)]   )
  ( (parseLet '((@ [x y] 3)))  [[0 ["x" "y"]] (1 3)]   )
  ( (parseLet '((@ a[x y] 3))) [["a" ["x" "y"]] (1 3)] )
)

; (lambda (self args..) (let) (let) body)

(defun (letExp bx)
  (@ nm (Ix0 bx))
  (@ bd (Ix1 bx))
  ["@" nm bd])

(defun (funExp self body pair)
  (@ args (Ix0 pair))
  (@ lets (Ix1 pair))
  (cat
    [["law" (cons self args)]
     (map letExp lets)
     [body]]))

(def oldlaw law)

(defmacro (law env x)
  (@ sig  (Ix1 x))
  (@ self (Ix0 sig))
  (@ args (map parseBind (drop 1 sig)))
  (@ lets (map parseLet (Init (drop 2 x))))
  (@ body (Last x))
  (oldlaw env
   (funExp self body (destroy (stream args) 0 (stream lets) 0))))

  ; Error[("self" self) ("args" args) ("lets" lets) ("body" body)]

; (defun (destroy args nuvars lets i)

(defun (uncurry fun [a1 a2])
  (@ res[pp qq] (fun a1 a2))
  res)

(tests
  (7 (uncurry Add [3 4])))


;;; Character Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (packClass strs)
  (lfoldl (flip Set) 0 (lcat (lmap lexplode (stream strs)))))

(defun (inClass charClass i)
  (Test i charClass))

(defun (unpackClass ccls)
  (lstrcat
    (lfilter (inClass ccls)
      (ltake (Bits ccls)
        (lenumFrom 0)))))

(def lowers "abcdefghijklmnopqrstuvwxyz")
(def uppers "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def digits "0123456789")
(def specials "!#$%&'*+,-./:=?@\^_`|~<>")
(def matchers "()[]{}")
(def alphaNums (lstrcat ~[lowers uppers digits]))
(def tab 9)
(def newline 10)
(def space 32)
(def spaces (lstrcat ~[tab newline space]))
(def semi ";")
(def dquote 34) ; double-quote character

(def lowChrs   (packClass [lowers]))
(def uprChrs   (packClass [uppers]))
(def symChrs   (packClass [lowers uppers digits specials]))
(def spcChrs   (packClass [spaces]))
(def natChrs   (packClass [digits]))
(def prnChrs   (packClass [alphaNums specials matchers spaces semi]))
(def opnChr    (packClass ["([{"]))
(def alnumChrs (packClass [alphaNums]))

(tests
  ("([{"        (unpackClass opnChr)  )
  ("0123456789" (unpackClass natChrs) ))

(defun (isAlnum c)   (Test c alnumChrs))
(defun (isNatChr c)  (Test c natChrs))
(defun (isLower c)   (Test c lowChrs))
(defun (isUpper c)   (Test c uprChrs))
(defun (isSymChr c)  (Test c symChrs))
(defun (isSpcChr c)  (Test c spcChrs))
(defun (isPrint c)   (Test c prnChrs))
(defun (strNone f s) (strAll {{f} c (Nil (f c))} s))

(tests
  ((unpackClass symChrs)
   (strWeld "!#$%&'*+,-./0123456789:<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            "\^_`abcdefghijklmnopqrstuvwxyz|~"))

  ( (strAll  isSymChr "abcDEF345_^!'"               ) 1)
  ( (strNone isSymChr " ()[]{};"                    ) 1)
  ( (strNone isNatChr  "abcdefghijklmnopqrstuvwxyz" ) 1)
  ( (strNone isNatChr  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ) 1)
  ( (strAll  isNatChr  "01234556789"                ) 1)
  ( (strAll  isUpper   "BIND"                       ) 1)
  ( (strNone isNatChr  "_~!@#$%^&*-=|/\:?,."        ) 1)
  ( (strNone isNatChr  " ()[]{};"                   ) 1)

  ( (lall isSpcChr                  ~[" " 9 10]) 1 )
  ( (lall {x -> (Nil (isSpcChr x))} ~["a" "("])  1 ))


;;; Reader ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (loadNum acc list[c cs])
  (Seq acc
    (Ifz list acc
      (loadNum (Add (Mul 10 acc) (Sub c "0")) cs))))

(tests
  (12 (loadNum 0 ~["0" "1" "2"])))

(defun (Sap f x)    (Seq x (f x)))
(defun (Sap2 f x y) (Seq2 x y (f x y)))

(defun (symbol _ cs k)
  (@ scs (ltakeWhile isSymChr cs))
  (@ exp (Ifz (lall isNatChr scs)
           (limplode scs)
           (Sap 1 (loadNum 0 scs))))
  (Sap2 k exp (ldropWhile isSymChr cs)))

(tests
  ( (limplode (lexplode "abcd"))   "abcd"         )
  ( (loadNum 0 ["1" ["2" ["3"]]])  123            )
  ( (symbol 0 ["a" ["b" ["("]]] 0) ["ab" ["("]]   )
  ( (symbol 0 ["1" ["2" ["("]]] 0) [(1 12) ["("]] ))

(defun (juxt prefix read s0 k)
  (prefix read s0
    {{read k} item s1[c] ->
      (Ifz (Test c opnChr) (k item s1)
        (read s1
          {{k item} x s2 ->
            (k (0 item x) s2)}))}))

(tests
  ((juxt symbol "read" ["1" ["2" [" " ["("]]]] 0)
   [(1 12) [" " ["("]]]))

(def XReadErr (Pin "ReadErr"))

(defun (unexpected _read cs[c] k)
  (XReadErr ("unexpected" c)))

(defun (ignore read stream k)
  (@ cs (Ix1 stream))
  (read cs k))

(defun (skipComment list)
  (ldropWhile (Ne newline) list))

(defun (skipSpace s[c cs])
  (If (Eq ";" c)     (skipSpace (skipComment cs))
    (If (isSpcChr c) (skipSpace cs)
      s)))

(def ex
  (lexplode
     "; hello
    a"))

(tests
  (~["a"] (skipSpace ex))
  (~["a"] (skipSpace ~[" " " " "a"])))

(defun (nestloop read mk end acc s0 k)
  (@ stream[c cs] (skipSpace s0))
  (Seq acc
    (If (Eq c end) (Sap2 k (mk acc) cs)
      (read stream
        {{nestloop read mk end acc k} item rest ->
          (nestloop read mk end (acc item) rest k)}))))

(defun (nested read stream[c cs] k)
  (If (Eq c "(") (nestloop read id           ")" 0 cs k)
    (If (Eq c "[") (nestloop read (0 "BRACED") "]" 0 cs k)
      (nestloop read (0 "CURLED") "}" 0 cs k))))

(defun (expLoadStr cs)
  (limplode (ltakeWhile (Ne dquote) cs)))

(defun (string _ stream[c cs] k)
  (Sap2 k
    (Sap 1 (expLoadStr cs))
    (Ix1 (ldropWhile (Ne dquote) cs))))

(def strExample
  (lexplode (lstrcat ~[dquote "hello" dquote "asdf"])))

(tests
  ((string "read" strExample 0)
   (0 (1 "hello") (lexplode "asdf"))))

(def expTable
  (strGen 128
    {c ->
      (If (Eq dquote c) 1
        (If (Test c opnChr) 2
          (If (isSymChr c) 3
            0)))}))

(defun (read cs0 k)
  (@ cs[c] (skipSpace cs0))
  (Ix (Load8 c expTable)
    [unexpected string nested (juxt symbol)]
    read cs k))

(def ex1 (lexplode "asdf #"))
(def ex2 (lexplode "0123 #"))
(def ex3 (lexplode (lstrcat ~[dquote "asdf" dquote " #"])))
(def ex4 (lexplode "  (a b c) #"))
(def ex5 (lexplode "foo(a b c) #"))
(def ex6 (lexplode "{a b c} #"))
(def ex7 (lexplode "[a b c] #"))
(def end (lexplode " #"))

(tests
  ((0 "asdf"                   end) (read ex1 0))
  ((0 (1 123)                  end) (read ex2 0))
  ((0 (1 "asdf")               end) (read ex3 0))
  ((0 ["a" "b" "c"]            end) (read ex4 0))
  ((0 ["foo" ["a" "b" "c"]]    end) (read ex5 0))
  ((0 ["CURLED" ["a" "b" "c"]] end) (read ex6 0))
  ((0 ["BRACED" ["a" "b" "c"]] end) (read ex7 0)))

(defun (load sxp[_ sig[self]])
  (@ tag  (If (IsNat self) self (Ix0 self)))
  (@ lets (Init (drop 2 sxp)))
  [tag (map Nat sig) (map {[_ b c] -> (b c)} lets) (Last sxp)])

(tests
  ((load ["law" ["f" "x"] "x"])
   ["f" ["f" "x"] [] "x"])

  ((load
     ["law"
       ["f" "x"]
       ["@" "xx" ["x" "x"]]
       ["@" "xxxx" ["xx" "xx"]]
       ["x" "xx" "xxxx"]])
   ["f" ["f" "x"]
        [("xx" ["x" "x"]) ("xxxx" ["xx" "xx"])]
        ["x" "xx" "xxxx"]])

  ((load ["law" [(1 5) "x"]
           ["@" "xx" ["x" "x"]]
           ["@" "xxxx" ["xx" "xx"]]
           ["x" "xx" "xxxx"]])
     [5 [0 "x"]
        [("xx" ["x" "x"]) ("xxxx" ["xx" "xx"])]
        ["x" "xx" "xxxx"]]))


;;; Environments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def nullenv 0)

(defun (getenv key x)
  (@ k (Ix0 x))
  (@ v (Ix1 x))
  (@ m (Ix2 x))
  (@ l (Ix3 x))
  (@ r (Ix4 x))
  (And x
    (Case3 (Cmp key k)
      (getenv key l)
      x
      (getenv key r))))

(defun (putenv key val mac x)
  (@ k (Ix0 x))
  (@ v (Ix1 x))
  (@ m (Ix2 x))
  (@ l (Ix3 x))
  (@ r (Ix4 x))
  (Ifz x [key val mac]
    (Case3 (Cmp key k)
      [k v m (putenv key val mac l) r]
      [key val mac l r]
      [k v m l (putenv key val mac r)])))

(defun (testenv seq)
  (lfoldl {e x -> (putenv x x x e)} nullenv seq))

(tests
  (( testenv ~[5])     [5 5 5]                     )
  (( testenv ~[5 4])   [5 5 5 [4 4 4] 0]           )
  (( testenv ~[5 6])   [5 5 5 0 [6 6 6]]           )
  (( testenv ~[5 6 7]) [5 5 5 0 [6 6 6 0 [7 7 7]]] )
  (( testenv ~[5 6 1]) [5 5 5 [1 1 1] [6 6 6]]     ))

(tests
  (( putenv 7 2 2 (testenv ~[5 6 7])) [5 5 5 0 [6 6 6 0 [7 2 2 0 0]]] ))

(tests
  ( (getenv 4 (testenv ~[5 6 7])) 0                           )
  ( (getenv 5 (testenv ~[5 6 7])) [5 5 5 0 [6 6 6 0 [7 7 7]]] )
  ( (getenv 6 (testenv ~[5 6 7])) [6 6 6 0 [7 7 7]]           )
  ( (getenv 7 (testenv ~[5 6 7])) [7 7 7]                     )
  ( (getenv 8 (testenv ~[5 6 7])) 0                           ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (mkenv seq)
  (lfoldl {e [k v m] -> (putenv k v m e)} nullenv seq))

(defun (lawenvStep env [ix nm])
  (putenv nm ix 0 env))

(defun (lawenv [tag sig lets body])
  (lfoldl lawenvStep nullenv
    (lindy 0 (lweld (stream sig) (lmap Hd (stream lets))))))

(tests
  ((lawenv
     (load
       ["law" ["f" "x"]
              ["@" "xx" ["x" "x"]]
              ["@" "xxxx" ["xx" "xx"]]
              ["x" "xx" "xxxx"]]))
   (mkenv ~[["f" 0] ["x" 1] ["xx" 2] ["xxxx" 3]])))

(tests
  ((lawenv
     (load
       ["law" [(1 0) "x"]
              ["@" "xx" ["x" "x"]]
              ["@" "xxxx" ["xx" "xx"]]
              ["x" "xx" "xxxx"]]))
   (mkenv ~[[0 0] ["x" 1] ["xx" 2] ["xxxx" 3]])))

(def ThrowOp (Pin "Throw"))
(def Throw ThrowOp)

(defun (resolve g l x)
  (@ exp (Case3 (Sz x) (1 0) (resolve g l (Ix0 x))
           (map (resolve g l) x)))
  (@ val x)
  (@ gb[_ gv] (getenv x g))
  (@ lb[_ lv] (getenv x l))
  (@ sym (If lb lv        ;  variable reference number
           (If gb (1 gv)  ;  constant
             (ThrowOp ("undefined-reference" x)))))
  (Case3 (Hd x) exp val sym))

(tests
  ((lawenv
     (load
       ["law" ["f" "x"]
              ["@" "xx" ["x" "x"]]
              ["@" "xxxx" ["xx" "xx"]]
              ["x" "xx" "xxxx"]]))
   (mkenv ~[["f" 0] ["x" 1] ["xx" 2] ["xxxx" 3]])))

(defun (app-is-code is-outer-expr x)
  (@ hx (Hd x))
  (Case3 (Dec (Sz x))
    (Nil hx)                                    ; 0[a]   -> 1
    (Or (Nil hx) (And is-outer-expr (Eq 1 hx))) ; 0[a b] -> 1
    0))                                         ; _      -> 0

; TODO: Maybe we should just always quote constants?  The lambda
; compiler can include a better code generator, and we can move
; the wisp quine downstream of that.
;
; This code is the only code where we need Case4, Type, Le, and this
; case costs 6 lines in the output.  So, this costs 9 lines out of ~250.
;
; DONE: should we also reload __READ at each step so that the reader
; can be extended?
;
; TODO: similarly, maybe we should get rid of xtry?  Since we can redefine
; __REPL now, maybe in the kernel we can just crash on bad inputs.
;
; Superficially, this would interfere with the ability to use Wisp for
; testing, but we can just have a debug build of the runtime system
; which prints errors.

(defun (isCode is-outer-expr maxref v)
  (Case4 (Type v)
    (Le v maxref)                   ; Nat
    0                               ; Pin
    0                               ; Law
    (app-is-code is-outer-expr v))) ; App

(tests
  ( (isCode 0 5 (1 9))   0 )
  ( (isCode 0 5 (0 9))   1 )
  ( (isCode 1 5 (1 0 0)) 1 )
  ( (isCode 0 5 (1 0 0)) 0 )
  ( (isCode 1 5 (0 0 0)) 1 )
  ( (isCode 0 5 (0 0 0)) 1 )
  ( (isCode 0 5 0)       1 )
  ( (isCode 0 5 5)       1 )
  ( (isCode 0 5 6)       0 ))

(defun (lit top maxref v)
  (If (isCode top maxref v) (0 v) v))

(def NO 0)
(def YES 1)

(defun (expr top maxref x[a b])
  (If (IsNat x) x
    (If (Hd x)
      (lit top maxref a)
      (If (Eq 1 (Sz x))
        (expr top maxref a)
        [(expr NO maxref (Init x))
         (expr NO maxref (Last x))]))))

(defun (genloop maxRef lets[l ls] body)
  (Ifz lets (expr YES maxRef body)
    (1 (expr NO maxRef l)
      (genloop maxRef ls body))))

(defun (codegen glo func[tag sig[name] lets body])
  (@ loc    (lawenv func))
  (@ arity  (Dec (Sz sig)))
  (@ maxRef (Add arity (Sz lets)))
  (MkLaw tag arity
    (genloop maxRef
      (lmap (compose (resolve glo loc) Ix0) (stream lets))
      (resolve glo loc body))))

(tests
  ((MkLaw "self" 1 0)
   (codegen 0 ["self" ["self" "ex"] [] "self"]))

  ((MkLaw "self" 1 [0])
   (codegen 0 ["self" ["self" "ex"] [] 0]))

  ((MkLaw "self" 1 2)
   (codegen 0 ["self" ["self" "ex"] [] (1 2)]))

  ((MkLaw "self" 1 [[0 1] 1])
   (codegen 0 ["self" ["self" "ex"] [] ["self" "ex" "ex"]]))

  ((MkLaw "self" 1 (1 1 (1 2 3)))
   (codegen 0 ["self" ["self" "ex"] [("a" "ex") ("b" "a")] "b"]))
)

(defun (tstep f old[s0 acc] x0)
  (@ [s1 x1] (f s0 x0))
  [s1 (acc x1)])

(defun (traverse st f row)
  (lfoldl (tstep f) [st 0] (stream row)))

(tests
  ( (traverse 5 {st x -> [(Inc st) (Add st x)]} [3 4 5])
    [8 [8 10 12]]
  ))

(defun (getmacro glo sym)
  (@ r (getenv sym glo))
  (And (Ix2 r) ; macro flag must be set
    (Ix1 r)))

(defun (expoType mac x0)
  (And (Nil (Hd x0))             ; not (a..)   -> DONE
    (And x0                      ; ()          -> DONE
      (Ifz (IsNat (Ix0 x0)) 1    ; (weird a..) -> RECUR (TODO: what is this for)
        (Ifz mac 1               ; not a macro -> RECUR
          2)))))                 ; macro       -> EXPAND

(defun (expo e0 x0)
  (@ mac     (getmacro e0 (Ix0 x0)))
  (@ [e1 x1] (mac e0 x0))
  (xtrace ("expo" x0)
    (Case3 (expoType mac x0)
      [e0 x0]                 ; not an expression, return
      (traverse e0 expo x0)   ; expand each sub-expression
      (expo e1 x1))))         ; process macro-expansion

(defun (Apply expr)
  (Coup (Ix0 expr) (drop 1 expr)))

(tests
 5(Apply [Add 2 3]))

(defun (evalExpr opeval e1 x)
  (@ [e2 vals] (traverse e1 opeval x))
  (Case3 (Sz x)
    [e1 (1 0)]
    (opeval e1 (Ix0 x))
    [e2 (Apply vals)]))

(defun (evalSym env nm)
  (@ res[_ val]  (getenv nm env))
  (Ifz res (Throw (strWeld "undefined-" nm))
    [env val]))

(defun (opeval e0 x0)
  (@ [e1 x1] (expo e0 x0))
  (Case3 (Hd x1)
    (evalExpr opeval e1 x1)
    [e1 (Ix0 x1)]
    (evalSym e1 x1)))

(def x9 ["x" 9])

(tests
  ([x9 9] (opeval x9 (1 9)))
  ([x9 9] (opeval x9 "x"))
)

(defun (letExpo e0 let[x0])
  (@ [e1 x1] (expo e0 x0))
  (xtrace "letExpo"
    [e1 (Up 0 x1 let)]))

(defun (lawExpo e0 f[fTag fSig fLets fBody])
  (@ [e1 lets] (traverse e0 letExpo fLets))
  (@ [e2 body] (expo e1 fBody))
  (xtrace "lawExpo"
  (xtrace ("lawExpo" e0 f)
    (xtrace ("lawExpo.e1" e1)
      [e2 [fTag fSig lets body]]))))

(defun (ticMacro env x)
  (Ifz (Eq 2 (Sz x)) (Throw ("ticMacro" x))
    [env (1 (Ix1 x))]))

(defun (oplaw e0 f)
  (@ out[e1 fv] (lawExpo e0 (load f)))
  (xtrace ("oplaw.out" out)
  (xtrace ("oplaw.e1" e1)
  (xtrace ("oplaw.fv" fv)
  (xtrace ("oplaw.gen" (codegen e1 fv))
    [e1 (1 (codegen e1 fv))])
))))

(def opdefine
  (law (define isMacro env [_ sig val])
    (@ [env value] (Force (opeval env val)))
    [(putenv sig value isMacro env)
     (1 value)]))

(defun (replstep e0 x)
  (@ res[[e1 out]] (xtry (eval e0) x))
  (xtrace x
    (Ifz (Hd res)
      (xtrace out [e1])
      (xtrace ("ERROR" res) 1))))

(defun (repl e0 cs)
  (@ cs (skipSpace cs))
  (And cs
    (read cs
      {{e0} x cs ->
        (@ out[e1] (replstep e0 x))
        (@ repl (Ix1 (getenv "__REPL" e1)))
        (If (Nat out) out
          (repl e1 cs))})))

(def env0
  (mkenv
    [["law"    oplaw        1]
     ["macro"  (opdefine 1) 1]
     ["def"    (opdefine 0) 1]
     ["Pin"    Pin          0]
     ["eval"   opeval       0]
     ["__REPL" repl         0]]))

(def testenv
  (putenv "nine"     9          0
  (putenv "'"        ticMacro   1
  (putenv "defvalue" (opdefine 0) 1
  env0))))

(def asdf
  (oplaw testenv
    ["law" ["func" "ex"]
           ["@" "xx" ["ex" "ex"]]
           ["@" "xxxx" ["xx" "xx"]]
           ["func" "ex" "xx" "xxxx" "nine"]]))

(tests
  ( [testenv
     (1 (MkLaw "func" 1
          (1 [1 1]
            (1 [2 2]
              ([[[[0 1] 2] 3] 9])))))]

     (oplaw testenv
       ["law" ["func" "ex"]
              ["@" "xx" ["ex" "ex"]]
              ["@" "xxxx" ["xx" "xx"]]
              ["func" "ex" "xx" "xxxx" "nine"]])))

(tests
  ((oplaw testenv
     ["law" ["f" "x"]
            ["@" "xx" ["x" "x"]]
            ["@" "xxxx" ["xx" "xx"]]
            ["'" ["f" "x" "xx" "xxxx" "nine"]]])
   (0 testenv
     (1
       (MkLaw "f" 1
         (1 [1 1]
           (1 [2 2]
             (["f" "x" "xx" "xxxx" "nine"])))))))) ; quoted

(tests
  ((opeval testenv
     ["law" ["f" "x"]
              ["@" "xx" ["x" "x"]]
              ["@" "xxxx" ["xx" "xx"]]
              ["x" ["law" ["eat" "x"] "eat"]]])
   [testenv
     (MkLaw "f" 1
       (1 [1 1]
         (1 [2 2]
           [1 {eat a => eat}])))]))

; Could derive the pointer again each time, but keeping it instead.
; It is stable.  Ptr is invalidated if buf is ever GC'd, so need to make
; sure the reference stays live by touching the buffer with Seq at some
; point when it is no longer needed.

(defun (streamBuf buf n i)
  (@ ci (Load8 i buf))
  (And (Lt i n)
    (Seq ci
      ; VERY important that this be evaluated right here, because if it
      ; is delated until later, then the value of the buffer will change
      ; because we are mutating it.
      [ci (streamBuf buf n (Inc i))])))

(def SYS_READ 0) ; amd64 syscall number is zero

(defun (strTake n s)
  (limplode (ltake n (lexplode s))))

(defun (readFileLoop buf ptr fd)
  (@ n (syscall (SYS_READ fd ptr 1)))
  (Seq2 n buf
    ; make sure the buffer stays alive until the very end
    ; and is not garbage collected.
    (And n
      (lweld (streamBuf buf n 0) (readFileLoop buf ptr fd)))))

(defun (readFile fd)
  (@ buf (XBufOp 128))
  (readFileLoop buf (XPtrOp buf) fd))

(defun (main cli)
  (repl env0 (readFile 0)))
