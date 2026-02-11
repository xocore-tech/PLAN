(require "data0")
;;;; Copyright (c) 2026 xoCore Technologies
;;;; SPDX-License-Identifier: MIT
;;;; See LICENSE for full terms.

;;;; Lambda Compiler
;;;; ===============
;;;;
;;;; The language of functions provided by the PLAN itself is extremely
;;;; bare-bones.  There are no nested functions, and each function
;;;; contains only one top-level letrec binding.
;;;;
;;;; This is a compiler from a more expressive lambda-calculus like
;;;; language to PLAN.  It supports:
;;;;
;;;; -   Nested let bindings.
;;;;
;;;; -   Non-recursive let bindings (names are shaddowed instead of
;;;;     recursion).
;;;;
;;;; -   Nested lambdas (via lambda lifting)
;;;;
;;;; -   Inlining (but only where specifically requested).
;;;;
;;;; Furthermore, we perform some basic optimizations:
;;;;
;;;; -   Unused let bindings are eliminated.
;;;;
;;;; -   Single-use let bindings are eliminated (inlined into the
;;;;     expression).
;;;;
;;;;     -   However, single-use let bindings are *not* eliminated if
;;;;         the only reference comes from a nested lambda, since this
;;;;         could effect how many times the expression is evaluted.
;;;;
;;;; -   Let bindings which bind small constants are eliminated (we can
;;;;     instead directly rely on sharing).
;;;;
;;;; -   Let bindings which trivially rebind another let binding are
;;;;     eliminated, as they contribute nothing to the result except
;;;;     runtime overhead.
;;;;
;;;; These basic optimizations (and inlining) mean that you can write
;;;; code in a more natural style without generating bad PLAN code.
;;;;
;;;; But an even more important reason is that using the macro system
;;;; used to do meta-programming tends to generate extremely a lot of
;;;; pointless bindings and single-use bindings, and having those concerns
;;;; be taken care of by a basic optimizer significantly increases the
;;;; utility of meta-programming.
;;;;
;;;; Oh, and these optimizations are also necessary for inlining too,
;;;; since inlining will convert each argument into a let binding,
;;;; and these arguments maybe be used only once (or not at all).
;;;; Without basic optimization, the resulting code would be very silly.
;;;;
;;;; This compiler code will be use for two different things:
;;;;
;;;; 1.  Right away within Wisp, to extend the syntax of the defun,
;;;;     lambda, and CURL ({a -> b}) macros with additional
;;;;     expressiveness.
;;;;
;;;; 2.  As a foundational component of the Sire language, which is built
;;;;     using Wisp.
;;;;
;;;; The inlining support is multi-step, as inlining a function can
;;;; expose additional opportunities for inlining.  In order to inline
;;;; global bindings, we will need not just the PLAN value of the binding,
;;;; but also the expression that was used to create it.
;;;;
;;;; Sire always tracks this information.  In Wisp, we can simply add
;;;; the information to the end of each binding, and replace the =defun=
;;;; macro with a new version which includes this information.

(defstruct Globe GLOBE       ; Global Binding
  (bind-key Nat)             ;   Name
  (bind-val Any)             ;   Value
  (bind-exp Expr))           ;   The expression which created the binding.

(deftype Expr                ; Expression Tree
  (V Nat)                    ;   local reference (de-bruijn index)
  (K Any)                    ;   constant value
  (G Globe)                  ;   global reference
  (A Expr Expr)              ;   function application
  (L Expr Expr)              ;   let binding
  (R (Row Expr) Expr)        ;   recursive let binding
  (M Expr)                   ;   Request Inline
  (F (Unpack Function)))     ;   Nested lambda expression

(defstruct Function FL       ; Lambda Expression
  (lam-pinned Bit)           ;   Should the function be pinned?
  (lam-marked Bit)           ;   Should we attempt to inline this?
  (lam-recur  Bit)           ;   Is this recursive? (cannot inline if recursive)
  (lam-name   Nat)           ;   What PLAN Law metadata should be assigned?
  (lam-arity  Nat)           ;   Function Arguments
  (lam-body   Expr))         ;   The actual lambda expression body.

(defstruct Arg ARG           ; Potentially-Inlined Argument
  (arg-depth Nat)            ;   depth (TODO: what mean?)
  (arg-expr  Nat))           ;   expr  (TODO: what mean?)

(defstruct Pot POT           ; The Inlining Potential of an Expression
  (pot-func Function)        ;   The function that could maybe be inlined
  (pot-mark Bool)            ;   Is there a mark requesting an inline?
  (pot-deep Nat)             ;   depth (TODO: what mean?)
  (pot-need Nat)             ;   more arguments needed before inlining.
  (pot-args (List Arg)))     ;   Arguments provided thus far.

(defstruct Res RES           ; The result of the inlining pass
  (res-exp Expr)             ;   The expression
  (res-pot (Maybe Pot)))     ;   When we can inline, but need more args.


;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro (# env x)
  (@ sym (Ix0 (Ix1 x)))
  (Ifz (Nat sym)
    (Error ("#: bad input" x))
    [env [(1 sym) sym]]))

(defun (all f xs)
  (lall f (stream xs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; One Step Of Inlining
;;; --------------------
;;;
;;; We inline whenever we encounter an expression which is marked for
;;; inlining, and which has been given all of it's arguments.
;;;
;;; This "marking" can either be when a function is marked as "always
;;; inline" or when the expression itself has (at any point in the
;;; expression) been marked for inlining.
;;;
;;; Inlining a function encoded in de-bruijn indexes is very simple at
;;; first glance, each argument-expression becomes a let-binding, and
;;; then the body is just copied in place without any change.
;;;
;;; However, there are two things which complicate this.
;;;
;;; 1.  Whenever we bind one of those arguments to a let binding,
;;;     the environment of each subsequent binding has one additional
;;;     thing in it, which will require renumbering.
;;;
;;; 2.  In addition to inlining functions which are globally bound,
;;;     we *also* support inlining functions which are locally bound
;;;     (in a let-binding, for example).  This works the same, except
;;;     that references into the shared context need to be renumbered,
;;;     since the code is being moved into a deeper nesting context.
;;;
;;; The procedures needed to perform these re-numberings are mechanically
;;; simple, but quite difficult to understand.  This is why de-bruijn
;;; indexes are generally not used in complex compilers.  However,
;;; we only have to do a tiny bit of this stuff, and it saves a lot
;;; of work.

;; The =move-to= routine moves an expression into a deeper nesting
;; context.  References to bindings introduced within the expression
;; are unchanged, but free variables need to get bigger to accommodate
;; the additional things in the enclosing environment.

(def lhead .0)
(def ltail .1)

(defun (llen xs)
  (lfoldl {x _ -> (Inc x)} 0 xs))

(defun (lsnoc list[x xs] v)
  (Ifz list (lsing v)
    [x (lsnoc xs v)]))

(defun (lhas e list[x xs])
  (And list
    (Or (Eq e x)
      (lhas e xs))))

(defun (lfind-loop i e list[x xs])
  (Seq i
    (And list
      (If (Eq e x) [i]
        (lfind-loop (Inc i) e xs)))))

(defun (lfind e list)
  (lfind-loop 0 e list))

(defun (lfind-known e list)
  (.0 (lfind e list)))

(defun (lany f list[x xs])
  (And list
    (Or (f x)
      (lany f xs))))

(defun (lrange i)
  (Seq i
    [i (lrange (Inc i))]))

(defun (lix i list[x xs])
  (And list
    (Ifz i x
      (lix (Dec i) xs))))

(defun (lix-error ctx i list[x xs])
  (Ifz list
    Error("lix-error" ctx)
    (Ifz i x
      (lix-error ctx (Dec i) xs))))

(defun (lrep i n)
  (lgen n (const i)))

(defun (lcatmap f r)
  (lcat (lmap f r)))

(defun (move-to-loop from to l e)
  (@ mv (move-to-loop from to))
  (@ a  (.0 e))
  (@ b  (.1 e))
  (@ ll (Add l (Sz a)))
  (union-switch e
    V(If (Lt a l) e (V (Sub (Add a to) from)))
    M(M (mv l a))
    A(A (mv l a) (mv l b))
    L(L (mv l a) (mv (Inc l) b))
    R(R (map (mv ll) a) (mv ll b))
    F(lam-body= (mv (Inc (Add l (.lam-arity e))) (.lam-body e)) e)
    K(e)
    G(e)))

(defun (move-to from to already-bound top)
  (If (Eq from to) top
    (move-to-loop from to already-bound top)))

;; =renum= renumbers references within each function argument, as we bind
;; each one to a let binding.

(defun (renum d n args[a[ad ax] as])
  (Seq n
    (Ifz args NIL
      (lcons
        (move-to ad (Add d n) 0 ax)
        (renum d (Inc n) as)))))

;; =expand-pot= performs the actual inlining step.  Each argument is mapped
;; to a let binding after renumbering their environments to accommodate
;; this.  And then the actual law body is copied inline, while also
;; renumbering it's free variables as needed.
;;
;; Since lambda bodies also have a self-reference in scope, we will need
;; to bind something for that as well.  However, since we only inline
;; functions which are not recursive, we know that this binding will
;; never be used, so we just bind a constant zero.  This binding will
;; get optimized away later, and adding a dummy binding here is much
;; simpler than doing a more complex renumbering of the lambda body.

(defun (expand-pot d pot)
  (@ lam (.pot-func pot))
  (lfoldr L (move-to (.pot-deep pot)
                     d
                     (Inc (.lam-arity lam))
                     (.lam-body lam))
    (renum d 0
      (lcons (ARG d (K 0))
        (lrev (.pot-args pot))))))

;; =pot-is-full= checks to see if the inline potential of a result is
;; ready for inline expansion.  This is true if:
;;
;; 1.  The potential is actually inhabited (it wont be if we don't
;;     statically know which function is being called, for example).
;;
;; 2.  If the expression is marked for inlining (either the expression
;;     contains an inline mark or the function itself is marked to always
;;     be inlined).
;;
;; 3.  If the expression is a saturated function call.  =pot-need=
;;     indicates the number of expression still pending before
;;     saturation, so if this is zero, the expression is saturated.

(defun (pot-is-full maybe-pot[pot])
  (And maybe-pot
    (And (Nil (.pot-need pot)) (.pot-mark pot))))

;; =reapply-step= applies an additional argument to the result of the
;; inlining pass, producing another inline-result object.
;;
;;     reapply-step : Depth -> Res -> Arg -> Res
;;
;; A inline-result object is an expression and an inlining-potential.
;;
;; The expression part of the result is just the previous expression
;; applied to the expression of the argument.  Because these arguments
;; might be coming somewhere else, we need to re-number free variables
;; (usually a noop).
;;
;; The inline-potential is nothing, if the previous potential was nothing,
;; or if the previous potential was saturated.  Otherwise, it's the same as
;; the previous potential, but with one additional argument added.

(defun (reapply-step d res arg)
  (@ mpot[pot] (.res-pot res))
  (@ need      (.pot-need pot))
  (RES
    (A (.res-exp res)
       (move-to (.arg-depth arg) d 0 (.arg-expr arg)))
    (And mpot
      (And need
        (SOME (pot-need=(Dec need)
                (pot-args=(lcons arg (.pot-args pot))
                  pot)))))))

;; =reapply= is the part of the inline pass which performs the actual
;; inlining
;;
;;     reapply : Depth -> List Arg -> Res -> Res
;;
;; The inline pass first transforms an expression into an inline potential
;; and a list of pending arguments, and then calls this to actually
;; process the arguments.
;;
;; At each step, if the inline potential is ripe, we perform the actual
;; inline expansion.  Otherwise, we apply the arguments one by one,
;; performing the same check each time.

(defun (reapply inline depth scope args f)
  (@ maybe-pot[pot] (.res-pot f))
  (DeepSeq f
    (If (pot-is-full maybe-pot)
      (inline depth scope args
        (expand-pot depth pot))
      (Ifz args f
        (reapply inline depth scope (ltail args)
          (reapply-step depth f (lhead args)))))))

; =inline=

(defun (inline-constant inline depth scope params x)
  (reapply inline depth scope params
    (RES x 0)))

(defun (inline-local inline depth scope params x[v])
  (reapply inline depth scope params
    (RES x (lix-error ("inline-local" v "in" scope) v scope))))

(defun (inline-global inline depth scope params x[p])
  (reapply inline depth scope params
    (RES x
      (.res-pot (inline depth NIL NIL (.bind-exp p))))))

(defun (res-set-mark res)
  (@ mpot[pot] (.res-pot res))
  (Ifz mpot res
    (res-pot=(pot-mark=(1) pot)
      res)))

(defun (inline-mark inline depth scope params [b])
  (reapply inline depth scope params
    (res-set-mark (inline depth scope NIL b))))

;; =inline-letrec= runs the inlining pass on a nested lambda expression.

(defun (inline-lambda inline depth scope params lam)
  (@ [_ lmark lrecr _ largs lbody] lam)
  (@ s_ (lweld (lrep NONE (Inc largs)) scope))
  (@ d_ (Inc (Add largs depth)))
  (reapply inline depth scope params
    (RES
      (lam-body=(.res-exp (inline d_ s_ NIL lbody)) lam)
      (And (Nil (.lam-recur lrecr))
        [(POT lam lmark depth largs NIL)]))))

;; =inline-letrec= runs the inlining pass on a letrec expression.

(defun (inline-letrec inline depth scope params x[vs b])
  (@ nbinds (Sz vs))
  (@ d_ (Add depth nbinds))
  (@ s_ (lweld (lrep NONE nbinds) scope))
  (@ vr (map (inline d_ s_ NIL) vs))
  (@ br (inline d_ s_ params b))
  (RES (R (map .res-exp vr) (.res-exp br)) NONE))

;; =inline-let= just runs the inlining pass on both the variable and
;; the body.
;;
;; Note that the inline potential of the binding is added to the scope.
;; This makes it possible to inline locally bound functions in the body.
;;
;; Also note that all of the parameters which we have collected are
;; passed into the body.
;;
;; We never inline through let bindings, so the inline potential is
;; always uninhabited.

(defun (inline-let inline depth scope params x[v b])
  (@ [vrs vre]  (inline depth       scope             NIL    v))
  (@ [brs _bre] (inline (Inc depth) (lcons vre scope) params b))
  (RES (L vrs brs) NONE))

;; =inline-apply= inlines an application expression.  This basically
;; just adds the function argument to the list of pending parameters
;; and then recurs back into the inlining pass.
;;
;; This explicit accumulation of arguments makes the pass more flexible,
;; allowing the inliner to "see through" things like nested lets.
;;
;; Note that we run the inlining pass on the argument before
;; adding it to the list.

(defun (inline-apply inline depth scope params [func arg])
  (@ RES[x _] (inline depth scope NIL arg))
  (inline depth scope (lcons (ARG depth x) params) func))

; inline : Depth > List (Maybe Pot) > List Arg > Expr > Res

(defun (inline depth scope params x)
  ((union-switch x
     K(inline-constant)
     V(inline-local)
     G(inline-global)
     M(inline-mark)
     F(inline-lambda)
     R(inline-letrec)
     L(inline-let)
     A(inline-apply)
   ) inline depth scope params x))

(def idlam
  (F 0 1 0 "Id" 1 (V 0)))

(defun (res-no-pot x)
  (RES x 0))

(tests

  ((RES (A (K 1) (K 2)) 0)
   (inline 0 0 NIL (A (K 1) (K 2))))

  ((res-no-pot
     (L (K 0) ; self-ref
     (L (K 2) ; argument
     (V 0)))) ; idlam body
   (inline 0 0 NIL (A idlam (K 2))))

  ((res-no-pot
     (L (K 0)     ; self-ref
     (L (K 2)     ; argument
     (A (V 0)     ; idlam body
        (K 3))))) ; additional argument
   (inline 0 0 NIL (A (A idlam (K 2)) (K 3))))

)

;;; Compiler
;;; --------
;;;
;;; Once we have done inlining, we need to translate lambda expressions
;;; to PLAN while performing the appropriate optimizations.

(deftype Exp
  (VAL Any)
  (VAR Nat)
  (APP Exp Exp))

(defstruct Fun FUN
  (fun-pinned Bool)
  (fun-name Nat)
  (fun-self Nat)
  (fun-args (List Nat))
  (fun-binds (Tab Nat Exp))
  (fun-body Exp))

(defun (pinArity x)
  (@ i (Unpin x))
  (If (IsLaw i)
    (Arity i)
    1))

(defun (planArity x)
  (Case4 (Type x)
    0
    (pinArity x)
    (Arity x)
    (Dec (planArity (Init x)))))

;; =folding-apply= performs a function application at compile time,
;; if the application is partial.  This is an optimization which results
;; in slightly smaller laws.

(defun (is-constant-app f x)
  (And (And (Eq VAL (Hd f)) (Eq VAL (Hd x)))
    (Ne 1 (planArity (.0 f)))))

(defun (folding-apply f[fv] x[xv])
  (If (is-constant-app f x)
    (VAL (fv xv))
    (APP f x)))

(tests
  1(is-constant-app (VAL 1) (VAL 2))
  1(is-constant-app (VAL const) (VAL 2))
  0(is-constant-app (VAL id) (VAL 2))
  0(is-constant-app (VAR 0) (VAL 2)))

(tests
  ( (VAL (1 2))             (folding-apply (VAL 1) (VAL 2))     )
  ( (VAL (const 2))         (folding-apply (VAL const) (VAL 2)) )
  ( (APP (VAL id) (VAL 2))  (folding-apply (VAL id) (VAL 2))    ))

;; The first step of compilation is ingestion.  The input has nested
;; functions and let/letrec expression.  We instead transform that into
;;
;; -   A table of all bindings, each with a unique key.
;;
;; -   A tree of applications, where the leaves are either constants or
;;     variable references.
;;
;; When ingestion encounters a nested lambda, it just runs the full
;; compiler on it to produce a constant, or if lambda lifted, a constant
;; applied to one-or-more variable references.

(defun (bstPut e k v)
  (bal-insert k v e))

;; =ingest-mark= does nothing, we just ignore the mark.

(defun (ingest-mark ingest compile s st [x])
  (ingest compile s x st))

;; =ingest-local= just translates the reference into the new
;; representation (unique key per binding) by indexing into a stack.

(defun (ingest-local ingest compile s st [i])
  [st (lix-error "ingest-local" i s)])

;; =ingest-global= just produces a constant.

(defun (ingest-global ingest compile s st [g])
  [st (VAL (.bind-val g))])

;; =ingest-constant= just produces a constant.

(defun (ingest-constant ingest compile s st [x])
  [st (VAL x)])

;; =ingest-apply= ingests the function and the argument and then
;; either applies them right away (if it is a constant partial
;; application), or produces an application expression.

(defun (ingest-apply ingest compile s st0 [f_ x_])
  (@ [st1 f] (ingest compile s f_ st0))
  (@ [st2 x] (ingest compile s x_ st1))
  [st2 (folding-apply f x)])

;; =ingest-let= ingests the binding first.
;;
;; If it is an application, then we add the binding to the table of
;; all bindings, adds a reference to this binding to the scope, and then
;; ingests the body in the context of that scope.
;;
;; If the things being bound is a constant or a trivial alias, we do
;; *not* include the result in the table of bindings, and we instead
;; directly add the expression to the scope.
;;
;; By adding the expression to the scope, it will be inlined into the
;; reference site instead of being preserved as a let binding.

(defun (ingest-let ingest compile s [env0 nex0] [v b])
  (@ iletout[[env1 nex1] vr]
    (ingest compile s v [env0 nex0]))
  (@ is-app
    (Eq APP (Hd vr)))
  (@ new-scope
      (If is-app
        (lcons (VAR nex1) s)
        (lcons vr s)))
  (@ env2
      (If is-app
        (bal-insert nex1 vr env1)
        env1))
  (@ nex2
      (If is-app
        (Inc nex1)
        nex1))
  (ingest compile new-scope b [env2 nex2]))

;; =ingest-letrec= ingests a recursive let binding.
;;
;; This doesn't do any of the let optimizations, and it instead just
;; loads everything into the bindings table verbatim.

(defun (ingest-letrec-step ingest compile s st [vx k])
  (@ [[env nex] vr] (ingest compile s vx st))
  [(bal-insert k vr env) nex])

(defun (ingest-letrec ingest compile s [env nex0] [vs b])
  (@ nBinds (Sz vs))
  (@ ks     (gen nBinds (Add nex0)))
  (@ nex1   (Add nex0 nBinds))
  (@ s2     (lweld (stream (map VAR ks)) s))
    ; ^ everything is in scope in all bindings and in the body
  (ingest compile s2 b
    (foldl
      (ingest-letrec-step ingest compile s2)
      [env nex1]
      (zip vs ks))))

;; =ingest-lambda= ingests a function by compiling it and performing
;; lambda lifting.

(defun (ingest-lambda ingest compile s [env selfkey] lam)
  (@ argkey1  (Inc selfkey))
  (@ argkeys  (lgen (.lam-arity lam) (Add argkey1)))
  (@ next-key (Add argkey1 (.lam-arity lam)))

  (@ [[bin nex3] bod]
    (ingest compile
      (lweld (lrev (lmap VAR argkeys))
             (lcons (VAR selfkey) s))
      (.lam-body lam)
      [bal-empty next-key]))

  (@ [cns free]
    (compile nex3
      (FUN (.lam-pinned lam) (.lam-name lam) selfkey argkeys bin bod)))

  (Force
    [[env nex3]
     (lfoldl APP (VAL cns) (lmap VAR free))]))

(defun (ingest compile scope ast st[_ old])
  (@ result[[bin nex] rx]
    ((union-switch ast
       M(ingest-mark)
       V(ingest-local)
       G(ingest-global)
       K(ingest-constant)
       A(ingest-apply)
       L(ingest-let)
       R(ingest-letrec)
       F(ingest-lambda)
     ) ingest compile scope st ast))
  (If (Ne R (Hd ast)) result
    (Force result)))

;;; Analyse
;;; =======
;;;
;;; Once we have finished ingesting the AST, we do a simple analysis pass
;;; which provides two pieces of information:
;;;
;;; 1.  How many times is each binding referenced?
;;;
;;; 2.  A list of all referenences, in the order in which they appear.
;;;
;;; While we could produce a list of references from the the refcount
;;; table, we are going to use this to generate free lists, and we want
;;; the result to be in a predictable order, instead of being based on
;;; the internal details of the unique keys we used to identify each
;;; binding.
;;;
;;; During the acual analysis pass, we maintain a set of seen reference
;;; in order to deduplicate this reference list.  (TODO: why is the refcount
;;; table not sufficient for this)?

;; =analyse-val= does nothing, since we only care about references here.

(defun (analyse-val loop fun st _)
  st)

;; =analyse-app= just performs the analysis on both the function and
;; the argument.

(defun (analyse-app loop fun st [f x])
  (loop fun (loop fun st f) x))

(defun (bump-refcount k tab)
  (bal-insert k (Inc (bal-idx k tab)) tab))

;; =record-refrence= adds a reference to the analysis state.  We always
;; bump the refcount for the binding.  But if the refcount was zero,
;; then this is the first time we have recorded this, so we add the
;; reference to the list of all references.

(defun (record-reference k [seen counts lis])
  (Force
    [ seen
      (bump-refcount k counts)
      (If (bal-has k counts) lis (lcons k lis))
    ]))

(defun (is-ref-let-bound fun ref)
  (bal-has ref (.fun-binds fun)))

(defun (is-unseen-ref-to-let ref fun st[seen])
  (And (Nil (bal-has ref seen))
    (is-ref-let-bound fun ref)))

;; =analyse-var= adds a reference to the result.
;;
;; We don't eagerly process the actual expressions of each let binding,
;; since some of them will be unused, and should therefore be ignored.
;; So, we instead only process bound expressions when we first encounter
;; a reference to them.
;;
;; So, when we encounter a reference, the first thing we need to do is
;; to process the bound expression.  If we have already processed the
;; binding, or if the reference is to something besides a binding,
;; then we skip this.

(defun (analyse-var loop fun st[seen tab lis] [k])
  (record-reference k
    (Ifz (is-unseen-ref-to-let k fun st)
      st
      (loop fun [(bal-insert k k seen) tab lis]
        (bal-idx k (.fun-binds fun))))))

(defun (analyse-loop fun st sx)
  ((union-switch sx
     VAL(analyse-val)
     APP(analyse-app)
     VAR(analyse-var)
   ) analyse-loop fun st sx))

(defun (analyse fun)
  (@ [seen tab lis]
    (analyse-loop fun [bal-empty bal-empty NIL] (.fun-body fun)))
  [tab (lrev lis)])

(defun (should-keep-bind refcounts fn ref)
  (And (bal-search ref (.fun-binds fn))
    (Gt (bal-idx ref refcounts) 1)))

; Fun > (Tab Nat Nat, List Nat) > Any

(defun (is-single-use-let refcounts fun ref)
  (And
    (is-ref-let-bound fun ref)
    (Eq 1 (bal-idx ref refcounts))))

(defun (cgen-val outermost go table refcounts maxref fn [k])
  (If (isCode outermost maxref k) (0 k) k))

(defun (cgen-app go table refcounts maxref fn [f x])
  (0 (go table refcounts maxref fn f)
     (go table refcounts maxref fn x)))

(defun (cgen-var go table refcounts maxref fn [k])
  (If (is-single-use-let refcounts fn k)
    (go table refcounts maxref fn
      (bal-idx k (.fun-binds fn)))
    (bal-idx k table)))

(defun (bal-search key tree[k v l r])
  (And tree
    (Case3 (Cmp key k)
      (bal-search key l)
      [v]
      (bal-search key r))))

(defun (cgen-inner table refcounts maxref fn s)
  ((union-switch s
     VAL(cgen-val 0)
     VAR(cgen-var)
     APP(cgen-app))
    cgen-inner table refcounts maxref fn s))

; =cgen= compiles an expression into a law body.  This is in the outermost
; position, where values shaped like lets (1 x) must be quoted.

(defun (cgen table refcounts maxref fn s)
  ((union-switch s
     VAL(cgen-val 1)
     VAR(cgen-var)
     APP(cgen-app))
    cgen-inner table refcounts maxref fn s))

(defun (pin-if flag val)
  (If flag (Pin val) val))

(defun (debug note info result)
  (xtrace (note info #(result))
    result))

(defun (genlet-step table refcounts maxref fn k rest)
  (1
    (cgen table refcounts maxref fn (bal-idx k (.fun-binds fn)))
    rest))

(defun (codegen fn stat[refcounts refSeq])
  (@ fArg    (.fun-args fn))
  (@ fSlf    (.fun-self fn))
  (@ binds   (lfilter (should-keep-bind refcounts fn) refSeq))
  (@ nBind   (llen binds))
  (@ nArg    (llen fArg))
  (@ maxref  (Inc (Add nArg nBind)))
  (@ scope   (lcons fSlf (lweld fArg binds)))
  (@ table   (bal-from-pairs-list (lzip scope (lrange 0))))
  (Force
    (pin-if (.fun-pinned fn)
     (Law (.fun-name fn) nArg
       (lfoldr
         (genlet-step table refcounts maxref fn)
         (cgen table refcounts maxref fn (.fun-body fn))
         binds)))))

;; =is-free= checks if an identifier is bound within a function.
;; It is bound if it is the self-reference, an argument, or something
;; that we bind (TODO: what is `bin` exactly)?

(defun (is-free self bin args ref)
  (Nil
    (Or (Eq self ref)
      (Or (bal-has ref bin)
        (lhas ref args)))))

; Alright, seems like the the list of nats produced by analyse is used
; for lambda lifting.  And we need to filter it, so I guess it is a list
; of all references made in the function?
;
; And then the rest of the information seems to be used during codegen.

; Nat > Fun > (Any, List Nat)
(defun (compile nex f1[pin1 tag1 slf1 arg1 bin1 bod1])
  (@ stat1[_ refs1] (analyse f1))
  (@ free           (lfilter (is-free slf1 bin1 arg1) refs1))
  (@ newSelf        (lfoldl APP (VAR nex) (lmap VAR free)))
  (@ f2             (fun-self=(nex)
                      (fun-args=(lweld free arg1)
                        (fun-binds=(bal-insert slf1 newSelf bin1)
                          f1))))
  (@ [f3 stat3]     (Ifz free [f1 stat1] [f2 (analyse f2)]))
  (@ output         (codegen f3 stat3))
  (Seq refs1
    [output free]))

(defun (ingest-expr x)
  (ingest compile NIL x [bal-empty 0]))

;; compile-expr : Expr > Any
;;
;; =compile-expr= is an adaptor which supports the compilation of any
;; expression.  This works by just wrapping the expression in a dummy
;; function, compiling it, and then evaluating it.
;;
;; This approach allows us to support, for example, top-level let
;; expressions using the same LET machinery used for functions.
;;
;; If the result of loading an expression is a constant value, we
;; skip this process, which is just an optimization.

(defun (expr-is-value x)
  (Eq VAL (Hd x)))

(defun (make-dummy-function [[bin nex] bod])
  (FUN 0 "dummy" nex (lsing (Inc nex)) bin bod))

(defun (compile-dummy-function [[bin nex] bod])
  (.0
    (compile (Add 2 nex)
      (FUN 0 "dummy" nex (lsing (Inc nex)) bin bod))))

(defun (compile-expr inlined)
  (@ res[[bin n] bod[val]] (ingest-expr inlined))
  (If (expr-is-value bod)
    val
    (compile-dummy-function res 0)))

; Expr > Any
(defun (eval-expr x)
  (compile-expr (.res-exp (inline 0 NIL NIL x))))

(defun (compiles-to fun law)
  (planEq
    [law ~[]]
    (compile 0 fun)))

; Let-shaped expressions are only escaped when necessary.

assert(compiles-to
  (FUN 0 "hi" 0 ~[1 2] bal-empty (VAL (1 2 3)))
  (Law "hi" 2 [(1 2 3)]))

assert(compiles-to
  (FUN 0 "hi" 0 ~[1 2] bal-empty (VAL (1 2)))
  (Law "hi" 2 (1 2)))

assert(compiles-to
  (FUN 0 "hi" 0 ~[1 2] bal-empty (APP (VAL 0) (VAL (1 2 3))))
  (Law "hi" 2 [[0] (1 2 3)]))


;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (do-not-compile nex fn)
  Error("do-not-compile" nex fn))

(defun (test-ingest x)
  (ingest do-not-compile 0 x [bal-empty 0]))

(defun (fake-state binds)
  [(bal-from-pairs-list
     (lzip (lrange 0) (stream binds)))
   (Sz binds)])

(def Add2 (Add 2))

(tests
  ; constant
  ([[0 0] (VAL 0)]
   (test-ingest (K 0)))

  ; global
  ([[0 0] (VAL Inc)]
   (test-ingest (G (BIND "Inc" Inc (K Inc)))))

  ; marked constant
  ([[0 0] (VAL 0)]
   (test-ingest (M (K 0))))

  ; static application
  ([[0 0] (VAL (Add 1))]
   (test-ingest (A (K Add) (K 1))))

  ; let-bound static application (folded into a constant and then inlined
  ; into reference-site)
  ([[0 0] (VAL (Add 1))]
   (test-ingest (L (A (K Add) (K 1)) (V 0))))

  ; dynamic application
  ([ (fake-state []) (APP (VAL Inc) (VAL 1))]
   (test-ingest (A (K Inc) (K 1))))

  ; let-bound dynamic application
  ( [ (fake-state [(APP (VAL Inc) (VAL 1))])
      (VAR 0) ]
   (test-ingest (L (A (K Inc) (K 1)) (V 0))))

  ; letrec
  ( [ (fake-state
        [(APP (VAL Inc) (VAR 1))
         (APP (VAL Inc) (VAL 0))])
      (VAR 0)
    ]
    (test-ingest (R [ (A (K Inc) (V 1))
                      (A (K Inc) (K 0))
                    ]
                   (V 0)))
  )

  ( [[0 0] (VAL (Add 2))]
    (test-ingest
      (L (A (K Add) (K 2))
        (V 0))))
)

(tests
  ; constant
  0(compile-expr (K 0))

  ; dynamic application
  2(compile-expr (A (K Inc) (K 1)))

  ; constant application in a let
  ((Add 1)
   (compile-expr (L (A (K Add) (K 1)) (V 0))))

  ; dynamic application in a let
  (2
   (compile-expr (L (A (K Inc) (K 1)) (V 0))))

  ; compile Id
  ({Id a => a}
   (compile-expr
     (F 0 0 0 "Id" 1 (V 0))))

  ; compile Const
  ({Const a b => a}
   (compile-expr
     (F 0 0 0 "Const" 2 (V 1))))

  ; compile Apply
  ({Apply a b => (a b)}
   (compile-expr
     (F 0 0 0 "Apply" 2 (A (V 1) (V 0)))))

  ; compile Const3 (via lifting)
  (({0 a b => a} 3)
   (compile-expr
     (L (A (K Inc) (K 2))
       (F 0 0 0 0 1
        (V 2)))))

  ; Outside references to foldable constants are inlined instead of
  ; being lifted.
  ({0 b => Add2}
   (compile-expr
     (L (A (K Add) (K 2))
       (F 0 0 0 0 1
         (V 2)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (expr-type sexp[fn])
  ( If (Nat sexp)           0 ; reference
  ( If (Eq 1 (Hd sexp))     1 ; constant
  ( If (Eq fn "*")          2 ; mark
  ( If (Eq fn "lambda")     3 ; lambda
  ( If (Eq fn "pin-lambda") 4 ; pin-lambda
  ( If (Eq fn "let*")       5 ; let*
  ( If (Eq fn "let")        6 ; let
  ( If (Eq fn "letrec")     7 ; letrec
                            8)))))))))

(defun (sexp-is-apply sexp)
  (Nil (Hd sexp)))

(defun (sexp-is-symbol sexp)
  (Ge (Nat sexp) 1))

(defun (sexp-is-value sexp)
  (Eq (Hd sexp) 1))

(defun (is-valid-selfref-leaf x[v])
  (Or (sexp-is-symbol x)
    (And (sexp-is-value x)
      (IsNat v))))

(defun (is-valid-marked-selfref x[v])
  (And (sexp-is-apply x)
    (And (Eq 1 (Sz x))
      (is-valid-selfref-leaf v))))

(defun (is-valid-selfref x[x1 x2])
  (Ifz (sexp-is-apply x)
    (is-valid-selfref-leaf x)
    (And (Eq 2 (Sz x))
      (And (Eq "*" x1)
        (is-valid-marked-selfref x2)))))

(tests
  1(is-valid-selfref '(fdas))
  1(is-valid-selfref '(123))
  1(is-valid-selfref '("self"))
  1(is-valid-selfref '(*(self)))
  0(is-valid-selfref '((* self)))
  0(is-valid-selfref '(*((1))))
  0(is-valid-selfref '((1)))
)

(defun (validate-selfref x k)
  (Ifz (is-valid-selfref x)
    (Error ("bad-selfref" x))
    k))

(defun (apple xs)
  (Ifz xs (K 0)
    (lfoldl A (.0 xs) (ltail (stream xs)))))

(tests
  ((K 0) (apple []))
  ((K 1) (apple [(K 1)]))
  ((A (A (K 1) (K 2)) (K 3)) (apple [(K 1) (K 2) (K 3)])))

(defun (uncurry f [x y])
  (f x y))

(defun (PAT key fields)
  (Coup key fields))

(def .pat-key    Hd)
(def .pat-fields (Coup 0))

(deftype AST                     ; Expression Tree
  (REFER Nat)                    ;   local (or unresoled) reference by name/key.
  (VALUE Any)                    ;   constant value
  (GLOBAL Globe)                 ;   global reference
  (APPLY (Seq AST))              ;   function application
  (LETPAR (Row [Pat Expr]) Expr) ;   let
  (LETSEQ (Row [Pat Expr]) Expr) ;   let*
  (LETREC (Row [Pat Expr]) Expr) ;   letrec
  (MARK Expr)                    ;   Request Inline
  (LAM (Unpack AbsLambda)))      ;   nested lambda

(defstruct AbsLambda ABSLAM      ; Lambda Expression
  (abs-lam-pinned Bit)           ;   Should the function be pinned?
  (abs-lam-marked Bit)           ;   Should we attempt to inline this?
  (abs-lam-name   Nat)           ;   What PLAN Law metadata should be assigned?
  (abs-lam-self   Nat)           ;   Self-reference (name or key)
  (abs-lam-args   (Row Pat))     ;   Arguments
  (abs-lam-body   AST))          ;   The actual lambda expression body.


;;; Load Validated+Expanded S-Expression into AST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (load-reference go x)
  (REFER x))

(defun (load-constant go [v])
  (VALUE v))

; =load-pat= loads one of the following form-shapes:
;
;     fdsa
;     [a b] = (BRACED (a b))
;     fdsa[a b] = (fdsa (BRACED (a b)))

(defun (load-pat x)
  (@ hed (.0 x))
  (If (Nat x)
    x
    (If (Eq "BRACED" hed)
      (Coup 0   (map load-pat (.1 x)))
      (Coup hed (map load-pat (.1 (.1 x)))))))

(defun (load-selfref-inner mark x[x1])
  (If (sexp-is-apply x)
    (load-selfref-inner mark x1)
    (If (sexp-is-symbol x)
      [x x mark]
      [0 x1 mark])))

(defun (load-selfref x)
  (If (sexp-is-apply x)
    (load-selfref-inner YES (.1 x))
    (load-selfref-inner NO x)))

(defun (load-sig sig)
  (If (Eq "BRACED" (.0 sig))
    [[0 0 0] (map load-pat (.1 sig))]
    [(load-selfref (.0 sig)) (map load-pat (drop 1 sig))]))

(def abc ["a" "b" "c"])

(tests
  ( [[0 0 0] abc]     (load-sig '([a b c]))         )
  ( [["f" "f" 0] abc] (load-sig '((f a b c)))       )
  ( [[0 "f" 0] abc]   (load-sig '(("f" a b c)))     )
  ( [[0 "f" 1] abc]   (load-sig '(((* "f") a b c))) )
  ( [["f" "f" 1] abc] (load-sig '((*(f) a b c)))    )
  ( [[0 "f" 1] abc]   (load-sig '((*("f") a b c)))  ))

(defun (load-lambda-common pinned go sexp[_ sig body])
  (@ [[tag self mark] pats] (load-sig sig))
  (LAM pinned mark tag self pats (go body)))

(defun (load-lambda go sexp)
  (load-lambda-common NO go sexp))

(defun (load-pin-lambda go sexp)
  (load-lambda-common YES go sexp))

(defun (load-bind go sexp[v b])
  [(load-pat v) (go b)])

(defun (load-let-gen cnstr go sexp[hd binds body])
  (cnstr (map (load-bind go) binds) (go body)))

(defun (load-letpar go x) (load-let-gen LETPAR go x))
(defun (load-letseq go x) (load-let-gen LETSEQ go x))
(defun (load-letrec go x) (load-let-gen LETREC go x))

(defun (load-apply go src[hd])
  (Coup APPLY (map go src)))

(defun (load-mark go src[star x])
  (MARK (go x)))

(defun (load-expr x)
  ((Case (expr-type x)
    [ load-reference
      load-constant
      load-mark
      load-lambda
      load-pin-lambda
      load-letseq
      load-letpar
      load-letrec ]
   ) load-apply load-expr x))

(tests
  ( (MARK (REFER "x"))
    (load-expr '((* x))))

  ( (MARK (APPLY (REFER "f") (REFER "x")))
    (load-expr '(*(f x))))

  ( (LETREC [["xx" (REFER "yy")]
             ["yy" (REFER "xx")]]
      (REFER "xx"))
    (load-expr '((letrec ((xx yy) (yy xx))
                    xx))))

  ((LAM NO NO "f" "f" ["a" "b" "c"] (REFER "c"))
   (load-expr '((lambda (f a b c) c))))

  ((LETPAR [["n" (REFER "v")]] (REFER "b"))
   (load-expr '((let ((n v)) b))))

  ((LETPAR [["x" (VALUE 4)]
            ["y" (VALUE 5)]]
     (APPLY (REFER "x") (REFER "y")))
   (load-expr '((let ((x 4) (y 5)) (x y)))))

  ((LETSEQ [["x" (VALUE 4)]
            ["y" (VALUE 5)]]
     (APPLY (REFER "x") (REFER "y")))
   (load-expr '((let* ((x 4) (y 5)) (x y)))))

  ((LETREC [["x" (VALUE 4)]
            ["y" (VALUE 5)]]
     (APPLY (REFER "x") (REFER "y")))
   (load-expr '((letrec ((x 4) (y 5)) (x y)))))

  ((LETSEQ [[(0 "x" "y") (REFER "xy")]
            [("ab" "a" "b") (REFER "ab")]]
     (APPLY (REFER "x") (REFER "y")))
   (load-expr '((let* (([x y] xy) (ab[a b] ab)) (x y)))))

  ((LAM 0 0 0 0
     ["f" (0 "a" "b")]
     (APPLY (REFER "f") (REFER "a") (REFER "b")))
   (load-expr '((lambda [f [a b]] (f a b)))))

  ((LAM 0 0 0 0
     ["f" ("ab" "a" "b")]
     (APPLY (REFER "f") (REFER "a") (REFER "b")))
   (load-expr '((lambda [f ab[a b]] (f a b))))))

  ((LAM 1 0 0 0
     ["f" ("ab" "a" "b")]
     (APPLY (REFER "f") (REFER "a") (REFER "b")))
   (load-expr '((pin-lambda [f ab[a b]] (f a b)))))


;;; Validation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (is-braced-pat is-valid-pat pat[br fs])
  (And (sexp-is-apply pat)
    (And (Eq (Sz pat 2))
      (And (Eq "BRACED" br)
        (all is-valid-pat fs)))))

(defun (is-aliased-pat is-valid-pat pat[nm fs])
  (And (sexp-is-apply pat)
    (And (Eq (Sz pat 2))
      (And (sexp-is-symbol nm)
        (is-braced-pat is-valid-pat fs)))))

(defun (is-valid-pat pat)
  (Or (sexp-is-symbol pat)
    (Or (is-braced-pat is-valid-pat pat)
      (is-aliased-pat is-valid-pat pat))))

(tests
  1(is-valid-pat '(fdsa))
  1(is-valid-pat '([]))
  1(is-valid-pat '([x]))
  1(is-valid-pat '([x y z]))
  1(is-valid-pat '(a[x y z]))
  0(is-valid-pat '((fdsa fdsa)))
  0(is-valid-pat '({fdsa})))

(defun (validate-arglist args k)
  (Ifz (And (sexp-is-apply args)
         (And (Sz args)
           (all is-valid-pat args)))
    Error("bad-arglist" args)
    k))

(defun (validate-signature sig k)
  (Ifz (And (sexp-is-apply sig) (Ge (Sz sig) 2))
    Error("bad-lambda-signature" sig)
    (If (Eq "BRACED" (.0 sig))
      (If (Ne 2 (Sz sig))
        Error("bad-lambda-signature")
        (validate-arglist (.1 sig)
          k))
      (validate-selfref (.0 sig)
        (validate-arglist (drop 1 sig)
          k)))))

(defun (validate-lambda sexp[_ sig body] k)
  (If (Ne 3 (Sz sexp))
    Error("bad-lambda" sexp)
    (validate-signature sig
      k)))

(defun (is-okay-bind bind)
  (And (sexp-is-apply bind)
   (And (Eq 2 (Sz bind))
     (And (sexp-is-symbol (.0 bind))))))

(defun (valid-let-binds binds)
  (And (sexp-is-apply binds)
    (all is-okay-bind binds)))

(defun (validate-binds binds k)
  (Ifz (valid-let-binds binds)
    Error("bad-let" binds)
    k))

(defun (validate-let sexp[_ binds b] k)
  (Ifz (Eq 3 (Sz sexp)) Error("bad-let" sexp)
    (validate-binds binds
      k)))


;;; Macro Expansion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (expo-keep go e0 x)
  [e0 x])

(defun (expo-lambda go e0 x[lam sig b0])
  (@ [e1 b1] (go e0 b0))
  (validate-lambda x
    Force[e1 [lam sig b1]]))

(defun (expo-bind go e0 bind[v b0])
  (@ [e1 b1] (go e0 b0))
  Force[e1 [v b1]])

(defun (expo-let go e0 x[let bs0 b0])
  (@ [e1 bs1] (traverse e0 (expo-bind go) bs0))
  (@ [e2 b1] (go e1 b0))
  (validate-let x
    Force[e2 [let bs1 b1]]))

(defun (expo-apply go e0 x[head])
  (@ mac (getmacro e0 head))
  (If mac
    Force(uncurry go (mac e0 x))
    Force(traverse e0 go x)))

(defun (expo-mark go e0 [star x0])
  (@ [e1 x1] (go e0 x0))
  [e1 [star x1]])

(defun (expo-expr e x)
  ( (Case (expr-type x)
          [expo-keep expo-keep
           expo-mark
           expo-lambda expo-lambda
           expo-let expo-let expo-let]
          expo-apply)
    expo-expr e x))

(tests
  ('((* (lambda [x]
          (letrec ((a (0 x y))
                   (b ()))
            (0 (0 a) (0 b))))))
   (.1
     (expo-expr ["BRACED" BRACED 1 0 0]
       '((*
           (lambda [x]
            (letrec ((a [x y]) (b ()))
              [[a] [b]]))))))))


;;; Name Resolution ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (zolv-value go n e0 x)
  [n x])

(defun (make-global-binder [k v m l r e])
  (GLOBE k v (Or e (1 v))))

(defun (zolv-refer go[globals] n e0 [nm])
  (@ loc (bal-search nm e0))
  (@ glo (getenv nm globals))
  Force[n
    (If loc
      (REFER (.0 loc))
      (If glo
        (GLOBAL (make-global-binder glo))
        Error("unbound" nm)))])

(defun (zolv-global go n e0 x) ; globals come *from* name resolution
  Error("zolv-global" "this should never happen"))

(defun (bind-table-ins nm key tab)
  (If (Or (Nil nm) (Eq "_" nm))
    tab
    (If (bal-has nm tab)
      Error("duplicate-name" nm)
      (bal-insert nm key tab))))

; zolv-pat : Pat Name -> State (Bal Str Key) Pat Key

(defun (zolv-pat [t0 n0] pat)
  (@ t1            (bind-table-ins (.pat-key pat) n0 t0))
  (@ n1            (Inc n0))
  (@ [[t2 n2] fs1] (traverse [t1 n1] zolv-pat pat))
  Force[[t2 n2] (PAT n0 fs1)])

(defun (try-zolv-pat next table pat)
  (@ [[t n] p] (zolv-pat [(bal-from-pairs-row table) next] pat))
  [n (bal-save t) p])

(tests
  ( [8 ["f" 7] 7]
    (try-zolv-pat 7 []
      "f"))

  ( [11 ["f" 7 "x" 9 "y" 10]
      (8 9 10)]
    (try-zolv-pat 8 [["f" 7]]
      (0 "x" "y")))
)

; =lcons-name= ignores patterns with no names and patterns named _.

(defun (lcons-name nm names)
  (If (Or (Nil nm) (Eq "_" nm))
    names
    [nm names]))

(defun (pat-names-list pat)
  (lcons-name (.pat-key pat) (lcatmap pat-names-list (stream pat))))

(tests
  (~["xx"]
   (pat-names-list "xx"))
  (~["xx" "yy" "zz"]
   (pat-names-list ("xx" "yy" 0 "zz")))
  (~["xx" "yy" "zz"]
   (pat-names-list ("xx"
                     ("yy"
                        (0 "zz"))))))

(defun (bal-set-insert x t)
  (bal-insert x 0 t))

(defun (nat-row-is-unique nats)
  (@ tab (lfoldr bal-set-insert bal-empty (stream nats)))
  (Eq (Hd tab) (Sz nats)))

(tests
  1(nat-row-is-unique [3 4 5])
  0(nat-row-is-unique [3 4 5 4]))

(defun (bal-union-left-biased x y)
  (lfoldr (uncurry bal-insert) y (bal-to-pairs-list x)))

(tests
  ( ["xx" 3 "yy" 4 "zz" 5]
    (bal-save
      (bal-union-left-biased
        (bal-from-pairs-row [["xx" 3] ["zz" 5]])
        (bal-from-pairs-row [["yy" 4] ["zz" 6]])))))

(defun (zolv-lambda go n0 e0 [pin mark tag self0 args0 body0])
  (@ o1[[t n1] pat1] (zolv-pat [0 n0] (PAT self0 args0)))
  (@ self1           (.pat-key pat1))
  (@ args1           (.pat-fields pat1))
  (@ [n2 body1]
    (go (bal-union-left-biased t e0) n1 body0))
  [n2 (LAM pin mark tag self1 args1 body1)])

(tests
  ( [12 (LAM 0 0 "hi" 7
          [8 9 (10 11)]
          (REFER "xx"))]
    (zolv-lambda {e n x -> [n x]} 7 0
      (LAM 0 0 "hi" "hi"
        ["_" "_" ("aa" "bb")]
        (REFER "xx")))))

(defun (zolv-mark go n e [x])
  (@ [n1 x1] (go e n x))
  [n1 (MARK x1)])

(defun (zolv-letseq-body go binds n0 env body0)
  (@ [n1 body1] (go env n0 body0))
  Force[n1 (LETSEQ binds body1)])

(defun (zolv-letseq-loop go n0 env binds list[[pat[nm fields] v0] more] body)
  (@ [n1 v1]     (go env n0 v0))
  (@ [[t n2] p1] (zolv-pat [0 n1] pat))
  (Ifz list
    (zolv-letseq-body go binds n0 env body)
    (zolv-letseq-loop go n2
      (bal-union-left-biased t env)
      (binds [p1 v1])
      more
      body)))

(defun (zolv-letseq go n e [binds body])
  (zolv-letseq-loop go n e [] (stream binds) body))

(defun (zolv-patseq n0 pats)
  (@ fake0 (PAT 0 pats))
  (@ [[t n1] fake1] (zolv-pat [0 (Dec n0)] fake0))
  [t n1 (.pat-fields fake1)])

(defun (zolv-letpar go n0 e [binds body0])
  (@ [n1 vals1]   (traverse n0 (go e) (map .1 binds)))
  (@ [t n2 pats1] (zolv-patseq n1 (map .0 binds)))
  (@ [n3 body1]   (go (bal-union-left-biased t e) n2 body0))
  [n3 (LETPAR (zip pats1 vals1) body1)])

(defun (zolv-letrec go n0 e0 [binds body0])
  (@ [t n1 pats1] (zolv-patseq n0 (map .0 binds)))
  (@ e1           (bal-union-left-biased t e0))
  (@ [n2 vals1]   (traverse n1 (go e1) (map .1 binds)))
  (@ [n3 body1]   (go e1 n2 body0))
  [n3 (LETREC (zip pats1 vals1) body1)])

(defun (zolv-apply go n e xs)
  (@ [n1 xs1] (traverse n (go e) xs))
  [n1 (Coup APPLY xs1)])

(defun (zolv-expr globals e n x)
  ((union-switch x
     REFER(zolv-refer)
     VALUE(zolv-value)
     GLOBAL(zolv-global)
     APPLY(zolv-apply)
     LETPAR(zolv-letpar)
     LETSEQ(zolv-letseq)
     LETREC(zolv-letrec)
     MARK(zolv-mark)
     LAM(zolv-lambda)
   ) (zolv-expr globals) n e x))

(defun (resolve-expr globals x)
  (.1 (zolv-expr globals 0 1 x)))

(defmacro (get-current-environment e x)
  [e (1 e)])

(tests
  ( [1 (VALUE 3)]
    (zolv-expr 0 0 1 (VALUE 3)))

  ( (17 ("unbound" "xx"))
    (xtry (zolv-expr 0 0 1) (REFER "xx")))

  ( [2 (REFER 1)]
    (zolv-expr 0 (bal-sing "xx" 1) 2 (REFER "xx")))

  ( [2 (GLOBAL (GLOBE "Add" Add (1 Add)))]
    (zolv-expr (get-current-environment) 0 2 (REFER "Add")) )

  ( [2 (LETSEQ [] (REFER 1))] ; empty let
    (zolv-expr 0 (bal-sing "xx" 1) 2
      (LETSEQ []
        (REFER "xx"))))

  ( [3 (LETSEQ [[2 (REFER 1)]] (REFER 2))]
    (zolv-expr 0 (bal-sing "xx" 1) 2
      (LETSEQ [["yy" (REFER "xx")]]
         (REFER "yy"))))

  ( [5 (LETSEQ [[(2 3 4) (REFER 1)]] (REFER 4))]
    (zolv-expr 0 (bal-sing "xx" 1) 2
      (LETSEQ [[("yy" "y1" "y2") (REFER "xx")]]
         (REFER "y2"))))

  ( [5 (LETPAR [[(2 3 4) (REFER 1)]] (REFER 4))]
    (zolv-expr 0 (bal-sing "xx" 1) 2
      (LETPAR [[("xx" "x1" "x2") (REFER "xx")]]
         (REFER "x2"))))

  ; Later binds don't see earlier ones.
  ( [4 (LETPAR [[2 (VALUE "hi")]
                [3 (REFER 1)]]
         (REFER 3))]
    (zolv-expr 0 (bal-sing "xx" 1) 2
      (LETPAR [["xx" (VALUE "hi")]
               ["yy" (REFER "xx")]]
         (REFER "yy"))))

  ; Recursive binds are recursive
  ( [5 (LETREC [[(2 3) (APPLY (VALUE "hi") (REFER 4))]
                [4     (APPLY (REFER 2) (REFER 4))]]
         (APPLY (REFER 4) (REFER 3)))]
    (zolv-expr 0 (bal-sing "xx" 1) 2
      (LETREC [[("xx" "x1") (APPLY (VALUE "hi") (REFER "yy"))]
               ["yy"        (APPLY (REFER "xx") (REFER "yy"))]]
         (APPLY (REFER "yy") (REFER "x1")))))

  ( [2 (APPLY (REFER 1) (MARK (REFER 1)) (REFER 1))]
    (zolv-expr 0 (bal-sing "xx" 1) 2
      (APPLY (REFER "xx") (MARK (REFER "xx")) (REFER "xx"))))

  ( [12 (LAM 0 0 "hi" 7
          [8 9 (10 11)]
          (APPLY (REFER 1) (REFER 10) (REFER 11)))]
    (zolv-expr 0 (bal-sing "xx" 1) 7
      (LAM 0 0 "hi" "hi"
        ["_" "_" ("aa" "bb")]
        (APPLY (REFER "xx") (REFER "aa") (REFER "bb"))))))

(defun (destroy-keep go x)
  x)

(defun (destroy-seq go xs)
  (Coup (Hd xs) (map go xs)))

(defun (destroy-field destroy-bind key ix pat)
  (destroy-bind [pat (APPLY (VALUE (mkix ix)) (REFER key))]))

(defun (lcatmapi f xs)
  (lcat (lmapi f xs)))

(defun (destroy-fields destroy-bind k fs)
  (lcatmapi (destroy-field destroy-bind k)
    (stream fs)))

(defun (destroy-bind bind[pat val])
  (@ k  (.pat-key pat))
  (@ fs (.pat-fields pat))
  (Ifz fs (lsing bind)
    (lcons [k val]
      (destroy-fields destroy-bind k fs))))

(tests
  ( ~[[3 (REFER 1)]
      [4 (APPLY (VALUE Ix0) (REFER 3))]
      [5 (APPLY (VALUE Ix1) (REFER 3))]]
    (destroy-bind [(3 4 5) (REFER 1)])))

(defun (destroy-binds binds)
  (array (lcatmap destroy-bind (stream binds))))

(defun (destroy-let go x[binds body])
  (Hd x (destroy-binds binds) (go body)))

(def pat-has-fields IsApp)

(defun (destroy-arg pat)
  (destroy-fields destroy-bind (.pat-key pat) (.pat-fields pat)))

(defun (destroy-body pats body)
  (Ifz pats body
    (LETSEQ (array (lcatmap destroy-arg pats))
      body)))

(defun (destroy-lam go [pin mark tag self args body])
  (@ pargs (lfilter pat-has-fields (stream args)))
  (LAM pin mark tag self
    (map .pat-key args)
    (destroy-body pargs
      (go body))))

(defun (destroy-expr x)
  ((union-switch x
     REFER(destroy-keep)
     VALUE(destroy-keep)
     GLOBAL(destroy-keep)
     APPLY(destroy-seq)
     LETPAR(destroy-let)
     LETSEQ(destroy-let)
     LETREC(destroy-let)
     MARK(destroy-seq)
     LAM(destroy-lam)
   ) destroy-expr x))

(tests
  ((APPLY (VALUE 1) (REFER 1))
   (destroy-expr (APPLY (VALUE 1) (REFER 1))))

  ((LETSEQ [[1 (VALUE 1)]
            [2 (APPLY (VALUE Ix0) (REFER 1))]
            [3 (APPLY (VALUE Ix1) (REFER 1))]]
     (REFER 2))
   (destroy-expr
     (LETSEQ [[(1 2 3) (VALUE 1)]]
       (REFER 2))))

  ((LETREC [[1 (VALUE 1)]
            [2 (APPLY (VALUE Ix0) (REFER 1))]
            [3 (APPLY (VALUE Ix0) (REFER 2))]]
     (REFER 2))
   (destroy-expr
     (LETREC [[(1 (2 3)) (VALUE 1)]]
       (REFER 2))))

  ((LAM 0 0 0 0
     [2 3]
     (REFER 2))
   (destroy-expr
     (LAM 0 0 0 0
       [2 3]
       (REFER 2))))

  ((LAM 0 0 0 0
     [2]
     (LETSEQ [[3 (APPLY (VALUE Ix0) (REFER 2))]
              [4 (APPLY (VALUE Ix1) (REFER 2))]]
       (REFER 2)))
   (destroy-expr
     (LAM 0 0 0 0
       [(2 3 4)]
       (REFER 2)))))


;;; Does An AST Contain Any Reference To This Key? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (walk-leaf go x)
  (lsing x))

(defun (walk-seq go x)
  [x (lcatmap go (stream x))])

(defun (walk-let go x[binds body])
  (lcons x
    (lcatmap go
      (lsnoc
        (lmap .1 (stream binds))
        body))))

(defun (walk-lam go x[pin mark tag self args body])
  [x (go body)])

(defun (ast-walk x)
  ((union-switch x
     REFER(walk-leaf)
     VALUE(walk-leaf)
     GLOBAL(walk-leaf)
     APPLY(walk-seq)
     LETPAR(walk-let)
     LETSEQ(walk-let)
     LETREC(walk-let)
     MARK(walk-seq)
     LAM(walk-lam)
   ) ast-walk x))

(defun (ast-has-reference-to key x)
  (lany (planEq (REFER key))
    (ast-walk x)))

(tests
  ( ~[ (LETREC [[8 (REFER 3)]
                [9 (REFER 4)]]
         (VALUE 4))
       (REFER 3)
       (REFER 4)
       (VALUE 4)
     ]
    (ast-walk
      (LETREC [[8 (REFER 3)]
               [9 (REFER 4)]]
        (VALUE 4))))

  ( ~[ (LAM 0 0 0 0 [5]
         (APPLY (REFER 5) (VALUE 4)))
       (APPLY (REFER 5) (VALUE 4))
       (REFER 5)
       (VALUE 4)
     ]
    (ast-walk
      (LAM 0 0 0 0 [5]
        (APPLY (REFER 5) (VALUE 4)))))

  1(ast-has-reference-to 3
     (LETREC [[0 (REFER 3)]]
       (VALUE 4))))

;;; Translate to De-Bruijn ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Alright, the main thing that we need here is to translate unique keys
; into debruijn indexes.  The dumbest way to do this is to just maintain
; a list of keys and find the index in that list.

(defun (lower-value go e [x])
  (K x))

(defun (lower-global go e [globe])
  (G globe))

(defun (lower-refer go e [key])
  (V (lfind-known key e)))

(defun (lower-mark go e [x])
  (M (go e x)))

(defun (lower-apply go e xs)
  (apple (map (go e) xs)))

(defun (bind-pat-key [pat x])
  (.pat-key pat))

(defun (lower-let-loop go e body list[bind[pat val] more])
  (Ifz list (go e body)
    (L (go e val)
      (lower-let-loop go [(.pat-key pat) e] body more))))

(defun (lower-let go e [binds body])
  (lower-let-loop go e body (stream binds)))

(defun (lower-letrec go e0 [binds body])
  (@ e1 (lweld (lmap bind-pat-key (stream binds)) e0))
  (R (map (compose (go e1) .1) binds)
     (go e1 body)))

(defun (lower-lambda go e0 [pin mark tag self args body])
  (F pin mark
    (ast-has-reference-to self body)
    tag
    (Sz args)
    (go (lweld (stream-rev (map .pat-key args))
               [self e0])
      body)))

(defun (lower-expr e x)
  (Force
    ((union-switch x
       REFER(lower-refer)
       VALUE(lower-value)
       GLOBAL(lower-global)
       APPLY(lower-apply)
       LETPAR(lower-let)
       LETSEQ(lower-let)
       LETREC(lower-letrec)
       MARK(lower-mark)
       LAM(lower-lambda)
     ) lower-expr e x)))

(defun (try-expr x)
  (lower-expr NIL
    (destroy-expr
      (resolve-expr (get-current-environment)
        (load-expr
          (.1
            (expo-expr 0 x)))))))

(defun (traced msg x)
  (xtrace (msg x)
    x))

(defun (try-full x)
  (eval-expr (try-expr x)))

(tests
  ((K 9)
   (try-expr '((9))))

  ((A (A (K 3) (K 4)) (K 5))
   (try-expr '((3 4 5))))

  ((K 0)
   (try-expr '(())))

  ((L (K 3) (V 0))
   (try-expr '((let ((a 3)) a))))

  ((L (K 2) (L (K 3) (L (V 0) (A (V 1) (V 0)))))
   (try-expr '((let ((x 2))
                 (let* ((x 3) (y x))
                   (x y))))))

  ((L (K 2) (L (K 3) (L (V 1) (A (V 1) (V 0)))))
   (try-expr '((let ((x 2))
                 (let ((x 3) (y x))
                   (x y))))))

  ((F 0 0 0 0 2 (A (V 1) (V 0)))
   (try-expr '((lambda [x y] (x y)))))

  ((A (G (GLOBE "Inc" Inc (1 Inc))) (K 3))
   (try-expr '((Inc 3))))

  ((F 0 0 0 0 2
     (L (A (K Ix0) (V 0))
       (L (A (K Ix1) (V 1))
         (A (A (V 3) (V 1)) (V 0)))))
   (try-expr '((lambda [f [x y]] (f x y)))))

  ({uncurry f xy => (f (Ix0 xy) (Ix1 xy))}
   (try-full '((lambda (uncurry f [x y])
                  (f x y)))))

  (5
   (try-full '((Add 2 3))))

  ((F 1 0 0 0 1
    (R [ (A (A (K 0) (K 3)) (V 1))
         (A (A (K 0) (K 4)) (V 0))
       ]
      (V 0)))
   (try-expr '(
     (pin-lambda [aa]
       (letrec ((xx (0 3 yy)) (yy (0 4 xx)))
         xx)))))

  ((F 0 0 0 0 2
    (R [ (A (A (K 0) (V 3)) (V 1))
         (A (A (K 0) (V 2)) (V 0))
       ]
      (A (A (K 0) (V 0)) (V 1))))
   (try-expr '(
     (lambda [aa bb]
       (letrec ((xx (0 aa yy)) (yy (0 bb xx)))
         (0 xx yy))))))

  ((F 0 1 1 "spin" 1
    (A (V 1) (V 0)))
   (try-expr '(
     (lambda (*(spin) aa)
       (spin aa)))))

  ((F 0 1 1 "spin" 1
    (A (V 1) (V 0)))
   (try-expr '(
     (lambda (*(spin) aa)
       (spin aa)))))

  ((MkLaw 0 2
     (1 [[[0] 1] 4]
     (1 [[[0] 2] 3]
     [[[0] 3] 4])))
   (try-full '(
     (lambda [aa bb]
       (letrec ((xx (0 aa yy)) (yy (0 bb xx)))
         (0 xx yy))))))

  (~[3 4 3]
   (try-full '(
     (ltake 3
       (Ix0
         ((pin-lambda [aa bb]
             (letrec ((xx (0 aa yy)) (yy (0 bb xx)))
               (0 xx yy)))
           3 4))))))
)

; {x y -> x}          (lambda [x y] x)
; {x y --> x}         (pin-lambda [x y] x)
; {Const x y ==> x}   (pin-lambda (Const x y) x)
; {*(Const) x y => x} ; marked variant
; {*(0) x y => x}

(defun (filter f row)
  (array (lfilter f (stream row))))

(defun (is-curled-arrow x)
  (lhas x ~["->" "-->" "=>" "==>"]))

(defun (takeWhile f row)
  (array (ltakeWhile f (stream row))))

(defun (arrow-type arrow)
  (If (Eq "->" arrow) 0
    (If (Eq "-->" arrow) 1
      (If (Eq "=>" arrow) 2
        3))))

(defun (wisp++ e0 x0)
  (@ [e1 x1] (expo-expr e0 x0))
  [e1
    (1
      (eval-expr
        (lower-expr NIL
          (destroy-expr
            (resolve-expr e0
              (load-expr x1))))))])

(macro lambda     wisp++)
(macro pin-lambda wisp++)
(macro let        wisp++)
(macro let*       wisp++)
(macro letrec     wisp++)

(tests
  ((lambda (id x) x)
   (MkLaw "id" 1 1))
  ((lambda [x y] x)
   (MkLaw 0 2 1)))

(defmacro (CURLED env x[_ ex])
  (@ arrows[arrow] (filter is-curled-arrow ex))
  (@ before        (takeWhile (compose Nil is-curled-arrow) ex))
  (@ after[body]   (drop (Inc (Sz before)) ex))
  [env
    (Ifz (And (Eq 2 (Sz x)) (Eq 1 (Sz arrows)))
      (Error ("bad-lambda" x))
      (Ifz (And (Sz before) (Eq 1 (Sz after)))
        (Error ("bad-lambda" x))
        (Case4 (arrow-type arrow)
          ["lambda" (cons (1 0) before) body]     ; ->
          ["pin-lambda" (cons (1 0) before) body] ; -->
          ["lambda" before body]                  ; =>
          ["pin-lambda" before body])))])         ; ==>

(tests
  ({foo bar -> foo}    (lambda [foo bar] foo))
  ({foo bar --> bar}   (pin-lambda [foo bar] bar))
  ({f x y => (f x y)}  (lambda (f x y) (f x y)))
  ({f x y ==> (f x y)} (pin-lambda (f x y) (f x y))))

(defmacro (SILLY_MACRO e [_ x])
  [e x])

(def foo "foo")

(tests

  ; basic lambdas
  ( {0 a b => a}
    (lambda (0 a b) a) )

  ; automatic lambda lifting
  ({a b -> ({a c d -> a} a)}
   {a b -> {c d -> a}})

  ; recursive lambda
  ( (MkLaw "eat" 1 0)
    (lambda (eat x) eat))

  ; simple let
  ( 3
    (let ((x 3))
      x))

  ; trivial-let and lambda
  ( {a -> ({a b -> a} a)}
    (lambda (0 x)
      (let ((xx x))
        (lambda (0 y)
          xx))))

  ; empty application
  ( {a -> 0}
    (lambda (0 x)
      ()))

  ; no-argument application
  ( {0 a => a}
    (lambda (0 x)
      (x)))

  ; applications
  ( {0 x => (0 x x)}
    (lambda (0 x)
      (0 x x)))

  ; macros
  (1 SILLY_MACRO(1))
  ( {0 x => (3 x)}
    (lambda (0 x)
      (3 SILLY_MACRO(x))))

  ; globals
  ( {0 x => foo}
    (lambda (0 x)
      foo))

  ; add-2
  ( {0 x => (Add2 x)}
    (lambda (0 x) (Add 2 x)))

  ; empty-let
  3(let* () 3)

  ; solo-let
  3(let* ((x 3)) x)

  ; chained-lets
  3(let* ((x 3) (y x)) y)

  ; shaddowed-lets
  4(let* ((x 3) (x 4)) x)

  ; empty-let
  3(let () 3)

  ; solo-let
  3(let ((x 3)) x)

  ; parallel-lets
  ( (4 3)
    (let ((x 3))
      (let ((x 4) (y x))
        (x y))))
)

(tests
  ( (lambda (0 a b)
      (letrec ((x [a y])
               (y [b x]))
        x))
    (Law 0 2
      (1 [[[0] 1] [[[0] 2] 3]]
        3))
  )

  ( ~[3 4 3 4 3]
    (ltake 5
      (letrec ((x [3 y])
               (y [4 x]))
        x))
  )
)

(def foo
  (lambda (0 x)
    (let* ((xx (x x))
           (xxxx (xx xx)))
      (lambda (0 y)
        (0 x xx xxxx y)))))

(def bar
  (lambda (0 x)
    (let ((xx (x x)))
      (let ((xxxx (xx xx)))
        ( (lambda (0 x xx xxxx y)
            (0 x xx xxxx y))
          x xx xxxx)))))

(tests
  (foo bar)
  ( ["aa" ("aa" "aa") (("aa" "aa") ("aa" "aa")) "bb"]
    (foo "aa" "bb")
  )
)

(def interactive 0)

(defun (__PRINT x)
  (xtrace
    (If (Eq 1 (Hd x)) (.0 x) x)
    0))

(defun (newreplstep e0 x)
  (@ __PRINT       (.1 (getenv "__PRINT" e0)))
  (@ res[[e1 out]] (xtry (wisp++ e0) x))
  (Ifz (Hd res)
    (Seq (__PRINT out)
      [e1])
    (Seq (__PRINT (1 ("ERROR" res)))
      (If interactive [e0] 1))))

(def newrepl
  (pin-lambda (newrepl e0 cs0)
    (let ((cs1 (skipSpace cs0)))
      (And cs1
        (read cs1
          {x cs2 ->
            (let ((out[e1] (newreplstep e0 x)))
              (If (Nat out) out
                (newrepl e1 cs2)))})))))

(def __REPL newrepl)

(defun (putenv-with-expr key val mac expr x)
  (@ k (Ix0 x))
  (@ v (Ix1 x))
  (@ m (Ix2 x))
  (@ l (Ix3 x))
  (@ r (Ix4 x))
  (Ifz x [key val mac 0 0 expr]
    (Case3 (Cmp key k)
      [k v m (putenv-with-expr key val mac expr l) r]
      [key val mac l r expr]
      [k v m l (putenv-with-expr key val mac expr r)])))

(defun (run-macro f e x)
  (f e x))

(defun (define-value e0 nam x)
  (@ [e1 x1] (expo-expr e0 x))
  (@ expr    (lower-expr NIL (destroy-expr (resolve-expr e1 (load-expr x1)))))
  (@ val     (eval-expr expr))
  [(putenv-with-expr nam val 0 expr e1)
   (1 0)])

(defun (define-function is-macro e0 x0[_ sig body])
  (@ [e1 x1] (expo-expr e0 ["pin-lambda" sig body]))
  (@ ast     (load-expr x1))
  (@ exp     (lower-expr NIL (destroy-expr (resolve-expr e1 ast))))
  (@ val     (eval-expr exp))
  (@ nam     (.lam-name ast))
  (If (Or (Ne 3 (Sz x0)) (Nil nam))
    (Error ("bad-defun" x0 (Sz x0) #(nam)))
    (DeepSeq [e1 x1 ast exp val]
      [ (putenv-with-expr nam val is-macro exp e1)
        (1 0) ])))

(defmacro (define e x[_ sig body])
  (If (Ne 3 (Sz x))
    Error("bad-define" x)
    (If (sexp-is-symbol sig)
      (define-value e sig body)
      (define-function 0 e x))))

(defmacro (defun e x)
  (define-function 0 e x))

(defmacro (defmacro e x)
  (define-function 1 e x))

(defmacro (get-bind-expr e x[_ nm])
  (let ((res[k v m l r ex] (getenv nm e)))
    (Ifz res
      (Error ("undefined" nm))
      [e (1 ex)])))

(defun (Identity a) a)
(define (*(Constant) a b) a)

(define (silly x)
  (Constant x x))

(tests
  (3 (Identity (Constant 3 4)))

  ((F 1 1 0 "Constant" 2 (V 1))
   (get-bind-expr Constant))

  ((MkLaw "silly" 1 1)
   (Unpin silly))) ; inlining was a success!


;;; Bigger-Picture TODOs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: Load '__interactive on each error, so that we can dynamically
; change whether or not errors are tolerated.
;
; TODO: Finish writing the seed serializer.
;
;     Just use the old seed format, instead of writting a new serializer
;     for the new format.
;
;     TODO: consider just using the old format in the runtime, as the
;     new format is bigger in practice, and we are loading each thing
;     pairwise anyways.
;
; TODO: Write a seed loader
;
; TODO: Support bang patterns.
