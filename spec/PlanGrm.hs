-- Copyright (c) 2026 xoCore Technologies
-- SPDX-License-Identifier: MIT
-- See LICENSE for full terms.
{-
    PlanGrm Optimizer
    -----------------

    PlanGrm is a basic STG-like optimization system for a compiler for
    a lazy, functional system named PLAN.

    The front-end produces code which is correct, but which is
    maximally indirect.  Every expression is encoded as a thunk, and
    all allocations happen unconditionally.

    The front-end output already contains stricness information, since
    every strict argument to a known function contains an explicit
    FORCE, but this strictness information has not yet been used to
    simplify code.

    This is operationally close to a graph-reduction machine, not a
    fast evaluator.  Basically, the input to this optimization just
    allocates a fine-grained graph of thunks and then relies on the
    graph reduction machinery to handle everything.

    But that is very expensive.  Instead, we want to collapse graph
    rreduction into direct execution wherever laziness is not observably
    required.

    This optimizer progressively collapses explicit graph reduction into
    direct execution by eliminating unnecessary laziness, shortening the
    lifetime of pure bindings, and rewriting forced thunks into eager
    control flow. Because all laziness and forcing are explicit in the IR
    and values do not escape, these transformations preserve semantics
    while producing flatter, more eager code with fewer allocations,
    fewer indirections, and more opportunities for inlining.


    IR Design
    ---------

    Unlike Haskell, PLAN is dynamically typed and doesn't have pattern
    matching as a primitive.  So, instead of the usual `CASE` primitive
    of STG, we have two separate operations `FORCE` and `BR` (switch on
    number), and pattern matching is implemented using a combination
    of these.

    Also unlike Haskell, this PLAN optimizer recives a single
    super-combinator, so nested lambdas have already been optimized away
    by the front-end.

    The front-end will produce supercombinators that encode all bindings
    as a single huge LETREC.  But an earlier graph-oriented ingestion
    pass takes that LETREC, and splits it up into a lot of tiny little
    LETs or absolutely minimal LETRECs.

    Once the ingestion pass has finished producing somewhat reasonable
    code, it will convert the supercombinator into a very dump PlanGrm
    "Proc", where every single sub-expression is linearly bound as a
    single thunk (or partial application).

    All forcing is explicit, and all known calls are compiled into
    explicit forces of strict arguments and then a call into a fastpath of
    that function (which assumes that strict arguments are always forced).

    For example, a function call (foo x y) will be initially translated
    into a statement like:

        a = delay {
            x' <- force x
            y' <- force y
            fastcall (foo x' y')
        }

    The result is that all evaluations are available as explicit force
    statements.

    It is the job of this pass to rearrange this tangle of tiny thunks
    into much fewer big thunks, which do as much evaluation as possible
    directly, instead of relying on the graph reduction engine to
    untangle everything.

    Note that PROC is a local concept (similar to a join point in STG)
    which is produced by compiler tranformations only.  nested procs
    never escape, are never passed to arguments, cannot recur (RECUR
    always calls into the top-level proc), and are only ever called once
    (though they can have multiple different callsites if used from
    differenent branches).

    Core IR invariants
    ------------------

    - All bindings use globally unique keys.
    - No shadowing is possible.
    - No forward references are allowed.
    - All evaluation is explicit via FORCE.
    - All laziness is explicit via DELAY.
    - FORCE always produces a value in WHNF.
    - Partial applications (CLOSE) are always in WHNF.
    - PROC represents eager control flow.
    - DELAY allocates a thunk and performs no evaluation.
    - Procs do not escape (not stored, returned, or passed).
    - Thunks are entered only via FORCE.
    - Blocks end in exactly one terminator (RET or BR).
    - Code after a terminator is dead (never do this)
    - Known calls use fast paths with already-forced strict args.
    - SLOW must only be used with already-forced functions.
    - Join points are represented using procs.
    - Tailness is inferred, not represented explicitly.
    - Analyses may be conservative.
    - The IR is trusted (not user-facing).
-}

{-# OPTIONS_GHC -Wall -Wno-orphans -Wno-unused-imports #-}
{-# LANGUAGE LambdaCase, BlockArguments, ViewPatterns #-}

module PlanGrm where

import           Control.Monad.State
import           Data.Foldable
import           Data.Functor
import           Plan                hiding (op)
import           Prelude

import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Set            as S

import           Control.DeepSeq     (force)
import           Control.Monad       (guard, when, unless, zipWithM_)
import           Data.Maybe          (mapMaybe, fromMaybe)
import           Debug.Trace         (trace, traceM, traceShow)
import           System.IO           (hPutStrLn, stderr)
import           Text.Show.Pretty    (ppShow)

deriving instance Show Val
deriving instance Eq Val
deriving instance Ord Val

type Key = Int

data Atom
    = REF Key
    | CNS Val
  deriving (Eq, Ord, Show)

data Alloc
    = CLOSE Atom [Atom] -- (add 3), (0 1 2 3), (self 3)
    | DELAY Proc -- argument list always empty
  deriving (Eq, Ord, Show)

{-
    SLOW has a Key, and not an Atom because it is always a referenced
    to something that we have explicitly forced, never to a constant.

    FAST is always to a Val because it is always a saturated call to a
    known value, and we need to know the actual value in order to generate
    code for this.

    SLOW uses a Key instead of an Atom because it isn't a PLAN value at
    all, but a reference to a localy bound proc.  It's a label, basically,
    we just use the same Key namespace for these also, because it makes
    life easier.
-}

data Op
   = FAST Val
   | SLOW Atom
   | RECUR
   | ADD
   | LOCAL Key
  deriving (Eq, Ord, Show)

data Strictness = STRICT | LAZY
  deriving (Eq, Ord, Show)

data Arg = ARG { argKey :: !Key, argStrict :: !Strictness }
  deriving (Eq, Ord)

instance Show Arg where
    show (ARG k s) = "(ARG " <> show k <> " " <> show s <> ")"

type FreeVars = S.Set Key

{-
    FORCE on a value is redundant and pointless, as constants are always
    in WHNF.  But enforcing this invariant in types just makes several
    places in the code more complex.
-}

data Stmt
    = RET Atom               --  return a
    | CALL Key Op [Atom]     --  let a = (add a b)
    | FORCE Key Atom         --  a = force b
    | PROC Key Proc          --  local a = force b
    | LET Key Alloc          --  let a = 0[a b]
    | REC [(Key, Alloc)]     --  letrec { r = (add a b); a = 0[r r] }
    | BR Atom [(Key, Proc)]  --  if a then b else c
  deriving (Eq, Ord, Show)

type Block = [Stmt]

data Forced = FORCED
    { forcedSet :: [Key]
    , forcedMap :: [(Key, Atom)]
    }

instance Semigroup Forced where
    FORCED as am <> FORCED bs bm = FORCED (as <> bs) (am <> bm)

instance Monoid Forced where
    mempty = FORCED mempty mempty

data Proc = MK_PROC
    { procArgs :: [Arg]
    , procFree :: FreeVars
    , procCode :: Block
    }
  deriving (Eq, Ord)

instance Show Proc where
    show (MK_PROC a f c) =
        "(MK_PROC (" <> show a <> ") (" <> show f <> ") (" <> show c <> "))"

ppProc :: Proc -> String
ppProc top = unlines (ppProc' 4 top)
  where
    -- indentation
    ind n = replicate (2*n) ' '

    -- atoms
    ppAtom = \case
      REF k -> "$" <> show k
      CNS (N n) -> show n
      CNS v -> show v

    intercalate :: String -> [String] -> String
    intercalate sep = concat . L.intersperse sep

    -- ops
    ppOp = \case
      ADD      -> "add"
      FAST v   -> "fast(" <> show v <> ")"
      SLOW a   -> "slow " <> ppAtom a
      LOCAL k -> "local %" <> show k
      RECUR    -> "recur"

    -- procs
    ppProc' :: Int -> Proc -> [String]
    ppProc' i (MK_PROC _args _ body) =
      concatMap (ppStmt i) body

    -- statements
    ppStmt :: Int -> Stmt -> [String]
    ppStmt i = \case
      LET k a ->
        [ ind i <> "$" <> show k <> " = " <> ppAlloc i a ]

      CALL k o xs ->
        [ ind i <> "$" <> show k <> " = "
          <> ppOp o <> "("
          <> intercalate ", " (map ppAtom xs) <> ")"
        ]

      FORCE k a ->
        [ ind i <> "$" <> show k <> " = force " <> ppAtom a ]

      RET a ->
        [ ind i <> "return " <> ppAtom a ]

      PROC k p ->
        (ind i <> "$" <> show k <> " = proc {")
        : ppProc' (i+1) p
       ++ [ ind i <> "}" ]

      REC as ->
        (ind i <> "rec {")
        : concatMap (\(k,a) ->
              [ ind (i+1) <> "$" <> show k <> " = " <> ppAlloc i a ]) as
       ++ [ ind i <> "}" ]

      BR a bs ->
        (ind i <> "switch " <> ppAtom a <> " {")
        : concatMap (ppBranch (i+1)) bs
       ++ [ ind i <> "}" ]

    -- branches
    ppBranch :: Int -> (Key, Proc) -> [String]
    ppBranch i (k,p) =
      (ind i <> "[%" <> show k <> "]{")
      : ppProc' (i+1) p
     ++ [ ind i <> "}" ]

    -- allocations
    ppAlloc :: Int -> Alloc -> String
    ppAlloc i = \case
      CLOSE f xs ->
        ppAtom f <> "[" <> intercalate ", " (map ppAtom xs) <> "]"

      DELAY p ->
        "delay {\n"
        <> unlines (ppProc' (i+1) p)
        <> ind i <> "}"

indent :: Int -> String
indent n = replicate (2*n) ' '

type UniqueM = State Int

gensym :: UniqueM Int
gensym = do
    maxKey <- get
    let !new = succ maxKey
    put new
    pure new

type Refs = M.Map Key Int

procCounts :: Proc -> State Refs ()
procCounts = traverse_ stmtCounts . procCode

stmtCounts :: Stmt -> State Refs ()
stmtCounts = \case
    RET x       -> atomCounts x
    CALL g o xs -> boundCounts g >> operCounts o >> traverse_ atomCounts xs
    FORCE g k   -> boundCounts g >> atomCounts k
    PROC g p    -> boundCounts g >> procCounts p
    LET g a     -> allocCounts (g, a)
    REC as      -> traverse_ allocCounts as
    BR a br     -> atomCounts a >> traverse_ (procCounts . snd) br

operCounts :: Op -> State Refs ()
operCounts FAST{}    = pure ()
operCounts (SLOW k)  = atomCounts k
operCounts RECUR     = pure ()
operCounts ADD       = pure ()
operCounts (LOCAL k) = refrCounts k

atomCounts :: Atom -> State Refs ()
atomCounts (REF r) = refrCounts r
atomCounts CNS{}   = pure ()

-- This makes sure that all bound keys are represented, even if
-- not referenced.
boundCounts :: Key -> State Refs ()
boundCounts k = modify' $ M.insertWith const k 0

allocCounts :: (Key, Alloc) -> State Refs ()
allocCounts (g, a) = do
    boundCounts g
    case a of CLOSE f xs -> atomCounts f >> traverse_ atomCounts xs
              DELAY p    -> procCounts p

refrCounts :: Key -> State Refs ()
refrCounts k = modify' $ M.insertWith (+) k 1

{-
    This counts the number of times a key (a bound value, or a local proc)
    is refered to.

    Since the only valid operation on a Proc is to call it, the result
    also tells you the number of times a local proc is called.
-}
refcounts :: Proc -> Refs
refcounts = \p -> execState (procCounts p) mempty

listOfProcs :: Proc -> [(Key, Proc)]
listOfProcs top = procCode top >>= \case
    PROC k p -> (k, p) : listOfProcs p
    LET _ a  -> alloc a
    REC as   -> do (_, a) <- as; alloc a
    BR _ bs  -> do (_, b) <- bs; listOfProcs b
    RET{}    -> []
    CALL{}   -> []
    FORCE{}  -> []
  where
    alloc a = do DELAY p <- [a]; listOfProcs p

singleUseProcs :: Proc -> [(Key, Proc)]
singleUseProcs top =
    filter singleUse (listOfProcs top)
  where
    singleUse (k, _) = Just 1 == M.lookup k counts
    counts = refcounts top

inlineSingleUseProcs :: Proc -> UniqueM Proc
inlineSingleUseProcs p =
    case singleUseProcs p of
        []  -> pure p
        x:_ -> inlineCallsToProc x p >>= inlineSingleUseProcs


deadCodeElim :: Proc -> Proc
deadCodeElim top = proc top
  where
    counts = refcounts top :: Refs

    keep k = M.findWithDefault 0 k counts > 0

    alloc :: Alloc -> Alloc
    alloc = \case
        DELAY p    -> DELAY (proc p)
        CLOSE f xs -> CLOSE f xs

    proc :: Proc -> Proc
    proc = overCode $ mapMaybe \case
        LET k a
          | keep k    -> Just (LET k (alloc a))
          | otherwise -> Nothing

        PROC k p
          | keep k    -> Just (PROC k (proc p))
          | otherwise -> Nothing

        -- TODO: generalize to all pure calls
        s@(CALL k ADD _)
            | keep k    -> Just s
            | otherwise -> Nothing

        REC as ->
          case filter (keep . fst) as of
            []  -> Nothing
            as' -> Just (REC (map (\(k,a) -> (k, alloc a)) as'))

        BR a bs ->
          Just (BR a (map (\(k,p) -> (k, proc p)) bs))

        s -> Just s

{-
    This is an inlining utility which replaces references to arguments
    with the actual arguments passed in.

    Input:

        proc p x y =
            z = add x y
            retuen z
        res = p(0, a)
        return res

    Output:

        z = add 0 a
        ret z
-}
subst :: M.Map Key Atom -> Block -> Block
subst table = block
  where
    proc :: Proc -> Proc
    proc = overCode block

    block :: Block -> Block
    block = map \case
        RET a       -> RET (atom a)
        CALL k o xs -> CALL k (oper o) (atom <$> xs)
        FORCE k a   -> FORCE k (atom a)
        PROC k p    -> PROC k (proc p)
        LET k a     -> LET k (alloc a)
        REC bs      -> REC $ bs <&> \(k,a) -> (k, alloc a)
        BR a bs     -> BR (atom a) $ bs <&> \(k, p) -> (k, proc p)

    alloc :: Alloc -> Alloc
    alloc (CLOSE f xs) = CLOSE (atom f) (atom <$> xs)
    alloc (DELAY p)    = DELAY (proc p)

    oper :: Op -> Op
    oper (SLOW a) = SLOW (atom a)
    oper o        = o

    atom :: Atom -> Atom
    atom (REF k) | Just p <- M.lookup k table = p
    atom a                                    = a

{-
    To inline any tail call, we just inline the body of the called
    function and drop the continuation.

    To inline basic blocks, we just inline the statements except the
    return, and then replace any references to the result of the call
    with whatever was returned.

    The remaining case is more complicated: inlinling a non-tail call
    into a function which contains it's own control flow.

    This works by turning our continuation into a join point (a local
    proc with an argument), binding that, and then using the callee as
    the continuation, replacing all returns with tail calls into this
    join point.

    For example, with an input like this:

        proc foo =
            if x then y else z
        let a = foo()
        let b = bar(a)
        return b

    We would have an output like this:

        proc $1(a) =
            let b = bar(a)
            return b
        if x then
             c = $1(y)
             ret c
        else
             d = $1(z)
             ret d
-}

inline :: (Key, Proc, [Atom]) -> Block -> UniqueM Block
inline (rk, target, params) cont =
    let table    = M.fromList (zip (argKey <$> procArgs target) params)
        retarget = overCode (subst table) target
    in if isTailCall cont then
        pure $ subst table (procCode target)
    else case openBasicBlock retarget of
        Just (stmts, targetRet) ->
            pure $ stmts <> subst (M.singleton rk targetRet) cont
        Nothing -> do
            jp <- gensym
            let ret a = gensym <&> \t -> [CALL t (LOCAL jp) [a], RET (REF t)]
            let joinProc = mkProc [ARG rk STRICT] cont
            retarget' <- rewriteReturns ret retarget
            pure (PROC jp joinProc : procCode retarget')
  where
    isTailCall [RET (REF k)] = k==rk
    isTailCall _             = False

rewriteReturns :: (Atom -> UniqueM [Stmt]) -> Proc -> UniqueM Proc
rewriteReturns f = proc
  where
    proc = forCode block
    block = fmap concat . traverse stmt
    branch (k,p) = (k,) <$> proc p
    stmt = \case BR a bs -> L.singleton . BR a <$> traverse branch bs
                 RET a   -> f a
                 s       -> pure [s]

inlineCallsToProc :: (Key, Proc) -> Proc -> UniqueM Proc
inlineCallsToProc (targetKey, targetProc) = proc
  where
    proc :: Proc -> UniqueM Proc
    proc = forCode block

    branch :: (Key, Proc) -> UniqueM (Key, Proc)
    branch (k, p) = (k,) <$> proc p

    alloc :: Alloc -> UniqueM Alloc
    alloc a@CLOSE{} = pure a
    alloc (DELAY p) = DELAY <$> proc p

    letrec :: (Key, Alloc) -> UniqueM (Key, Alloc)
    letrec (k, DELAY p)    = (k,) . DELAY <$> proc p
    letrec (k, CLOSE f xs) = pure (k, CLOSE f xs)

    block :: Block -> UniqueM Block
    block [] = pure []
    block (s:cont) = case s of
        CALL k (LOCAL c) xs | c==targetKey ->
            inline (k, targetProc, xs) cont >>= block
        PROC k _ | k==targetKey -> block cont
        PROC k p                -> block1 (PROC k <$> proc p) cont
        BR a bs                 -> block1 (BR a <$> traverse branch bs) cont
        RET{}                   -> (s:) <$> block cont
        FORCE{}                 -> (s:) <$> block cont
        CALL{}                  -> (s:) <$> block cont
        LET k a                 -> block1 (LET k <$> alloc a) cont
        REC as                  -> block1 (REC <$> traverse letrec as) cont

    block1 :: UniqueM Stmt -> Block -> UniqueM Block
    block1 s b = (:) <$> s <*> block b

mkProc :: [Arg] -> Block -> Proc
mkProc as b = MK_PROC as (freeVars (argKey <$> as) b) b

freeVars :: [Key] -> Block -> S.Set Key
freeVars = \args b -> evalState (block b) (S.fromList args)
  where
    block = many stmt

    bound key = modify' (S.insert key)

    many act xs = S.unions <$> traverse act xs

    stmt :: Stmt -> State (S.Set Key) (S.Set Key)
    stmt = \case
        RET k       -> atom k
        FORCE k x   -> bound k >> atom x
        PROC k p    -> bound k >> proc p
        LET k x     -> bound k >> alloc x
        REC as      -> traverse_ (bound . fst) as >> many (alloc . snd) as
        BR a ps     -> S.union <$> atom a <*> many branch ps
        CALL k o xs -> bound k >> (S.union <$> oper o <*> many atom xs)

    alloc :: Alloc -> State (S.Set Key) (S.Set Key)
    alloc (CLOSE f xs) = many atom (f : xs)
    alloc (DELAY p)    = proc p

    branch = proc . snd

    proc = many ref . S.toList . procFree

    atom CNS{}   = pure S.empty
    atom (REF k) = ref k

    oper (SLOW k)  = atom k
    oper (LOCAL k) = ref k
    oper _         = pure S.empty

    ref k = get <&> \b -> if k `S.member` b then S.empty else S.singleton k

{-
    usedLazily checks if a binding is ever used lazily within a block.

    A binding is used lazily if it is used for anything else besides
    forcing.  Specifically if it is uses as a parameter to an operation,
    stored in a closure, or referenced from another thunk.

    This is a part of the analysis logic for the `dethunk` pass: If
    the only thing we do with a binding is to force it, then we don't
    actually need it to be a thunk in the first place.

    A binding is considered ‘used lazily’ iff it may be evaluated
    more than once or at a non-deterministic time.
-}
usedLazily :: Key -> Block -> Bool
usedLazily =
    \k b -> any (== k) (bloc b)
  where
    bloc = concat . map stmt

    branch = proc . snd

    proc = bloc . procCode

    stmt :: Stmt -> [Key]
    stmt (RET a)       = atom a
    stmt (CALL _ f xs) = oper f <> concatMap atom xs
    stmt FORCE{}       = [] -- not used lazily!
    stmt (PROC _ p)    = bloc (procCode p) -- could be strict
    stmt (LET _ a)     = alloc a
    stmt (REC as)      = concatMap (alloc . snd) as
    stmt (BR a ps)     = atom a <> concatMap branch ps

    alloc :: Alloc -> [Key]
    alloc (CLOSE k xs) = concatMap atom (k:xs)
    alloc (DELAY p)    = S.toList (procFree p)

    atom :: Atom -> [Key]
    atom (REF k) = [k]
    atom (CNS _) = []

    oper :: Op -> [Key]
    oper (SLOW (REF k)) = [k]
    oper _              = []

{-
    Reforce rewrites blocks to avoid situations where we force something
    twice or reference a thunk which has already been forced.

    For example, if we have code like this:

        b <- force a  --  First force.
        c <- force b  --  Redundant: b is never a thunk
        d <- force a  --  References a thunk when WHNF version is in scope.
        call (op a)   --  References a thunk when WHNF version is in scope.

    Then that is rewritten as:

        b <- force a
        call (op b)
-}
reforce :: Proc -> Proc
reforce =
    \top -> overCode (go $ args (procArgs top) mempty) top
  where
    go f block = case block of
        []              -> []
        RET a : _       -> RET (atom f a)      : []
        PROC k p : b    -> PROC k (proc f p)   : go f b
        LET k a : b     -> LET k (alloc f a)   : go (knowclz [(k,a)] f) b
        REC as : b      -> REC (bind f <$> as) : go (knowclz as f) b
        BR a bs : _     -> BR (atom f a) (branch f <$> bs)   : []
        CALL k o xs : b -> CALL k (reop f o) (atom f <$> xs) : go (known k f) b
        FORCE k x : b   -> whnf f k (atom f x) b

    branch :: Forced -> (Key, Proc) -> (Key, Proc)
    branch f (k, p) = (k, proc f p)

    proc :: Forced -> Proc -> Proc
    proc f p = overCode (go $ args (procArgs p) f) p

    args :: [Arg] -> Forced -> Forced
    args []           !f = f
    args (ARG k s:as) !f = args as (case s of STRICT -> known k f; _ -> f)

    whnf :: Forced -> Key -> Atom -> [Stmt] -> [Stmt]
    whnf f k a b | isForced f a = go (alias k a f) b
    whnf f k a b | otherwise    = FORCE k a : go (explicit k a f) b

    isForced :: Forced -> Atom -> Bool
    isForced f (REF k) = elem k (forcedSet f)
    isForced _ CNS{}   = True

    explicit :: Key -> Atom -> Forced -> Forced
    explicit k (REF a) = (FORCED [k] [(a,REF k)] <>)
    explicit _ CNS{}   = error "impossible"

    known :: Key -> Forced -> Forced
    known k = (FORCED [k] [] <>)

    -- invariant: `old` is always a canonical key (already rewritten).
    alias :: Key -> Atom -> Forced -> Forced
    alias new old = (FORCED [] [(new, old)] <>)

    -- Partial applications are always in WHNF by definition, so it is
    -- pointless to force them, even if they are part of a letrec binding.
    -- Delayed blocks are thunks, however.
    knowclz :: [(Key, Alloc)] -> Forced -> Forced
    knowclz []                f = f
    knowclz ((_, DELAY{}):as) f = knowclz as f
    knowclz ((k, CLOSE{}):as) f = knowclz as (known k f)

    reop :: Forced -> Op -> Op
    reop f (SLOW key) = SLOW (atom f key)
    reop _ o          = o

    bind :: Forced -> (Key, Alloc) -> (Key, Alloc)
    bind f (k, a) = (k, alloc f a)

    alloc :: Forced -> Alloc -> Alloc
    alloc f (DELAY p)    = DELAY (proc f p)
    alloc f (CLOSE v as) = CLOSE (atom f v) (atom f <$> as)

    rewrite :: Forced -> Key -> Atom
    rewrite (FORCED _ as) k =
        case lookup k as of
            Nothing -> REF k
            Just k' -> k'

    atom :: Forced -> Atom -> Atom
    atom f (REF k) = rewrite f k
    atom _ (CNS v) = CNS v

{-
    `optimizeEachBlock` transforms and optimization which runs on a
    single shallow block (and not on sub-blocks) into an optimization
    that runs on all sub-blocks recursively, running on outer blocks
    first.
-}

optimizeEachBlock :: (Proc -> Proc) -> Proc -> Proc
optimizeEachBlock opt = go
  where
    go = overCode (map stmt) . opt

    stmt = \case
        PROC k p        -> PROC k (go p)
        LET k (DELAY p) -> LET k (DELAY (go p))
        REC as          -> REC $ as <&> \case (k, DELAY p) -> (k, DELAY (go p))
                                              kp           -> kp
        BR a bs         -> BR a $ bs <&> \(k,p) -> (k, go p)
        s               -> s

{-
    `reforce` bubbles forces upward and sink allocations downward, unless
    that would change observable effects or violate data dependencies.

    In general, evaluation should happen as early as possible so that
    code after the evaluation can make use of the WHNF binding and
    avoid redundant forcing. This also creates new opportunities for
    no-update thunks and eager evaluation, since earlier forces reduce
    the number of lazy uses of thunks.

    Conversely, allocations should happen as late as possible to
    minimize the amount of live heap data at any point in time and to
    cluster allocations so they may be combined into fewer GC
    allocations.

    This pass performs dependency-based rescheduling subject to the
    following rules:

    •   A FORCE may be moved earlier past another statement if that
        statement has no effects and the FORCE does not depend on any
        keys bound by that statement.

    •   An allocation (LET / REC) may be moved later past another
        statement if that statement does not reference any of the keys
        bound by the allocation.

    In this IR, allocation is pure and has no observable effects, so
    allocation order is unconstrained except by key dependencies.
-}

reschedule :: Proc -> Proc
reschedule =
    optimizeEachBlock $ overCode $ foldr ins []
  where
    ins s (x:xs) | reorder s x = x : ins s xs
    ins s xs                   = s : xs

    isTerm RET{} = True
    isTerm BR{}  = True
    isTerm _     = False

    reorder a b = not (isTerm b || fxDep || refDep || noGain)
      where
        counts = execState (stmtCounts b) mempty
        refDep = any (`M.member` counts) (stmtKeys a)
        fxDep  = hasfx a && hasfx b
        noGain = priority a >= priority b

    priority FORCE{} = 2 :: Int
    priority LET{}   = 0
    priority REC{}   = 0
    priority _       = 1

    hasfx LET{}          = False
    hasfx REC{}          = False
    hasfx PROC{}         = False
    hasfx (CALL _ ADD _) = False
    hasfx _              = True

tests_reschedule_allocations :: Bool
tests_reschedule_allocations =
    all testone cases
  where
    testone (nm, inp, out) =
        testcase nm reschedule (mkProc [] inp) (mkProc [] out)

    cases =
      [   ( "1. Simple allocation sinking"
          , [ LET 1 (CLOSE (CNS (N 10)) [])
            , CALL 2 ADD [CNS (N 1), CNS (N 2)]
            , RET (CNS (N 0))
            ]
          , [ CALL 2 ADD [CNS (N 1), CNS (N 2)]
            , LET 1 (CLOSE (CNS (N 10)) [])
            , RET (CNS (N 0))
            ]
          )

      ,   ( "2. Allocation stops at first use"
          , [ LET 1 (CLOSE (CNS (N 10)) [])
            , CALL 3 ADD [CNS (N 2), CNS (N 3)]
            , CALL 2 ADD [REF 1, CNS (N 1)]
            , RET (REF 3)
            ]
          , [ CALL 3 ADD [CNS (N 2), CNS (N 3)]
            , LET 1 (CLOSE (CNS (N 10)) [])
            , CALL 2 ADD [REF 1, CNS (N 1)]
            , RET (REF 3)
            ]
          )

      ,   ( "3. Multiple allocations sink independently"
          , [ LET 1 (CLOSE (CNS (N 1)) [])
            , LET 2 (CLOSE (CNS (N 2)) [])
            , CALL 3 ADD [REF 1, CNS (N 10)]
            , CALL 4 ADD [REF 2, CNS (N 20)]
            , RET (REF 4)
            ]
          , [ LET 1 (CLOSE (CNS (N 1)) [])
            , CALL 3 ADD [REF 1, CNS (N 10)]
            , LET 2 (CLOSE (CNS (N 2)) [])
            , CALL 4 ADD [REF 2, CNS (N 20)]
            , RET (REF 4)
            ]
          )

      ,   ( "4. Allocation does not cross force"
          , [ LET 1 (CLOSE (CNS (N 7)) [])
            , FORCE 2 (REF 1)
            , CALL 3 ADD [REF 2, CNS (N 1)]
            , RET (REF 3)
            ]
          , [ LET 1 (CLOSE (CNS (N 7)) [])
            , FORCE 2 (REF 1)
            , CALL 3 ADD [REF 2, CNS (N 1)]
            , RET (REF 3)
            ]
          )

      ,   ( "5. Allocation crosses effectful call"
          , [ LET 1 (CLOSE (CNS (N 5)) [])
            , CALL 2 (SLOW (REF 99)) []
            , RET (CNS (N 0))
            ]
          , [ CALL 2 (SLOW (REF 99)) []
            , LET 1 (CLOSE (CNS (N 5)) [])
            , RET (CNS (N 0))
            ]
          )

      ,   ( "6. Effects may not rescheduled"
          , [ CALL 3 (SLOW (REF 99)) [REF 1]
            , FORCE 2 (REF 1)
            , RET (CNS (N 0))
            ]
          , [ CALL 3 (SLOW (REF 99)) [REF 1]
            , FORCE 2 (REF 1)
            , RET (CNS (N 0))
            ]
          )

      ,   ( "7. Pure calls may can be rescheduled"
          , [ CALL 3 ADD [REF 1, REF 1]
            , FORCE 2 (REF 1)
            , RET (CNS (N 0))
            ]
          , [ FORCE 2 (REF 1)
            , CALL 3 ADD [REF 1, REF 1]
            , RET (CNS (N 0))
            ]
          )
      ]


optimizeOnce :: Proc -> UniqueM Proc
optimizeOnce p = do
    let q = dethunk $ reforce $ reschedule $ lowerPureBinds p
    deadCodeElim <$> inlineSingleUseProcs q

fixedPointM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
fixedPointM f x = do
    x' <- f x
    if x' == x then pure x else fixedPointM f x'

{-
    optimize runs all optimizations repeatedly, until a pass produces
    no changes.  This converges because no pass introduces new sharing
    or new thunks.
-}

optimize :: Proc -> Proc
optimize top = evalState (fixedPointM optimizeOnce top) maxKey
  where
    maxKey :: Int
    maxKey =
        case M.maxViewWithKey (refcounts top) of
            Nothing -> 0
            Just ((k, _), _) -> k

blockProc :: Block -> Proc
blockProc = mkProc []

ex1 :: Proc
ex1 = optimize $ blockProc $
    [ RET (CNS (N 0))
    ]

ex2 :: Proc
ex2 = optimize $ blockProc $
    [ fors 1 0
    , RET (REF 1)
    ]

ex3 :: Proc
ex3 = optimize $ blockProc $
    [ fors 1 0
    , RET (REF 1)
    ]

ex4 :: Proc
ex4 = optimize $ blockProc $
    [ fors 1 0
    , fors 2 1
    , fors 3 2
    , RET (REF 3)
    ]

ex5 :: Proc
ex5 = optimize $ blockProc $
    [ fors 1 0
    , fors 2 0
    , CALL 3 ADD [REF 1, REF 2]
    , RET (REF 3)
    ]

ex6 :: Proc
ex6 = optimize $ mkProc [ARG 0 STRICT]
    [ CALL 1 ADD [REF 0, REF 0]
    , fors 2 1
    , RET (REF 2)
    ]

-- This should reorder the force to be before the allocation of the thunk,
-- AND THEN the reforce optimization noticies that the argument is
-- already forced and removes the redundant force.

ex7 :: Proc
ex7 = optimize $ blockProc
    [ LET 2 $ DELAY $ blockProc
                  [ fors 2 1
                  , CALL 3 ADD [REF 2, REF 2]
                  , RET (REF 3)
                  ]
    , fors 3 1
    , LET 4 (CLOSE (CNS (N 0)) [REF 2, REF 3])
    , RET (REF 4)
    ]

test :: IO ()
test = do
    case force test_results of
        (True, _) ->
            hPutStrLn stderr "SUCCESS All tests pass!"
        (False, ts) -> do
            hPutStrLn stderr "FAILURE Some tests failed"
            for_ ts \(nm, res) ->
                unless res do hPutStrLn stderr ("\t" <> nm)

test_results :: (Bool, [(String, Bool)])
test_results = (all snd results, results)
  where
    results =
        [ ("test_no_alias_chains", test_no_alias_chains)
        , ("test_dethunk_simple", test_dethunk_simple)
        , ("test_move_into_thunk", test_move_into_thunk)
        , ("test_not_moved_if_used_directly", test_not_moved_if_used_directly)
        , ("test_not_moved_if_multiple_children", test_not_moved_if_multiple_children)
        , ("test_letrec_moves_as_group", test_letrec_moves_as_group)
        , ("test_letrec_blocked_by_secondary_direct_use", test_letrec_blocked_by_secondary_direct_use)
        , ("test_move_multiple_pure_into_thunk", test_move_multiple_pure_into_thunk)
        , ("test_move_thunk_into_branch", test_move_thunk_into_branch)
        , ("test_not_moved_if_used_in_branch_cond", test_not_moved_if_used_in_branch_cond)
        , ("test_move_proc_into_thunk", test_move_proc_into_thunk)
        , ("test_move_proc_into_branch", test_move_proc_into_branch)
        , ("test_proc_not_moved_if_called_directly", test_proc_not_moved_if_called_directly)
        , ("test_not_moved_if_thunk_and_branch_both_reference", test_not_moved_if_thunk_and_branch_both_reference)
        , ("test_letrec_group_sinks_once", test_letrec_group_sinks_once)
        , ("test_move_into_branch_only_once", test_move_into_branch_only_once)
        , ("test_fixed_point_blocks_on_direct_use", test_fixed_point_blocks_on_direct_use)
        , ("test_cascading_moves", test_cascading_moves)
        , ("test_recursive_move_inside_thunk", test_recursive_move_inside_thunk)
        , ("test_recursive_move_inside_branch", test_recursive_move_inside_branch)
        , ("test_recursive_cascade", test_recursive_cascade)
        , ("test_recursive_blocked_by_direct_use", test_recursive_blocked_by_direct_use)
        , ("test_recursive_letrec_inside_thunk", test_recursive_letrec_inside_thunk)
        , ("test_slide_dethunk_inline_chain", test_slide_dethunk_inline_chain)
        , ("test_branch_slide_dethunk_inline", test_branch_slide_dethunk_inline)
        , ("test_multi_stage_cascade", test_multi_stage_cascade)
        , ("tests_reschedule_allocations", tests_reschedule_allocations)
        , ("unused thunk", test_occ_unused_thunk)
        , ("single force", test_occ_single_force)
        , ("double force", test_occ_double_force)
        , ("force one branch", test_occ_force_in_one_branch)
        , ("force both branches", test_occ_force_in_both_branches)
        , ("escape via close", test_occ_escape_via_close)
        , ("proc free var", test_occ_proc_free_var)
        , ("proc not called", test_occ_proc_not_called)
        , ("self recur zero", test_occ_self_recur_zero)
        , ("recur amplifies", test_occ_recur_amplifies)
        , ("test_occ_nested_thunks", test_occ_nested_thunks)
        ]

fors :: Key -> Key -> Stmt
fors k x = FORCE k (REF x)

test_no_alias_chains :: Bool
test_no_alias_chains =
    testcase "test_no_alias_chains" reforce input output
  where
    input  = blockProc [ fors 1 0, fors 2 1, fors 3 2, RET (REF 3) ]
    output = blockProc [ fors 1 0, RET (REF 1) ]

test_dethunk_simple :: Bool
test_dethunk_simple =
    testcase "test_dethunk_simple" dethunk inputProc outputProc
  where
    thunkProc :: Proc
    thunkProc = blockProc [ RET (CNS (N 42)) ]

    inputProc :: Proc
    inputProc = blockProc [ LET 1 (DELAY thunkProc)
                      , fors 2 1
                      , RET (REF 2)
                      ]

    outputProc :: Proc
    outputProc = blockProc [ PROC 1 thunkProc
                       , CALL 2 (LOCAL 1) []
                       , RET (REF 2)
                       ]

test_move_multiple_pure_into_thunk :: Bool
test_move_multiple_pure_into_thunk =
  tryPureMoves proc == [(1, 3), (2, 3)]
  where
    proc = mkProc []
      [ LET 1 (CLOSE (CNS (N 10)) [])
      , LET 2 (CLOSE (CNS (N 20)) [])
      , thunk 3
          [ CALL 4 ADD [REF 1, REF 2]
          , RET (REF 4)
          ]
      , RET (CNS (N 0))
      ]

test_move_thunk_into_branch :: Bool
test_move_thunk_into_branch =
  tryPureMoves proc == [(1, 2)]
  where
    proc = mkProc []
      [ LET 1 (CLOSE (CNS (N 0)) [])
      , mkbr (CNS (N 1))
          [ (2, [ RET (REF 1) ])
          , (3, [ RET (CNS (N 0)) ])
          ]
      ]

test_not_moved_if_used_in_branch_cond :: Bool
test_not_moved_if_used_in_branch_cond =
  null $ tryPureMoves $ mkProc []
    [ LET 1 (CLOSE (CNS (N 0)) [])
    , mkbr (REF 1)
        [ (2, [ RET (CNS (N 1)) ])
        , (3, [ RET (CNS (N 0)) ])
        ]
    ]

test_move_proc_into_thunk :: Bool
test_move_proc_into_thunk =
  tryPureMoves proc == [(1, 2)]
  where
    proc = mkProc []
      [ PROC 1 $ mkProc [] [ RET (CNS (N 99)) ]
      , thunk 2 [ CALL 3 (LOCAL 1) [], RET (REF 3) ]
      , RET (CNS (N 0))
      ]

test_move_proc_into_branch :: Bool
test_move_proc_into_branch =
  tryPureMoves proc == [(1, 2)]
  where
    proc = mkProc []
      [ PROC 1 $ mkProc [] [ RET (CNS (N 7)) ]
      , mkbr (CNS (N 0))
          [ (2, [ CALL 3 (LOCAL 1) [], RET (REF 3) ])
          , (3, [ RET (CNS (N 0)) ])
          ]
      ]

test_proc_not_moved_if_called_directly :: Bool
test_proc_not_moved_if_called_directly =
  null $ tryPureMoves $ mkProc []
    [ PROC 1 $ mkProc [] [ RET (CNS (N 7)) ]
    , CALL 2 (LOCAL 1) []
    , thunk 3 [ CALL 4 (LOCAL 1) [], RET (REF 4) ]
    , RET (REF 2)
    ]


test_not_moved_if_thunk_and_branch_both_reference :: Bool
test_not_moved_if_thunk_and_branch_both_reference =
  null $ tryPureMoves $ mkProc []
    [ LET 1 (CLOSE (CNS (N 0)) [])
    , thunk 2 [ RET (REF 1) ]
    , mkbr (CNS (N 0))
        [ (3, [ RET (REF 1) ]) ]
    ]

examples :: [Proc]
examples = [ex1, ex2, ex3, ex4, ex5, ex6, ex7]

openBasicBlock :: Proc -> Maybe (Block, Atom)
openBasicBlock = go [] . procCode
  where
    go _ []            = error "block never returns"
    go acc (RET a : _) = Just (reverse acc, a)
    go _ (BR{} : _)    = Nothing
    go acc (s:ss)      = go (s:acc) ss

procThunks :: Proc -> [(Key, Proc)]
procThunks =
    \p -> concatMap stmt (procCode p)
  where
    stmt :: Stmt -> [(Key, Proc)]
    stmt RET{} = []
    stmt CALL{} = []
    stmt FORCE{} = []
    stmt PROC{} = []
    stmt BR{} = []
    stmt (LET k a) = toList (alloc (k, a))
    stmt (REC as)  = mapMaybe alloc as

    alloc :: (Key, Alloc) -> Maybe (Key, Proc)
    alloc (k, DELAY p) = Just (k, p)
    alloc _            = Nothing

overCode :: (Block -> Block) -> (Proc -> Proc)
overCode f (MK_PROC a _ c) = mkProc a (f c)

forCode :: Monad m => (Block -> m Block) -> (Proc -> m Proc)
forCode f p = mkProc (procArgs p) <$> f (procCode p)

{-
    Dethunk converts thunks which are never used lazily into procs.
-}
dethunkStep :: Proc -> Proc
dethunkStep top =
    if M.null toProc then top else overCode block top
  where
    isProcable (k, _) = not $ usedLazily k $ procCode top

    toProc = M.fromList (filter isProcable ts)

    ts = procThunks top

    proc :: Proc -> Proc
    proc = overCode block

    block :: Block -> Block
    block = map stmt

    branch :: (Key, Proc) -> (Key, Proc)
    branch (k, p) = (k, proc p)

    stmt s = case s of
        LET k DELAY{} -> -- rewrite DELAY to PROC
            case M.lookup k toProc of
                Nothing -> s
                Just p -> PROC k p

        FORCE out (REF label) -> -- rewrite FORCE to CALL
            case M.lookup label toProc of
                Nothing -> s
                Just _  -> CALL out (LOCAL label) []

        PROC k p      -> PROC k (proc p)
        BR a ps       -> BR a (branch <$> ps)
        FORCE _ CNS{} -> s
        LET{}         -> s
        REC{}         -> error "TODO: dethunk letrec"
        RET{}         -> s
        CALL{}        -> s

dethunk :: Proc -> Proc
dethunk =
    overCode (map stmt) . dethunkStep
  where
    alloc :: Alloc -> Alloc
    alloc (DELAY p) = DELAY (dethunk p)
    alloc a@CLOSE{} = a

    stmt s = case s of
        LET k a         -> LET k (alloc a)
        PROC k p        -> PROC k (dethunk p)
        BR a ps         -> BR a $ ps <&> \(k,b) -> (k, dethunk b)
        FORCE _ CNS{}   -> s
        REC as          -> REC $ as <&> \(k,a) -> (k, alloc a)
        RET{}           -> s
        CALL{}          -> s
        FORCE{}         -> s

thunk :: Key -> Block -> Stmt
thunk k b = LET k $ DELAY $ mkProc [] b

mkbr :: Atom -> [(Key, Block)] -> Stmt
mkbr a bs = BR a (map (\(k,b) -> (k, blockProc b)) bs)

test_move_into_thunk :: Bool
test_move_into_thunk = tryPureMoves proc == [(1, 2)]
  where proc = mkProc []
            [ LET 1 $ CLOSE (CNS (N 0)) []
            , thunk 2 [ RET (REF 1) ]
            , RET (CNS (N 42))
            ]

test_not_moved_if_used_directly :: Bool
test_not_moved_if_used_directly =
  null $ tryPureMoves $ mkProc [] $
      [ LET 1 (CLOSE (CNS (N 0)) [])
      , fors 2 1
      , thunk 3 [ RET (REF 1) ]
      , RET (REF 2)
      ]

test_not_moved_if_multiple_children :: Bool
test_not_moved_if_multiple_children =
    null $ tryPureMoves $ mkProc [] $
      [ LET 1 (CLOSE (CNS (N 0)) [])
      , mkbr (CNS (N 0))
            [ (2, [RET (REF 1)])
            , (3, [RET (REF 1)])
            ]
      ]

test_letrec_moves_as_group :: Bool
test_letrec_moves_as_group =
    (== [(1, 3)]) $ tryPureMoves $ blockProc $
        [ REC [(1, CLOSE (REF 2) []), (2, CLOSE (REF 1) [])]
        , thunk 3 [ RET (REF 2) ]
        , RET (CNS (N 0))
        ]

test_letrec_blocked_by_secondary_direct_use :: Bool
test_letrec_blocked_by_secondary_direct_use =
  null $ tryPureMoves $ blockProc
      [ REC [ (1, CLOSE (REF 2) [])
            , (2, CLOSE (REF 1) [])
            ]
      , fors 3 2
      , thunk 4 [ RET (REF 1) ]
      , RET (REF 3)
      ]

{-
    Are any of these keys used directly during the execution of this
    basic block (ignoring branches and sub-blocks)?
-}
usedDirectly :: (Key -> Bool) -> Block -> Bool
usedDirectly target = any stmt
  where
    stmt = \case
        CALL _ c xs -> call c || any ref xs
        FORCE _ x   -> ref x
        RET a       -> ref a
        BR a _      -> ref a
        LET _ a     -> alloc a
        PROC{}      -> False
        REC as      -> not (any (target . fst) as) && any alloc (snd <$> as)
            -- REC ignores self-references within the same binding group.

    call (LOCAL f) = target f
    call (SLOW f)  = ref f
    call _         = False

    alloc (CLOSE f xs) = ref f || any ref xs
    alloc (DELAY _)    = False

    ref (REF x) = target x
    ref _       = False

-- Immediate child regions of a block (each has a binding key)
blockChildren :: Block -> [(Key, Proc)]
blockChildren = concatMap \case
    PROC k p           -> [(k, p)]
    LET k (DELAY p)    -> [(k, p)]
    BR _ bs            -> bs
    _                  -> []

stmtKeys :: Stmt -> [Key]
stmtKeys = \case
    CALL k _ _ -> [k]
    FORCE k _ -> [k]
    PROC k _ -> [k]
    LET k _ -> [k]
    REC as -> (fst <$> as)
    RET{} -> []
    BR{} -> []


-- Lower Pure Binds ------------------------------------------------------------

moveStmt :: (Key, Stmt, Key) -> Proc -> Proc
moveStmt (stmtKey, stmt, destKey) =
    overCode $ mapMaybe \s ->
        case stmtKeys s of
            k:_ | k==stmtKey -> Nothing
            _                -> Just if isTargetStmt s
                                     then prependToSubBlock s
                                     else s
  where
    proc :: Proc -> Proc
    proc = overCode (stmt:)

    alloc :: (Key, Alloc) -> (Key, Alloc)
    alloc (k, DELAY p) | k==destKey = (k, DELAY (proc p))
    alloc ka                        = ka

    branch :: (Key, Proc) -> (Key, Proc)
    branch (k, p) | k==destKey = (k, proc p)
    branch kp                  = kp

    isTargetStmt :: Stmt -> Bool
    isTargetStmt = \case
        PROC k _ -> k==destKey
        BR _ bs  -> any (==destKey) (fst <$> bs)
        REC as   -> any (==destKey) (fst <$> as)
        LET k _  -> k==destKey
        _        -> False

    prependToSubBlock :: Stmt -> Stmt
    prependToSubBlock = \case
        LET k (DELAY p) -> LET k $ DELAY (proc p)
        PROC k p        -> PROC k (proc p)
        BR a bs         -> BR a (branch <$> bs)
        REC as          -> REC (alloc <$> as)
        _               -> error "bad move target"

{-
    A list of binding without effects and their keys.  These are bindings
    that can potential be moved into more specific contexts.

    The returned key is a "statement idenfier" key, which can be used
    for deletions.  In a LETREC, it is the key of the first binding.
-}
pureBinds :: Block -> [(Key, Stmt)]
pureBinds = concatMap \s -> case s of
    PROC k _      -> [(k, s)]
    LET k _       -> [(k, s)]
    CALL k ADD _  -> [(k, s)]
    REC ((k,_):_) -> [(k, s)]
    _             -> []

{-
    computePureMoves computes the set of pure bindings (bindings which
    have no side-effects) which an be moved down into a sub-block and
    the sub-block to which it should be moved.

    A binding can be moved if it is not referenced in the top-level block,
    and exactly one immediate sub-block references it.

    The statement is identified by its binding key (or the first binding
    key of a letrec) which is sufficient to locate it for deletion.

    The target block is also identified by its binding key, which is
    sufficient to find it so that we can move into it.
-}

computePureMoves :: Proc -> [(Key, Stmt, Key)]
computePureMoves (procCode -> b) = do
    (stmtId, stmt) <- pureBinds b

    let keyset = S.fromList (stmtKeys stmt)

    let subBlocksWhichUse = do
            let subBlocks = blockChildren b
            (key, proc) <- subBlocks
            let references = (keyset `S.intersection` procFree proc)
            guard $ not $ S.null references
            pure key

    False  <- pure $ usedDirectly (`S.member` keyset) b
    [dest] <- pure $ subBlocksWhichUse
    pure (stmtId, stmt, dest)

tryPureMoves :: Proc -> [(Key, Key)]
tryPureMoves = map (\(a,_,c) -> (a,c)) . computePureMoves

{-
    lowerPureBinds moves non-effectful bindings (thunks, procs, partial
    applications, some primops like add) into more specific sub-blocks
    if that is the only place where they are used.

    For example, if a closure is only used in on branch of an if
    statement, we will move the closure allocation into that one branch.
-}
lowerPureBinds :: Proc -> Proc
lowerPureBinds = optimizeEachBlock go
  where
    go p = case computePureMoves p of
               []     -> p
               mv : _ -> go (moveStmt mv p)

--------------------------------------------------------------------------------

test_letrec_group_sinks_once :: Bool
test_letrec_group_sinks_once =
    testcase "test_letrec_group_sinks_once" lowerPureBinds proc expected
  where
    proc = mkProc []
      [ REC [ (1, CLOSE (REF 2) [])
            , (2, CLOSE (REF 1) [])
            ]
      , thunk 3 [ RET (REF 2) ]
      , RET (CNS (N 0))
      ]

    expected = mkProc []
      [ thunk 3
          [ REC [ (1, CLOSE (REF 2) [])
                , (2, CLOSE (REF 1) [])
                ]
          , RET (REF 2)
          ]
      , RET (CNS (N 0))
      ]

test_move_into_branch_only_once :: Bool
test_move_into_branch_only_once =
    testcase "test_move_into_branch_only_once" lowerPureBinds proc expected
  where
    proc = mkProc []
      [ LET 1 (CLOSE (CNS (N 7)) [])
      , mkbr (CNS (N 0))
          [ (2, [ RET (REF 1) ])
          , (3, [ RET (CNS (N 0)) ])
          ]
      ]

    expected = mkProc []
      [ mkbr (CNS (N 0))
          [ (2, [ LET 1 (CLOSE (CNS (N 7)) [])
                , RET (REF 1)
                ])
          , (3, [ RET (CNS (N 0)) ])
          ]
      ]

test_fixed_point_blocks_on_direct_use :: Bool
test_fixed_point_blocks_on_direct_use =
    testcase "test_fixed_point_blocks_on_direct_use" lowerPureBinds proc proc
  where
    proc = mkProc []
      [ LET 1 (CLOSE (CNS (N 0)) [])
      , thunk 2 [ RET (REF 1) ]
      , CALL 3 ADD [REF 1, CNS (N 1)]
      , RET (REF 3)
      ]

test_cascading_moves :: Bool
test_cascading_moves =
    testcase "test_cascading_moves" lowerPureBinds proc expected
  where
    proc = mkProc []
      [ LET 1 (CLOSE (CNS (N 0)) [])
      , thunk 2 [ RET (REF 1) ]
      , thunk 3 [ RET (REF 2) ]
      , RET (CNS (N 99))
      ]

    expected = mkProc []
      [ thunk 3
          [ thunk 2
              [ LET 1 (CLOSE (CNS (N 0)) [])
              , RET (REF 1)
              ]
          , RET (REF 2)
          ]
      , RET (CNS (N 99))
      ]

test_recursive_move_inside_thunk :: Bool
test_recursive_move_inside_thunk =
    testcase "test_recursive_move_inside_thunk" lowerPureBinds proc expected
  where
    proc = mkProc []
      [ thunk 1
          [ LET 2 (CLOSE (CNS (N 0)) [])
          , thunk 3 [ RET (REF 2) ]
          , RET (CNS (N 99))
          ]
      , RET (CNS (N 0))
      ]

    expected = mkProc []
      [ thunk 1
          [ thunk 3
              [ LET 2 (CLOSE (CNS (N 0)) [])
              , RET (REF 2)
              ]
          , RET (CNS (N 99))
          ]
      , RET (CNS (N 0))
      ]

test_recursive_move_inside_branch :: Bool
test_recursive_move_inside_branch =
    lowerPureBinds proc == expected
  where
    proc = mkProc []
      [ mkbr (CNS (N 0))
          [ (1,
              [ LET 2 (CLOSE (CNS (N 7)) [])
              , thunk 3 [ RET (REF 2) ]
              , RET (CNS (N 1))
              ])
          , (4, [ RET (CNS (N 2)) ])
          ]
      ]

    expected = mkProc []
      [ mkbr (CNS (N 0))
          [ (1,
             [ thunk 3
                  [ LET 2 (CLOSE (CNS (N 7)) [])
                  , RET (REF 2)
                  ]
              , RET (CNS (N 1))
              ])
          , (4, [ RET (CNS (N 2)) ])
          ]
      ]

test_recursive_cascade :: Bool
test_recursive_cascade =
    lowerPureBinds proc == expected
  where
    proc = mkProc []
      [ thunk 1
          [ LET 2 (CLOSE (CNS (N 0)) [])
          , thunk 3
              [ thunk 4 [ RET (REF 2) ]
              , RET (CNS (N 10))
              ]
          , RET (CNS (N 20))
          ]
      ]

    expected = mkProc []
      [ thunk 1
          [ thunk 3
              [ thunk 4
                  [ LET 2 (CLOSE (CNS (N 0)) [])
                  , RET (REF 2)
                  ]
              , RET (CNS (N 10))
              ]
          , RET (CNS (N 20))
          ]
      ]

test_recursive_blocked_by_direct_use :: Bool
test_recursive_blocked_by_direct_use =
    lowerPureBinds proc == proc
  where
    proc = mkProc []
      [ thunk 1
          [ LET 2 (CLOSE (CNS (N 0)) [])
          , fors 3 2
          , thunk 4 [ RET (REF 2) ]
          , RET (REF 3)
          ]
      ]

test_recursive_letrec_inside_thunk :: Bool
test_recursive_letrec_inside_thunk =
    lowerPureBinds proc == expected
  where
    proc = mkProc []
      [ thunk 1
          [ REC [ (2, CLOSE (REF 3) [])
                , (3, CLOSE (REF 2) [])
                ]
          , thunk 4 [ RET (REF 2) ]
          , RET (CNS (N 0))
          ]
      ]

    expected = mkProc []
      [ thunk 1
          [ thunk 4
              [ REC [ (2, CLOSE (REF 3) [])
                    , (3, CLOSE (REF 2) [])
                    ]
              , RET (REF 2)
              ]
          , RET (CNS (N 0))
          ]
      ]

test_slide_dethunk_inline_chain :: Bool
test_slide_dethunk_inline_chain = optimize proc == expected
  where
    proc =
        mkProc []
        [ LET 1 (CLOSE (CNS (N 10)) [])
        , thunk 2
            [ CALL 3 ADD [REF 1, CNS (N 1)]
            , RET (REF 3)
            ]
        , thunk 4
            [ fors 5 2
            , CALL 6 ADD [REF 5, CNS (N 2)]
            , RET (REF 6)
            ]
        , FORCE 7 (REF 4)
        , RET (REF 7)
        ]

    expected =
        mkProc []
        [ LET 1 $ CLOSE (CNS (N 10)) []
        , CALL 3 ADD [ REF 1, CNS (N 1) ]
        , CALL 6 ADD [REF 3, CNS (N 2)]
        , RET (REF 6)
        ]

testcase :: String -> (Proc -> Proc) -> Proc -> Proc -> Bool
testcase nm f inp expected =
    trace ("==== " <> nm <> " ====\n") $
    trace ("  INPUT:") $
    trace (ppProc inp)
    trace ("  EXPECTED:") $
    trace (ppProc expected)
    if result==expected then True else
        trace ("  OUTPUT:") $
        trace (ppProc result) $
        False
  where
    result = f inp

test_branch_slide_dethunk_inline :: Bool
test_branch_slide_dethunk_inline =
    testcase "test_branch_slide_dethunk_inline" optimize proc expected
  where
    proc =
      mkProc []
        [ LET 1 (CLOSE (CNS (N 7)) [])
        , thunk 2
            [ CALL 3 ADD [REF 1, CNS (N 1)]
            , RET (REF 3)
            ]
        , mkbr (CNS (N 0))
            [ (4,
                [ fors 5 2
                , RET (REF 5)
                ])
            , (6,
                [ RET (CNS (N 99)) ])
            ]
        ]

    expected =
      mkProc []
        [ mkbr (CNS (N 0))
            [ (4,
                [ LET 1 (CLOSE (CNS (N 7)) [])
                , CALL 3 ADD [ REF 1 , CNS (N 1) ]
                , RET (REF 3)
                ])
            , (6,
                [ RET (CNS (N 99)) ])
            ]
        ]

test_multi_stage_cascade :: Bool
test_multi_stage_cascade =
    testcase "test_multi_stage_cascade" optimize proc expected
  where
    proc =
      mkProc []
        [ LET 1 (CLOSE (CNS (N 1)) [])
        , LET 2 (DELAY $
            mkProc []
              [ CALL 3 ADD [REF 1, CNS (N 2)]
              , RET (REF 3)
              ])
        , LET 4 (DELAY $
            mkProc []
              [ FORCE 5 (REF 2)
              , CALL 6 ADD [REF 5, CNS (N 3)]
              , RET (REF 6)
              ])
        , LET 7 (DELAY $
            mkProc []
              [ FORCE 8 (REF 4)
              , CALL 9 ADD [REF 8, CNS (N 4)]
              , RET (REF 9)
              ])
        , FORCE 10 (REF 7)
        , RET (REF 10)
        ]

    expected =
      mkProc []
        [ LET 1 (CLOSE (CNS (N 1)) [])
        , CALL 3 ADD [REF 1, CNS (N 2)]
        , CALL 6 ADD [REF 3, CNS (N 3)]
        , CALL 9 ADD [REF 6, CNS (N 4)]
        , RET (REF 9)
        ]

test_no_inline_when_shared :: Bool
test_no_inline_when_shared =
    testcase "test_no_inline_when_shared" optimize proc proc
  where
    proc =
      mkProc []
        [ thunk 1
            [ CALL 2 ADD [CNS (N 1), CNS (N 2)]
            , RET (REF 2)
            ]
        , fors 3 1
        , fors 4 1
        , CALL 5 ADD [REF 3, REF 4]
        , RET (REF 5)
        ]

placeStmt :: Stmt -> Key -> Proc -> Proc
placeStmt item target = overCode bloc
  where
    bloc = map stmt

    stmt s = case s of
        PROC k p | k==target -> PROC k (place p)
                 | otherwise -> s
        BR a bs -> BR a (br <$> bs)
        LET k (DELAY p) | k==target -> LET k (DELAY (place p))
        REC as  -> REC (alloc <$> as)
        LET{}   -> s
        RET{}   -> s
        CALL{}  -> s
        FORCE{} -> s

    alloc :: (Key, Alloc) -> (Key, Alloc)
    alloc (k, DELAY p) | k==target = (k, DELAY (place p))
    alloc ka                       = ka

    br :: (Key, Proc) -> (Key, Proc)
    br (k,p) | k==target = (k, place p)
    br kp                = kp

    place :: Proc -> Proc
    place = overCode (item:)

{-
===============================================================================
Occurrence Analysis (for Thunk Update Flags)
===============================================================================

This pass computes *entry attempt counts* for thunks and local procs, in order
to decide whether a thunk must be Updatable (Many) or can be NoUpdate (Zero /
Once).

This analysis exists solely to drive correct and efficient thunk update
decisions. It is not a general-purpose cost or execution-count analysis.

------------------------------------------------------------------------------
High-level mental model
------------------------------------------------------------------------------

• An occurrence represents a *possible entry attempt*.
• Thunk bodies execute at most once; repeated entries are handled by UPDATE.
• We therefore count entry attempts, not executions.

Occurrences live in a small monotone lattice:

    Zero < Once < Many

------------------------------------------------------------------------------
Core semantic assumptions (IR invariants)
------------------------------------------------------------------------------

• All code executes at most once per entry, except via RECUR.
• Thunks are entered only via FORCE.
• Binding a thunk or local proc has no effect; only entering it matters.
• Local procs do not escape, except via CLOSE (pessimistic).
• Branches (BR) are mutually exclusive.
• BR and RET are terminators; code after them is unreachable.

------------------------------------------------------------------------------
Sequential composition and branching
------------------------------------------------------------------------------

• Sequential statements accumulate occurrences via addition (+).
• Branches (BR) are mutually exclusive:
    – Occurrences are joined via lub (max), not summed.
• Because BR is a terminator, only the joined occurrence result matters;
  pending state does not need to be merged across branches.

------------------------------------------------------------------------------
Lazy charging of thunk / proc bodies
------------------------------------------------------------------------------

Thunk and proc bodies are NOT charged when they are defined.

Instead:
• Each thunk/proc is registered as *pending* with a lazy occurrence summary.
• On the first possible entry (FORCE / LOCAL call), the summary is activated:
    – Free-variable occurrences are charged once.
    – Argument occurrences are mapped to the actual parameters.
• Summaries are memoized automatically via laziness.

This matches runtime semantics: a thunk body may execute at most once.

------------------------------------------------------------------------------
Recursion
------------------------------------------------------------------------------

Local procs are non-recursive. The only source of repetition is RECUR.

Occurrence analysis is therefore a fixed point over top-level argument uses:
• Start with all arguments at Zero.
• Compute occurrences for one execution of the proc.
• Feed RECUR argument uses back into the next iteration.
• Iterate until stable.

The lattice is finite, so termination is guaranteed.

------------------------------------------------------------------------------
Design notes
------------------------------------------------------------------------------

• This analysis is intentionally conservative where additional precision would
  complicate the model without improving Update-flag decisions.
• Precision is traded for simplicity only in ways that preserve correctness.
• The result is a demand-driven, lazily memoized occurrence analysis that
  closely mirrors runtime thunk semantics.
-}



data Uses = Zero | Once | Many
  deriving (Eq, Ord, Show)

plus :: Uses -> Uses -> Uses
plus Zero u    = u
plus u    Zero = u
plus _     _   = Many

type Occ = M.Map Key Uses

data ProcOcc = ProcOcc
  { occFree :: Occ        -- free-variable usage
  , occArgs :: [Uses]     -- per-argument usage
  }

data OccState = ST
  { stOcc  :: !Occ                  -- accumulated entry attempts
  , stPend :: !(M.Map Key ProcOcc)  -- pending (not yet entered)
  }

emptyST :: OccState
emptyST = ST M.empty M.empty

activate :: Key -> [Atom] -> State OccState ()
activate k params =
  gets (M.lookup k . stPend) >>= \case
      Nothing -> pure ()
      Just (ProcOcc free argUses) -> do
          modify' \s -> s { stPend = M.delete k (stPend s) }
          for_ (M.toList free) \(fv, u) -> useThunk u (REF fv)
          for_ (zip argUses params) (uncurry useThunk)

useThunk :: Uses -> Atom -> State OccState ()
useThunk Zero _    = pure ()
useThunk _ CNS{}   = pure ()
useThunk u (REF k) = do
    activate k []
    modify' \st -> st { stOcc = M.insertWith plus k u (stOcc st) }

procOcc :: [Uses] -> Proc -> ProcOcc
procOcc recurArgUses p =
    ProcOcc free args
  where
    st   = execState (analyzeProc recurArgUses p) emptyST
    occ  = stOcc st
    free = M.filterWithKey (\k _ -> k `elem` (procFree p)) occ
    args = [ M.findWithDefault Zero (argKey a) occ | a <- procArgs p ]

analyzeProc :: [Uses] -> Proc -> State OccState ()
analyzeProc recurArgUses p =
    for_ (procCode p) (analyzeStmt recurArgUses)

analyzeStmt :: [Uses] -> Stmt -> State OccState ()
analyzeStmt recurArgUses = \case
    LET k (DELAY p)     -> registerPending k p
    LET _ (CLOSE f xs)  -> escapeAlloc f xs
    PROC k p            -> registerPending k p
    FORCE _ a           -> useThunk Once a
    CALL _ (LOCAL k) xs -> activate k xs
    CALL _ op xs        -> for_ (zip (argUses op) xs) (uncurry useThunk)
    BR _ bs             -> branches (snd <$> bs)
    RET _               -> pure ()
    REC as              -> letrec as
  where
    argUses RECUR   = recurArgUses
    argUses ADD     = [Once, Once]
    argUses FAST{}  = repeat Many -- TODO: lookup from metadata
    argUses SLOW{}  = repeat Many -- Unknown call, must be pessimistic
    argUses LOCAL{} = error "this should not be reachable"

    letrec as = do
        for_ as \case (k, DELAY p)    -> registerPending k p; _ -> pure ()
        for_ as \case (_, CLOSE f xs) -> escapeAlloc f xs;    _ -> pure ()

    escapeAlloc f xs = useThunk Many f >> for_ xs (useThunk Many)

    registerPending k p = modify' \st ->
        st{ stPend = M.insert k (procOcc recurArgUses p) (stPend st) }

    branches :: [Proc] -> State OccState ()
    branches ps = modify' \st ->
      st{ stOcc = foldl' (M.unionWith max) M.empty $ ps <&> \p ->
                    stOcc (execState (analyzeProc recurArgUses p) st) }

occurs :: Proc -> Occ
occurs top = go initArgOcc
  where
    topArgs = map argKey (procArgs top)

    initArgOcc :: Occ
    initArgOcc = M.fromList [ (k, Zero) | k <- topArgs ]

    recurArgUsesFrom :: Occ -> [Uses]
    recurArgUsesFrom argOcc =
      [ M.findWithDefault Zero k argOcc | k <- topArgs ]

    restrictArgs :: Occ -> Occ
    restrictArgs occ =
      M.fromList [ (k, M.findWithDefault Zero k occ) | k <- topArgs ]

    go :: Occ -> Occ
    go argOcc =
      let recurArgUses = recurArgUsesFrom argOcc
          st           = execState (analyzeProc recurArgUses top) emptyST
          occ          = stOcc st
          argOcc'      = restrictArgs occ
      in if argOcc' == argOcc then occ else go argOcc'

occOf :: Key -> Occ -> Uses
occOf k occ = M.findWithDefault Zero k occ


-- Tests For Occurence Analysis ------------------------------------------------

test_occ_unused_thunk :: Bool
test_occ_unused_thunk = occOf 1 (occurs proc) == Zero
  where
    proc = mkProc []
      [ LET 1 (DELAY (mkProc [] [ RET (CNS (N 0)) ]))
      , RET (CNS (N 1))
      ]

test_occ_single_force :: Bool
test_occ_single_force = occOf 1 (occurs proc) == Once
  where
    proc = mkProc []
      [ LET 1 (DELAY (mkProc [] [ RET (CNS (N 0)) ]))
      , FORCE 2 (REF 1)
      , RET (REF 2)
      ]

test_occ_double_force :: Bool
test_occ_double_force = occOf 1 (occurs proc) == Many
  where
    proc = mkProc []
      [ LET 1 (DELAY (mkProc [] [ RET (CNS (N 0)) ]))
      , FORCE 2 (REF 1)
      , FORCE 3 (REF 1)
      , RET (REF 3)
      ]

test_occ_force_in_one_branch :: Bool
test_occ_force_in_one_branch = occOf 1 (occurs proc) == Once
  where
    proc = mkProc []
      [ LET 1 (DELAY (mkProc [] [ RET (CNS (N 0)) ]))
      , BR (CNS (N 0))
          [ (2, mkProc [] [ FORCE 3 (REF 1), RET (REF 3) ])
          , (4, mkProc [] [ RET (CNS (N 1)) ])
          ]
      ]

test_occ_force_in_both_branches :: Bool
test_occ_force_in_both_branches = occOf 1 (occurs proc) == Once
  where
    proc = mkProc []
      [ LET 1 (DELAY (mkProc [] [ RET (CNS (N 0)) ]))
      , BR (CNS (N 0))
          [ (2, mkProc [] [ FORCE 3 (REF 1), RET (REF 3) ])
          , (4, mkProc [] [ FORCE 5 (REF 1), RET (REF 5) ])
          ]
      ]

test_occ_escape_via_close :: Bool
test_occ_escape_via_close = occOf 1 (occurs proc) == Many
  where
    proc = mkProc []
      [ LET 1 (DELAY (mkProc [] [ RET (CNS (N 0)) ]))
      , LET 2 (CLOSE (REF 1) [])
      , RET (CNS (N 0))
      ]

test_occ_proc_free_var :: Bool
test_occ_proc_free_var = occOf 1 (occurs proc) == Once
  where
    inner = mkProc [] [ FORCE 3 (REF 1), RET (REF 3) ]

    proc = mkProc []
      [ LET 1 (DELAY (mkProc [] [ RET (CNS (N 0)) ]))
      , PROC 2 inner
      , CALL 4 (LOCAL 2) []
      , RET (REF 4)
      ]

test_occ_proc_not_called :: Bool
test_occ_proc_not_called = occOf 1 (occurs proc) == Zero
  where
    proc = mkProc []
      [ LET 1 (DELAY (mkProc [] [ RET (CNS (N 0)) ]))
      , PROC 2 $ mkProc []
            [ FORCE 3 (REF 1), RET (REF 3) ]
      , RET (CNS (N 0))
      ]

test_occ_self_recur_zero :: Bool
test_occ_self_recur_zero = occOf 0 (occurs proc) == Zero
  where
    proc = mkProc [ARG 0 STRICT]
      [ CALL 1 RECUR [REF 0]
      , RET (CNS (N 0))
      ]

test_occ_recur_amplifies :: Bool
test_occ_recur_amplifies = occOf 0 (occurs proc) == Many
  where
    proc = mkProc [ARG 0 STRICT]
      [ FORCE 1 (REF 0)
      , CALL 2 RECUR [REF 0]
      , RET (REF 1)
      ]

test_occ_nested_thunks :: Bool
test_occ_nested_thunks = and
    [ occOf 1 occ == Once
    , occOf 2 occ == Once
    , occOf 3 occ == Once
    ]
  where
    a = mkProc [] [ RET (CNS (N 0)) ]
    b = mkProc [] [ FORCE 10 (REF 1), RET (REF 10) ]
    c = mkProc [] [ FORCE 11 (REF 2), RET (REF 11) ]

    proc = mkProc []
      [ LET 1 (DELAY a)
      , LET 2 (DELAY b)
      , LET 3 (DELAY c)
      , FORCE 12 (REF 3)
      , RET (REF 12)
      ]

    occ = occurs proc


{-
    DONE: Remove redundant forces.

    DONE: Reorder forces to happen before allocations.

    DONE: Reorder allocations after independant statements.

        If the statement immediatly following an allocation does not
        actually make use of the allocation, then we can swap the order.
        The result is that allocations are floated downwards.  This is
        useful, because we can cluster groups of allocations in order
        to do less GC.

    DONE: Calculate Free Variable for Blocks (and remember them)

    DONE: Calculate Lazy Reference Sets

    DONE: Thunks which are never used lazily become procs.

        If a thunk is passed as an argument to an operation, then it is
        used lazily, otherwise it doesn't need to be a thunk.

    DONE: Lower single-use pure binds into sub-procs.

        If a proc (a thunk or a local procedure) is not used in the main
        control flow, but is instead only used in exactly one sub-proc
        (a thunk or a local procedure), then we should move it's
        definition into the sub-proc.

        Criteria: is this lowerable?

        -    If it is used in the main logical flow (for example, in
            a call or force), then no.  It cannot be lowered.

        -   Next, count the number of procs which reference it.  If the
            count is one, it can be inlined.

        Transform:

        -   Rewrite each statement.

        -   When we find the statement which defines the proc, delete it.

            When we find the sub-proc which references the proc, insert
            the definition into it's code block as the first statment.

    DONE: Remove unused bindings.

        Any binding with zero references should be deleted.

    DONE: Identify blocks with control flow.

        This is very simple: does it end with a switch or a return?

    DONE: Inline procs with one callsite.

        Just count the number of callsites for each proc.

        If the proc is called in tail position, then simply inline it's
        code directly.

        If the proc is called in non-tail position, but it has no
        branching, then inline the code in place, and rewrite the final
        return into a binding (to the variable which was assigned by
        the call).

        If the proc is called in a non-tail position and it has branching,
        then factor out all following statements into a join point
        (continuation producure) (the continuation) and push a call into
        this continuation down into every return point of the proc.
-}

{-
    Lowering Optimzations
    =====================

    DONE: Occurence analysis

        Need occurence info for all call targets.

        Count the number of times a binding is used.  A force is 1 use.
        An CLOSE is 2 uses.  A call depends on the occurence info for
        the call.

    TODO: Identify Tail Calls (easy)

    TODO: Lift local procs.

        Only do this at the very end: lambda lift local procs to make
        codegen simple and easy.
-}

{-
    Bonus Optimizations
    ===================

    TODO: Zero out occurence-zero arguments.

        Any zero-occurence argument should always be replaced with zero
        (since it will never be used, and this will open up more potential
        for dead code elimination).

    TODO: If all branches start with the same operation, lift it out.

        Easy enough, just split off the first operation of each branch,
        if they are all equal, the branches are just the remaining
        operations.  Otherwise non change.

    TODO: Discover strict args to local blocks.

        Identify all cases where we pass a lazy binding to a proc.
        If we do, then the argument is strict, otherwise it is lazy.

        TODO: we can float allocations down after other operations
        too, if the other operation doesn't depend on the allocation.
        Generally, we want all allocations to end up in big clusters,
        and to happen as late as possible.
-}
