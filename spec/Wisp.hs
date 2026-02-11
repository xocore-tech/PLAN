-- Copyright (c) 2026 xoCore Technologies
-- SPDX-License-Identifier: MIT
-- See LICENSE for full terms.
{-# LANGUAGE LambdaCase, ViewPatterns, BlockArguments #-}

module Wisp where

import System.Environment (getArgs)
import Data.Char (isAlphaNum, isDigit, ord, chr)
import Plan
import Data.Foldable (traverse_)
import Control.Monad (when)
import Numeric.Natural (Natural)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (pPrint)

strNat :: [Char] -> Natural
strNat []     = 0
strNat (c:cs) = fromIntegral (ord c) + (256 * strNat cs)

curlNat, pinNat, brakNat, defNat, macNat, lawNat, evalNat, atNat :: Natural
curlNat = strNat "CURL"
brakNat = strNat "BRAK"
pinNat  = strNat "PIN"
defNat  = strNat "DEF"
macNat  = strNat "MAC"
lawNat  = strNat "LAW"
evalNat = strNat "EVAL"
atNat   = strNat "@"

symE, strE :: String -> Val
symE s   = N (strNat s)
strE s   = N 1 `A` N (strNat s)

natE :: Natural -> Val
natE n   = N 1 `A` N n

listE, curlE, brakE :: [Val] -> Val
listE xs = array xs
curlE xs = array [N curlNat, listE xs]
brakE xs = array [N brakNat, listE xs]

data CharType = GAP | SYM | STR | END | NEST ([Val] -> Val) Char

cat :: Char -> CharType
cat = \case
    { '('  -> NEST listE ')' ; '[' -> NEST brakE ']' ; '{' -> NEST curlE '}'
    ; ')'  -> END            ; ']' -> END            ; '}' -> END
    ; '\n' -> GAP            ; ' ' -> GAP            ; ';' -> GAP
    ; '"'  -> STR            ; _   -> SYM
    }

eat :: String -> String
eat (';'  : cs) = eat (dropWhile (/= '\n') cs)
eat (' '  : cs) = eat cs
eat ('\n' : cs) = eat cs
eat cs          = cs

parse :: String -> (Val, String)
parse s0 =
    let c:cs = eat s0 in
    case cat c of
        STR         -> case break (=='"') cs of (body, '"':r) -> (strE body, r)
        NEST mk end -> pseq mk end cs
        SYM         -> parseSymbol (c:cs)
  where
    parseSymbol xs = let (s, r) = span isSymChar xs in
                     let v = if all isDigit s then natE (read s) else symE s in
                     case r of
                         (cat -> NEST mk end):ys -> let (i,s3) = pseq mk end ys
                                                    in (listE [v, i], s3)
                         _                       -> (v, r)

    isSymChar c = case cat c of SYM -> True; _ -> False

pseq :: ([Val] -> Val) -> Char -> String -> (Val, String)
pseq mk end str = go [] (' ' : str)
  where
    go :: [Val] -> String -> (Val, String)
    go _   []                  = error "eof in list"
    go acc (c:r) | c==end      = (mk (reverse acc), r)
    go _   (c:_) | not (gap c) = error "bad list"
    go acc cs                  = case eat cs of
        (c:r) | c==end -> (mk (reverse acc), r)
        xs2            -> go (a:acc) xs3 where (a, xs3) = parse xs2

    gap :: Char -> Bool
    gap c = c == ' ' || c == '\n' || c == ';'

parseMany :: String -> [Val]
parseMany s = case eat s of
    [] -> []
    xs -> n : parseMany rest where (n, rest) = parse xs

vEnv :: IORef Val
vEnv = unsafePerformIO (newIORef (N 0))

getenvIO :: Natural -> IO (Maybe (Bool, Val))
getenvIO key = getenv key <$> readIORef vEnv

getenv :: Natural -> Val -> Maybe (Bool, Val)
getenv _ (N 0) = Nothing
getenv key env = case aSeq env of
    [N 0, N k, v, isMacro, l, r] ->
        case compare key k of
            LT -> getenv key l
            EQ -> Just (isMacro /= N 0, v)
            GT -> getenv key r
    _ ->
        Nothing -- TODO: be more lenient

putEnvIO :: Natural -> Val -> Bool -> IO ()
putEnvIO key val isMacro = modifyIORef' vEnv (putenv key val isMacro)

putenv :: Natural -> Val -> Bool -> Val -> Val
putenv key val isMac env =
  case env of
    N 0 -> node key val (flag isMac) (N 0) (N 0)
    _   -> case aSeq env of
      [N 0, N k, v, m, l, r] ->
        case compare key k of
          LT -> node k v m (putenv key val isMac l) r
          EQ -> node k val (flag isMac) l r
          GT -> node k v m l (putenv key val isMac r)
      _ -> error ("putenv: bad env node: " <> show env)
  where
    flag bit = N (if bit then 1 else 0)
    node k v m l r = N 0 `A` N k `A` v `A` m `A` l `A` r

aSeq :: Val -> [Val]
aSeq = go [] where go acc = \case { A f x -> go (x:acc) f; x -> x:acc }

eval :: Val -> IO Val
eval top = macroexpand [] top >>= go
  where
    go (N 0)       = pure (N 0)
    go (A (N 1) x) = pure x
    go (N n)       = getenvIO n >>= \case
                       Just (_, v) -> pure v
                       _           -> error ("unbound: " <> show n)
    go x           = fmap apple $ traverse go $ listElems x

data Macro = EVAL | PIN | LAW | DEF | MAC | USER Val

expand1 :: Macro -> Val -> IO Val
expand1 mac x = case (mac, listElems x) of
    (EVAL, [_, v])     -> A (N 1) <$> eval v
    (PIN, [_, v])      -> A (N 1) . P <$> eval v
    (LAW, _:args)      -> A (N 1)  <$> lawExp args
    (DEF, [_, N n, e]) -> bind n e False
    (MAC, [_, N n, e]) -> bind n e True
    (USER macVal, _) -> do
        env <- readIORef vEnv
        case aSeq (macVal % env % x) of
            [N 0, e2, out] -> writeIORef vEnv e2 >> pure out
            _              -> error "bad macro output"
  where
    bind nm e isMacro = do
        val <- eval e
        putEnvIO nm val isMacro
        pure (N 1 `A` val)

macroexpand :: Locals -> Val -> IO Val
macroexpand loc = go
  where
    go v = case aSeq v of
        N 0 : xs -> getmacro xs >>= \case
                        Nothing  -> array <$> traverse go xs
                        Just mac -> expand1 mac v >>= go
        _        -> pure v

    getmacro = \case N s : _ -> sym s; _ -> pure Nothing

    sym s = do
        env <- readIORef vEnv
        pure $ case (lookup s loc, getenv s env) of
            (Just{}, _)               -> Nothing
            (_, Just (True, v))       -> Just (USER v)
            (_, Just (False, _))      -> Nothing
            (_, Nothing) | s==pinNat  -> Just PIN
            (_, Nothing) | s==lawNat  -> Just LAW
            (_, Nothing) | s==evalNat -> Just EVAL
            (_, Nothing) | s==macNat  -> Just MAC
            (_, Nothing) | s==defNat  -> Just DEF
            (_, Nothing) | otherwise  -> Nothing

array :: [Val] -> Val
array xs = foldl A (N 0) xs

apple :: [Val] -> Val
apple [] = N 0
apple [a] = a
apple (x:y:z) = apple ((x%y):z)

type Locals = [(Natural, Natural)] -- (sym -> refIndex)

lawExp :: [Val] -> IO Val
lawExp []                = error "law: bad form"
lawExp (sigForm : forms) = do
    let (bodySrc, bindForms) = case reverse forms of
            body : revBinds -> (body, reverse revBinds)
            _               -> error "law: missing body"

    (nm, tag, argSyms) <- parseArgList sigForm
    binds              <- traverse parseBind bindForms

    let nArgs  = length argSyms
    let locals = buildLocals nm argSyms (map fst binds)

    when (nArgs==0) $ do error "empty argument list"

    bindExps <- traverse (macroexpand locals) (snd <$> binds)
    bodyExp  <- macroexpand locals bodySrc
    bindIRs  <- traverse (compileExpr locals) bindExps
    bodyIR   <- compileExpr locals bodyExp

    pure $ L (fromIntegral nArgs) (N tag)
         $ foldr (\v k -> N 1 `A` v `A` k) bodyIR bindIRs
  where
    buildLocals :: Natural -> [Natural] -> [Natural] -> Locals
    buildLocals self args binds =
      (self, 0)
      : zip args  [1..]
     ++ zip binds [fromIntegral (length args) + 1 ..]

    parseBind v = case listElems v of
        [N tag, N nm, expr] | tag==atNat -> pure (nm, expr)
        _ -> error ("law: bad bind (expected (@ name expr)): " <> show v)

    parseArgList :: Val -> IO (Natural, Natural, [Natural])
    parseArgList v = case listElems v of
        N s           : args -> pure (s, s, expectSym <$> args)
        A (N 1) (N s) : args -> pure (0, s, expectSym <$> args)
        _                    -> error ("law: bad arg list: " <> show v)
      where
        expectSym (N s) = s
        expectSym x     = error ("law: expected symbol in arg list: " <> show x)

listElems :: Val -> [Val]
listElems v = case aSeq v of
    N 0 : xs -> xs
    _        -> error ("expected list: " <> show v)

compileExpr :: Locals -> Val -> IO Val
compileExpr locals v = case v of
    A (N 1) x -> pure (N 0 `A` x)
    N s       -> case lookup s locals of
        Just ix -> pure (N ix)
        Nothing -> getenvIO s >>= \case
            Just (False, gv) -> pure (N 0 `A` gv)   -- embed as constant
            Just (True,  _)  -> error ("law: global is a macro (not allowed in law body): " <> show s)
            Nothing          -> error ("law: unbound global: " <> show s)

    _ -> do let (f:as) = listElems v
            f'  <- compileExpr locals f
            as' <- traverse (compileExpr locals) as
            pure (foldl (\acc x -> N 0 `A` acc `A` x) f' as')

main :: IO ()
main = getArgs >>= \case
    ident:_ -> loadFile ident >>= traverse_ \node ->
                 macroexpand [] (listE [symE "EVAL", node]) >>= pPrint
  where
    loadFile nm = do
        when (null nm || any (not . okFileChar) nm) $ do
            error "bad path"
        forms <- parseMany <$> readFile ("wisp/" <> nm <> ".lisp")
        case forms of
            (readIncl -> Just inc) : more -> (<> more) <$> loadFile inc
            _                             -> pure forms

    okFileChar c = isAlphaNum c || c `elem` "_-"

    readIncl (N x) = case explode x of '<':cs -> Just cs; _ -> Nothing
    readIncl _     = Nothing

    explode 0 = []
    explode c = chr (fromIntegral c `mod` 256) : explode (c `div` 256)
