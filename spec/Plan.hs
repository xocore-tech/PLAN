-- Copyright (c) 2026 xoCore Technologies
-- SPDX-License-Identifier: MIT
-- See LICENSE for full terms.
module Plan where
import Numeric.Natural

data Val = P !Val | L !Natural Val Val | A !Val Val | N !Natural
  deriving (Eq, Show)

nat x = case x of N n -> n; _ -> 0

arity (A f _)       = if arity f == 0 then 0 else arity f - 1
arity (P (L a _ _)) = a
arity (L a _ _)     = a
arity (N _)         = 0
arity (P _)         = 1

match p _ _ _ _ (P i)     = p % i
match _ l _ _ _ (L a m b) = l % N a % m % b
match _ _ a _ _ (A x e)   = a % x % e
match _ _ _ z m (N o)     = if o==0 then z else m % N (o-1)

(%) f x = if arity f /= 1 then A f x else exec f [x]

exec (P (N o))       [x] = op o x
exec (A f x)         e   = exec f (x:e)
exec f@(L a m b)     e   = judge a (reverse (f:e)) b
exec f@(P (L a m b)) e   = judge a (reverse (f:e)) b
exec (P x)           e   = error $ show ("running bad pin", x, e)

op 0 n                                         = P n
op 1 (N 0 `A` a `A` m `A` b)                   = L (nat a + 1) m b
op 2 (N 0 `A` p `A` l `A` a `A` z `A` m `A` o) = match p l a z m o

kal n e (N b) | b<=n      = e !! fromIntegral (n-b)
kal n e (N 0 `A` f `A` x) = (kal n e f % kal n e x)
kal _ _ (N 0 `A` x)       = x
kal _ _ x                 = x

judge args ie body = res
  where (n, e, res::Val) = go args ie body
        go i acc (N 1 `A` v `A` k) = go (i+1) (kal n e v : acc) k
        go i acc x                 = (i, acc, kal n e x)
