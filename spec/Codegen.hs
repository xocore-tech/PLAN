-- Copyright (c) 2026 xoCore Technologies
-- SPDX-License-Identifier: MIT
-- See LICENSE for full terms.
module Codegen where

import Plan
import PlanGrm
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Word

--------------------------------------------------------------------------------

type ThunkLabel = Int
type BlockLabel = Int
type Reg        = Int
type Offset     = Int

data UpdateFlag = UPDATE | NO_UPDATE

type LBlock = [LStmt]

data LProc = LProc
    { lprocName :: BlockLabel
    , lprocArgs :: [Reg]
    , lprocBody :: LBlock
    }

data LDelay = LThunk
    { ldelayName   :: ThunkLabel
    , ldelaySelf   :: Reg
    , ldelayBody   :: LBlock
    , ldelayUpdate :: UpdateFlag
    }

data LAlloc
    = L_PAP Reg Val [Reg]
    | L_DELAY Reg ThunkLabel [Reg]

data LStmt
    = L_MOVE Reg Val
    | L_ALLOC Reg Word
    | L_STOREV Reg Offset Reg
    | L_STOREW Reg Offset Word
    | L_FORCE Reg Reg
    | L_BRANCH Reg [LBlock]
    | L_RETURN Reg
    | L_CALL_PROC Reg BlockLabel [Reg]
    | L_TAILCALL_PROC Reg BlockLabel [Reg]
    | L_CALL_KNOWN Val [Reg]
    | L_TAILCALL_KNOWN Val [Reg]
    | L_CALL_UNKNOWN Reg [Reg]
    | L_TAILCALL_UNKNOWN Reg [Reg]

data Program = PROGRAM
    { thunks :: M.Map ThunkLabel LDelay
    , procs  :: M.Map BlockLabel LProc
    }


-- Printer ---------------------------------------------------------------------

ppProgram :: Program -> String
ppProgram (PROGRAM ths ps) =
    unlines $ concatMap concat
      [ [["PROGRAM"]]
      , ppLThunk 1 <$> M.elems ths
      , ppLProc  1 <$> M.elems ps
      ]

ppLProc :: Int -> LProc -> [String]
ppLProc i (LProc name args body) =
    (indent i ++ "proc " ++ show name ++ "(" ++ comma args ++ ") =")
    : ppBlock (i + 1) body

ppLThunk :: Int -> LDelay -> [String]
ppLThunk i (LThunk name self body upd) =
    (indent i ++ "thunk " ++ show name
               ++ "(" ++ show self ++ ")"
               ++ showUpd upd ++ " =")
    : ppBlock (i + 1) body
  where
    showUpd UPDATE    = " UP"
    showUpd NO_UPDATE = ""

ppBlock :: Int -> LBlock -> [String]
ppBlock i = concatMap (ppLStmt i)

ppLStmt :: Int -> LStmt -> [String]
ppLStmt i stmt = case stmt of
    L_MOVE r v ->
        [ indent i ++ show r ++ " = " ++ show v ]

    L_ALLOC r z -> undefined r z

    L_FORCE dst src ->
        [ indent i ++ show dst ++ " = force " ++ show src ]

    L_RETURN r ->
        [ indent i ++ "return " ++ show r ]

    L_BRANCH r blocks ->
        (indent i ++ "branch " ++ show r ++ " {")
        : concatMap (ppLBranch (i + 1)) (zip [0 :: Int ..] blocks)
       ++ [ indent i ++ "}" ]

    L_CALL_PROC dst f xs ->
        [ indent i
            ++ show dst ++ " = call_proc "
            ++ show f ++ "(" ++ comma xs ++ ")"
        ]

    L_TAILCALL_PROC dst f xs ->
        [ indent i
            ++ "tailcall_proc "
            ++ show f ++ "(" ++ comma xs ++ ") -> " ++ show dst
        ]

    L_CALL_KNOWN v xs ->
        [ indent i
            ++ "call_known "
            ++ show v ++ "(" ++ comma xs ++ ")"
        ]

    L_TAILCALL_KNOWN v xs ->
        [ indent i
            ++ "tailcall_known "
            ++ show v ++ "(" ++ comma xs ++ ")"
        ]

    L_CALL_UNKNOWN f xs ->
        [ indent i
            ++ "call_unknown "
            ++ show f ++ "(" ++ comma xs ++ ")"
        ]

    L_TAILCALL_UNKNOWN f xs ->
        [ indent i
            ++ "tailcall_unknown "
            ++ show f ++ "(" ++ comma xs ++ ")"
        ]

ppLAlloc :: Int -> LAlloc -> [String]
ppLAlloc i alloc = case alloc of
    L_PAP r v xs ->
        [ indent i
            ++ show r ++ " = pap "
            ++ show v ++ "(" ++ comma xs ++ ")"
        ]

    L_DELAY r l xs ->
        [ indent i
            ++ show r ++ " = delay "
            ++ show l ++ "(" ++ comma xs ++ ")"
        ]

ppLBranch :: Int -> (Int, LBlock) -> [String]
ppLBranch i (n, blk) =
    (indent i ++ "case " ++ show n ++ " ->")
    : ppBlock (i + 1) blk

comma :: Show a => [a] -> String
comma = concat . L.intersperse ", " . map show
