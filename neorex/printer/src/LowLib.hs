-- Copyright (c) 2026 xoCore Technologies
-- SPDX-License-Identifier: MIT
-- See LICENSE for full terms.
{-# LANGUAGE BangPatterns, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
-- {-# OPTIONS_GHC -Werror -Wno-deprecations -Wno-unused-top-binds -Wno-unused-imports #-}

module LowLib
    ( someFunc
    ) where

import Prelude

import Text.Show.Pretty (ppShow)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Data.List.NonEmpty as NonEmpty

import Data.String
import Control.Monad      (replicateM_)
import Data.Foldable      (all, toList)
import Data.Functor       ((<&>))
import Data.List.Extra    (splitOn)
import Data.List.NonEmpty (NonEmpty(..), singleton)
import Optics             (_2, over)


-- Printing Combinators --------------------------------------------------------

spaces :: Int -> Doc
spaces n = if n <= 0 then mempty else text (replicate n ' ')

offset False x = x
offset True  x = text "  " <> x

backstep :: [Doc] -> Doc
backstep []     = mempty
backstep (x:xs) = align (spaces (2 * length xs) <> x <> go (reverse xs))
  where
    go []     = mempty
    go [x]    = line <> x
    go (x:xs) = nest 2 (go xs) <> line <> x


-- Clumps ----------------------------------------------------------------------

data ClumpWord -- Anything but a word comes after a word.
    = CW_ String
    | CWT String ClumpText
    | CWQ String ClumpQuip
    | CWP String ClumpPage
    | CWS String ClumpSlug
    | CWR String ClumpRune
    | CWN String ClumpNest

data ClumpText -- Anything but text comes after text.
    = CT_ String
    | CTW String ClumpWord
    | CTQ String ClumpQuip
    | CTP String ClumpPage
    | CTS String ClumpSlug
    | CTR String ClumpRune
    | CTN String ClumpNest

data ClumpQuip -- Only quips and slugs can come after quips.
    = CQ_  String
    | CQQ  String ClumpQuip
    | CQS  String ClumpSlug
    | CQRQ String Rune ClumpQuip -- TODO: Only if it's not a runequip.
    | CQRS String Rune ClumpSlug -- TODO: Only if it's not a runequip.

data ClumpPage -- Anything comes after a page.
    = CP_ String
    | CPC String Clump

data ClumpSlug -- Nothing comes after a slug.
    = CS_ String

-- A rune cannot come after a rune, and a rune cannot end a clump.
data ClumpRune
    = CRW Rune ClumpWord
    | CRT Rune ClumpText
    | CRQ Rune ClumpQuip
    | CRP Rune ClumpPage
    | CRS Rune ClumpSlug
    | CRN Rune ClumpNest

data ClumpNest -- Anything can come after a nest.
    = CN_ Clump
    | CNC Clump Clump

data Clump
    = CW ClumpWord
    | CT ClumpText
    | CQ ClumpQuip
    | CP ClumpPage
    | CS ClumpSlug
    | CR ClumpRune
    | CN ClumpNest


-- Untyped Clumps --------------------------------------------------------------

type RawClump = [RawClumpItem]

data RawClumpItem
    = RCW String
    | RCT String
    | RCQ String
    | RCP String
    | RCS String
    | RCR Rune
    | RCN RawClump

class Unclump a where
    unclump :: a -> RawClump

instance Unclump ClumpWord where
    unclump (CW_ w)   = RCW w : []
    unclump (CWT w t) = RCW w : unclump t
    unclump (CWQ w q) = RCW w : unclump q
    unclump (CWP w p) = RCW w : unclump p
    unclump (CWS w s) = RCW w : unclump s
    unclump (CWR w r) = RCW w : unclump r
    unclump (CWN w n) = RCW w : unclump n

instance Unclump ClumpText where
    unclump (CT_ t)   = RCT t : []
    unclump (CTW t w) = RCT t : unclump w
    unclump (CTQ t q) = RCT t : unclump q
    unclump (CTP t p) = RCT t : unclump p
    unclump (CTS t s) = RCT t : unclump s
    unclump (CTR t r) = RCT t : unclump r
    unclump (CTN t n) = RCT t : unclump n

instance Unclump ClumpQuip where
    unclump (CQ_  q)      = RCQ q : []
    unclump (CQQ  q q')   = RCQ q : unclump q'
    unclump (CQS  q s)    = RCQ q : unclump s
    unclump (CQRQ q r q') = RCQ q : RCR r : unclump q'
    unclump (CQRS q r s)  = RCQ q : RCR r : unclump s

instance Unclump ClumpPage where
    unclump (CP_ p)   = RCP p : []
    unclump (CPC p c) = RCP p : unclump c

instance Unclump ClumpNest where
    unclump (CN_ n)   = RCN (unclump n) : []
    unclump (CNC n m) = RCN (unclump n) : unclump m

instance Unclump ClumpSlug where
    unclump (CS_ s) = RCS s : []

instance Unclump Clump where
    unclump (CW w) = unclump w
    unclump (CT t) = unclump t
    unclump (CQ q) = unclump q
    unclump (CP p) = unclump p
    unclump (CS s) = unclump s
    unclump (CR r) = unclump r
    unclump (CN n) = unclump n

instance Unclump ClumpRune where
    unclump (CRW r w) = RCR r : []
    unclump (CRT r t) = RCR r : unclump t
    unclump (CRQ r q) = RCR r : unclump q
    unclump (CRP r p) = RCR r : unclump p
    unclump (CRS r s) = RCR r : unclump s
    unclump (CRN r n) = RCR r : unclump n

renderClump :: Clump -> Doc
renderClump = fs . unclump
  where
    fs :: RawClump -> Doc
    fs = mconcat . fmap f

    f :: RawClumpItem -> Doc
    f = \case
        RCW w -> text w
        RCT t -> char '"' <> text t <> char '"'
        RCQ q -> char '\'' <> text q
        RCP p -> string "'''" <> text p <> string "'''"
        RCS s -> string "' " <> text s
        RCR r -> pRune r
        RCN n -> char '(' <> fs n <> char ')'


-- Types -----------------------------------------------------------------------

type Render = RunePoem

newtype Rune = RUNE String
  deriving newtype IsString

data RunePoem = RP (NonEmpty (Rune, [PoemLine])) (Maybe Closed)

data PoemLine
    = PL_SLIME String
    | PL_WORDS (NonEmpty Closed)
    | PL_BLOCK RunePoem

-- TODO: make this possible:
--
--     hi' slug
--

-- Very ugly types for encoding juxtaposition invariants.

data WordTail = W | WT TextAnd | WQ QuipAnd | WP PageAnd | WN Closed
data TextTail = T | TW WordAnd | TQ QuipAnd | TP PageAnd | TN Closed
data PageTail = P | PP Closed
data QuipTail = Q | QQ QuipAnd
data SlugTail = S

type WordAnd = (String, WordTail)
type TextAnd = (String, TextTail)
type QuipAnd = (String, QuipTail)
type PageAnd = (String, PageTail)
type IFixAnd = IFix
type SlugAnd = (String, SlugTail)

-- Can't put an infix rune after (an unwrapped) slug or quip.
--
-- TODO Except for this pattern: 'foo.'bar
-- TODO And this: 'foo'bar.'foo'bar

-- TODO What about bloodlines between runes:
--
--     a"foo".x
--     a('quip).x
--     a('quip).x
--
-- They can never contain a quip or a slug.  They can't contain infix
-- forms nor prefix forms.  Otherwise, they have the same rules as
-- usual heir-sequences.

data BeforeRuneAnd
    = BR_WORD BrWordAnd
    | BR_TEXT BrTextAnd
    | BR_PAGE BrPageAnd
    | BR_NEST BrNestAnd

data BrWordTail = BRW                  | BRWT BrTextAnd | BRWP BrPageAnd
data BrTextTail = BRT | BRTW BrWordAnd                  | BRTP BrPageAnd
data BrPageTail = BRP | BRPW BrWordAnd | BRPT BrTextAnd | BRPP BrPageAnd
data BrNestTail = BRN | BRNN BrNestAnd

type BrWordAnd = (String, BrWordTail)
type BrTextAnd = (String, BrTextTail)
type BrPageAnd = (String, BrPageTail)
type BrNestAnd = (Closed, BrNestTail)

type IFix = (BeforeRuneAnd, Rune, AfterRune)

data AfterRune
    = AR_WORD WordAnd
    | AR_TEXT TextAnd
    | AR_QUIP QuipAnd
    | AR_SLUG SlugAnd
    | AR_PAGE PageAnd
    | AR_IFIX IFixAnd
    | AR_NEST Closed

data Closed
    = WORD WordAnd
    | TEXT TextAnd
    | QUIP QuipAnd
    | SLUG SlugAnd
    | PAGE PageAnd
    | PREF Rune AfterRune
    | IFIX IFix


-- Rendering Leaves ------------------------------------------------------------

pRune :: Rune -> Doc
pRune (RUNE x) = text x -- placeholder

renderSlug [] = text "'"
renderSlug xs = text ("' " <> xs) -- placeholder


-- Rendering Closed Forms ------------------------------------------------------

-- A closed form is one or more closed items.

pWordTail :: WordTail -> Doc
pWordTail W      = mempty
pWordTail (WT t) = pClosed (TEXT t)

pTextTail :: TextTail -> Doc
pTextTail T      = mempty
pTextTail (TW t) = pClosed (WORD t)

pAfterRune :: AfterRune -> Doc
pAfterRune = \case
    AR_WORD wa -> pClosed (WORD wa)
    AR_TEXT ta -> pClosed (TEXT ta)
    AR_QUIP qa -> pClosed (QUIP qa)
    AR_SLUG sa -> pClosed (SLUG sa)
    AR_PAGE pa -> pClosed (PAGE pa)
    AR_IFIX ix -> pClosed (IFIX ix)
    AR_NEST it -> char '(' <> pClosed it <> char ')'

pBrWordTail :: BrWordTail -> Doc
pBrWordTail BRW      = mempty
pBrWordTail (BRWT t) = pBrText t

pBrTextTail :: BrTextTail -> Doc
pBrTextTail BRT      = mempty
pBrTextTail (BRTW t) = pBrWord t

pBrWord :: BrWordAnd -> Doc
pBrWord (w, t) = pWord w <> pBrWordTail t

pBrText :: BrTextAnd -> Doc
pBrText (w, t) = pText w <> pBrTextTail t

pWord = text

pText t = char '"' <> text t <> char '"'

pClosed :: Closed -> Doc
pClosed (WORD (w, wt))            = text w <> pWordTail wt
pClosed (TEXT (t, tt))            = pText t <> pTextTail tt
pClosed (IFIX (BR_WORD w, r, ar)) = pBrWord w <> pRune r <> pAfterRune ar

-- Rendering Rune Poems --------------------------------------------------------

pPoem :: RunePoem -> Doc
pPoem (RP ls k) =
    vsep ((pPara <$> layoutPoem (toList ls)) <> (pClosed <$> toList k))
  where
    pLine :: (Bool, PoemLine) -> Doc
    pLine (o, pl) = offset o $ case pl of
                       PL_SLIME s  -> renderSlug s
                       PL_WORDS ws -> hsep $ fmap pClosed $ toList ws
                       PL_BLOCK pm -> pPoem pm

    pPara :: (Rune, [[(Bool, PoemLine)]]) -> Doc
    pPara (rune, bs) =
        if null bs then pRune rune else
        pRune rune <> space <> backstep (align . vsep . fmap pLine <$> bs)

    chunkPoem :: [PoemLine] -> [PoemLine] -> [[PoemLine]]
    chunkPoem acc = \case
        []                -> if null acc then [] else [reverse acc]
        p@PL_BLOCK{} : ps -> (reverse (p:acc)) : chunkPoem [] ps
        p:ps              -> chunkPoem (p:acc) ps

    slugJog :: [PoemLine] -> [(Bool, PoemLine)]
    slugJog = \case
        []                              -> []
        [x]                             -> [(False, x)]
        x@PL_SLIME{} : y@PL_SLIME{} : z -> (False,x) : (True,y) : slugJog z
        x:xs                            -> (False,x) : slugJog xs

    layoutPoem :: [(Rune, [PoemLine])] -> [(Rune, [[(Bool, PoemLine)]])]
    layoutPoem = fmap $ over _2 (fmap slugJog . chunkPoem [])


-- Testing----------------------------------------------------------------------

rp :: [(Rune, [PoemLine])] -> RunePoem
rp []     = error "bad rp"
rp (x:xs) = RP (x :| xs) Nothing

rph :: [(Rune, [PoemLine])] -> Closed -> RunePoem
rph []     _    = error "bad rp"
rph (x:xs) heir = RP (x :| xs) (Just heir)

hiLine :: PoemLine
hiLine = PL_WORDS $ singleton $ txt "hi"

tinLine :: PoemLine
tinLine = PL_WORDS $ singleton $ IFIX $ ( BR_WORD ("tin", BRWT ("can", BRT))
                                        , "."
                                        , AR_TEXT ("say", TW ("hi", W))
                                        )

word, txt :: String -> Closed
word s = WORD (s, W)
txt s  = TEXT (s, T)

blockLine :: Int -> PoemLine
blockLine n = PL_WORDS (word ("BLOCK" <> show n) :| [])

trivialPoem :: Int -> RunePoem
trivialPoem n = rp [("|", [blockLine n])]

examplePoem :: RunePoem
examplePoem = rph items heir
  where
    items =
        [ ("|", [ hiLine
                , PL_BLOCK (trivialPoem 1)
                , PL_SLIME "First slug."
                , PL_SLIME "Second slug."
                , PL_SLIME "Third slug."
                , PL_SLIME "Fourth slug."
                , PL_BLOCK (trivialPoem 2)
                , hiLine
                ])
        , ("|", [])
        , ("|", [])
        , ("|", [tinLine])
        ]

    heir =
        (WORD ("heir", WT ("yo", T)))

someFunc :: IO ()
someFunc = do
    if True
    then do
        let try x = do putDoc (indent 4 $ renderClump x)
                       replicateM_ 2 (putStrLn "")
        try $ CW (CWT "hi" $ CT_ "hallo")
        try $ CQ (CQ_ "html[]{}")
        try $ CQ (CQRQ "html[]{}" "." $ CQ_ "asdf")
        try $ CR (CRQ "~" $ CQRQ "foo" "," $ CQRS "bar" "," $ CS_ "slug")
    else do
        putDoc $ indent 4 $ pPoem examplePoem

        putStrLn "" >> putStrLn ""

        putDoc $ indent 4 $ text "hi " <> backstep [text "a", text "b", text "c"]

        putStrLn "" >> putStrLn ""
        putStrLn "Hello" >> putStrLn "World"
