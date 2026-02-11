-- Copyright (c) 2026 xoCore Technologies
-- SPDX-License-Identifier: MIT
-- See LICENSE for full terms.
{-# LANGUAGE BangPatterns, LambdaCase #-}
{-# OPTIONS_GHC -Werror -Wno-deprecations -Wno-unused-top-binds -Wno-unused-imports #-}

{-
    TODO: Minimize dedents when using slugs (and instead use jogging)

        | a
          ' slug 1
          ' slug 1
            ' slug 2
            ' slug 2
          ' slug 3
          ' slug 3
          b
          ' slug 4
          ' slug 4

    TODO: Wide-mode infix.
    TODO: Tall-mode infix.

    TODO: Tight prefix.
    TODO: Tight infix.

    ( 3 + 4
    + 5 + 6
    + 7 + 8
    )
-}

module Lib
    ( someFunc
    , word, txt, quip, slug, page
    ) where

import Prelude

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Data.List.NonEmpty as NonEmpty

import Data.Foldable      (all, toList)
import Data.List.Extra    (splitOn)
import Data.List.NonEmpty (NonEmpty(..), singleton)
import Data.Functor       ((<&>))

-- Rex -------------------------------------------------------------------------

data Color = PAREN | CURLY | BRACK | CLEAR | TIGHT
data Shape = INFIX | PREFIX

data Style = WORD | TEXT | QUIP | PAGE | SLUG

newtype Rex = REX (NonEmpty Item)

data Item
    = LEAF Style String
    | NODE Shape Color String [Rex]


-- Pre-Process -----------------------------------------------------------------

cleanItem :: Item -> Item
cleanItem i = case i of
    NODE shape  color rune xs  -> NODE shape color
                                    (cleanTick shape color rune xs)
                                    (cleanRex <$> xs)
    LEAF _ _                  -> i
  where
    cleanTick PREFIX PAREN "`"  [_] = "`"
    cleanTick PREFIX PAREN "`"  _   = ""
    cleanTick PREFIX BRACK "`"  _   = ""
    cleanTick PREFIX CURLY "`"  _   = ""
    cleanTick _      _     rune _   = rune

data UnwrapDirection = LEFT | RIGHT | BOTH | CHOICE | NONE

--      w- | || && && && &&
--      t- | && || && && &&
--      q- | >> >> && && &&
--      u- | && && || || ||
--      s- | >> >> >> >> >>

-- TODO: There are pobably mistakes here!
wrapDir :: Item -> Item -> UnwrapDirection
wrapDir (LEAF WORD _) (LEAF WORD _) = CHOICE
wrapDir (LEAF WORD _) LEAF{}        = NONE
wrapDir (LEAF WORD _) NODE{}        = RIGHT

wrapDir (LEAF TEXT _) (LEAF TEXT _) = CHOICE
wrapDir (LEAF TEXT _) LEAF{}        = NONE
wrapDir (LEAF TEXT _) NODE{}        = RIGHT

wrapDir (LEAF QUIP _) (LEAF WORD _) = LEFT -- quips are hungry
wrapDir (LEAF QUIP _) (LEAF TEXT _) = LEFT
wrapDir (LEAF QUIP _) NODE{}        = BOTH
wrapDir (LEAF QUIP _) LEAF{}        = NONE -- but they wont eat quip/page/slug

wrapDir (LEAF PAGE _) LEAF{}        = NONE -- pages don't conflict.
wrapDir (LEAF PAGE _) NODE{}        = RIGHT -- pages don't conflict.

wrapDir (LEAF SLUG _) LEAF{}        = LEFT -- slug juxt must always be wrapped
wrapDir (LEAF SLUG _) NODE{}        = BOTH -- slug juxt must always be wrapped

wrapDir NODE{} NODE{} = BOTH
wrapDir NODE{} LEAF{} = LEFT

-- TODO: tight infix

cleanRex :: Rex -> Rex
cleanRex = \(REX xs) -> REX (wrapSeq $ cleanItem <$> xs)
  where
    wrapSeq :: NonEmpty Item -> NonEmpty Item
    wrapSeq items@(x :| xs) =
        case xs of
            []   -> items
            y:ys -> let (x', y') = case wrapDir x y of
                          NONE   -> (x, y)
                          RIGHT  -> (x, wrap y)
                          LEFT   -> (wrap x, y)
                          BOTH   -> (wrap x, wrap y)
                          CHOICE -> (x, wrap y) -- TODO
                    in NonEmpty.cons x' (wrapSeq (y' :| ys))

wrapInitRune :: Rex -> Rex
wrapInitRune (REX (item@(NODE PREFIX TIGHT _ _) :| [])) = REX (wrap item :| [])
wrapInitRune rex                                        = rex

wrap :: Item -> Item
wrap i@(NODE _  PAREN _ _) = i
wrap i@(NODE _  BRACK _ _) = i
wrap i@(NODE _  CURLY _ _) = i
wrap i@(NODE _  CLEAR _ _) = i -- NODE sh PAREN y s -- TODO special case for `
wrap i@(NODE _ TIGHT _ _)  = NODE PREFIX PAREN "" [REX (i :| [])]
wrap i@LEAF{}              = NODE PREFIX PAREN "" [REX (i :| [])]


-- Printing Leaves -------------------------------------------------------------

prettyLeaf :: Style -> String -> Doc
prettyLeaf s t = case s of
    TEXT -> green (prettyText t)
    PAGE -> bold $ yellow (prettyPage t)
    SLUG -> bold $ cyan  (prettySlug t)
    -- D -> underline (text t)
    WORD -> text t
    QUIP -> bold $ magenta (prettyQuip t)
  where
    escaped :: String -> String
    escaped []              = []
    escaped (x:xs) | x=='"' = '"' : '"' : escaped xs
    escaped (x:xs)          = x : escaped xs

    prettyText = hang 1 . dquotes . vsep . fmap (text . escaped) . splitOn "\n"

    prettyQuip = align . vsep . fmap text . splitOn "\n"

    prettySlug input = align (vsep $ fmap slugLine $ splitOn "\n" input)

    slugLine ""  = text "'"
    slugLine abc = text ("' " <> abc)

    delimSz !acc !this []        = max acc this
    delimSz !acc !this ('\'':cs) = delimSz acc (this+1) cs
    delimSz !acc !this (_:cs)    = delimSz (max acc this) 1 cs

    prettyPage input =
        align (delim <> line <> body <> line <> delim)
      where
        sz    = delimSz 2 0 input
        delim = text (replicate sz '\'')
        body  = vsep (text <$> splitOn "\n" input)

runetxt :: String -> Doc
runetxt = bold . red . text

unflow :: Flow -> [Rex]
unflow (WORDS xs) = xs
unflow (SINGL x)  = [x]
unflow (BOXED x)  = [x]

flowPad :: String -> [Flow] -> Int
flowPad rune flows =
    if null rune
    then sum (go <$> flows)
    else length rune + 1 + sum (go <$> drop 1 flows)
  where
    go BOXED{} = 2
    go _       = 0

prettyOpenRune :: String -> [Rex] -> (Doc, Bool)
prettyOpenRune rune ss =
    let ps  = paragraphs ss
        pad = flowPad rune ps
        off = if null rune then 0 else length rune + 1
    in (\x -> (x, slugishFlow (last ps))) $
       case ps of
        []   -> runetxt rune
        [x]  -> hang off $
                  fill pad (runetxt rune) <> fillSep (prettyRex <$> unflow x)
        xs   -> hang off $
                  fill pad (runetxt rune) <> openFlow xs

openFlow :: [Flow] -> Doc
openFlow = dent . reverse
  where
    item = align . fillSep . fmap prettyRex . unflow

    dent :: [Flow] -> Doc
    dent []              = mempty
    dent [x]             = item x
    dent (x:y@BOXED{}:z) = nest 2 (dent (y:z)) <> line <> item x
    dent (x:y:z)         = dent (y:z) <> line <> item x

slugishFlow :: Flow -> Bool
slugishFlow = \case
    BOXED b -> slugishRex b
    _       -> False
  where
    slugishItem (LEAF SLUG _) = True
    slugishItem _             = False

    slugishRex (REX xs) = slugishItem (NonEmpty.last xs)

-- slugline :: [Flow] -> Doc -> Doc
-- slugline []     d                 = d
-- slugline [x]    d | slugishFlow x = line <> d
-- slugline (x:xs) d                 = slugline xs d

nestPrefixDoc :: Char -> Char -> String -> [Rex] -> Doc
nestPrefixDoc l r rune ss =
    align $ wrapped l r $
    if null ss
    then runetxt rune
    else align body <> if needLine then line else mempty
    where (body, needLine) = prettyOpenRune rune ss

wrapped :: Char -> Char -> Doc -> Doc
wrapped l r body = blue (char l) <> body <> blue (char r)

prettyRex :: Rex -> Doc
prettyRex rex =
    case openSep [] (rexItems rex) of
       ([], Just c)     -> closedRex c
       (opens, mClosed) -> vsep ( fmap prettyItem opens
                               <> fmap closedRex (toList mClosed)
                                )
  where
    rexItems (REX x) = NonEmpty.toList x

    closedRex (REX (x :| xs)) = mconcat (prettyItem <$> (x:xs))

    openSep :: [Item] -> [Item] -> ([Item], Maybe Rex)
    openSep acc []                               = (reverse acc, Nothing)
    openSep acc (x@(NODE PREFIX CLEAR _ _) : xs) = openSep (x:acc) xs
    openSep acc (x:xs)                           = (reverse acc, Just (REX (x :| xs)))

prettyItem :: Item -> Doc
prettyItem = \case
    LEAF s t                     -> prettyLeaf s t
    NODE PREFIX CLEAR rune sons  -> fst $ prettyOpenRune rune sons
    NODE PREFIX PAREN rune sons  -> nestPrefixDoc '(' ')' rune sons
    NODE PREFIX BRACK rune sons  -> nestPrefixDoc '[' ']' rune sons
    NODE PREFIX CURLY rune sons  -> nestPrefixDoc '{' '}' rune sons
    NODE PREFIX TIGHT rune [s]   -> runetxt rune <> prettyRex (wrapInitRune s)
    NODE PREFIX TIGHT _    _     -> error "impossible"
    NODE INFIX  _     _    _     -> text "TODO"

small :: Rex -> Bool
small r = rexSz r < 20
 where
    rexSz (REX (x :| xs)) = smallAcc 0 (itemSz <$> (x:xs))

    textSz t = if ('\n' `elem` t) then 20 else length t

    itemSz (LEAF WORD t) = length t
    itemSz (LEAF QUIP t) = textSz t + 1
    itemSz (LEAF TEXT t) = textSz t + 2
    itemSz _             = 20

    smallAcc !i []     = i
    smallAcc !i (x:xs) = if i >= 20 then 20 else smallAcc (i+x) xs

data Flow = BOXED Rex | SINGL Rex | WORDS [Rex]

paragraphs :: [Rex] -> [Flow]
paragraphs = go []
  where
    go []  []               = []
    go acc []               = [WORDS (reverse acc)]
    go acc (x:xs) | small x = go (x:acc) xs
    go []  (x:xs)           = maybeBox x : go [] xs
    go acc (x:xs)           = WORDS (reverse acc) : maybeBox x : go [] xs

    maybeBox :: Rex -> Flow
    maybeBox r = (if boxedRex r then BOXED else SINGL) r

    boxedItem (LEAF SLUG _)           = True
    boxedItem (LEAF _ _)              = False
    boxedItem (NODE PREFIX CLEAR _ _) = True
    boxedItem (NODE _ _ _ _)          = False

    boxedRex (REX (x :| xs)) = any boxedItem (x:xs)


-- Tests -----------------------------------------------------------------------

leaf :: Style -> String -> Rex
leaf s t = REX $ singleton $ LEAF s t

node :: Shape -> Color -> String -> [Rex] -> Rex
node s c t ss = REX $ singleton $ NODE s c t ss

clump :: [Item] -> Rex
clump []     = error "empty clump"
clump (x:xs) = REX (x :| xs)

word, txt, quip, slug, page :: String -> Rex
word = leaf WORD
txt  = leaf TEXT
quip = leaf QUIP
slug = leaf SLUG
page = leaf PAGE

someFunc :: IO ()
someFunc = do
    putDoc $ indent 4 $ prettyRex $ cleanRex $ clump
      [ LEAF WORD "hi"
      , NODE PREFIX CLEAR "|"
        [ leaf WORD "Hello"
        , leaf WORD "Pizza"
        , node PREFIX BRACK "," (word <$> ["hi", "pie", "pizza"])
        , node PREFIX PAREN "`" (word <$> ["map", "f", "xs"])
        , node PREFIX PAREN "|" [ node PREFIX PAREN "`" (word <$> ["map", "f", "xs"])
                                , node PREFIX PAREN "`" (word <$> ["map", "f", "xs"])
                                , node PREFIX PAREN "`" (word <$> ["map", "f", "xs"])
                                ]
        , node PREFIX PAREN "`" [ node PREFIX PAREN "`" (word <$> ["map", "f", "xs"])
                                , node PREFIX PAREN "`" (word <$> ["map", "f", "xs"])
                                , node PREFIX PAREN "`" (word <$> ["map", "f", "xs"])
                                ]
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , leaf WORD "Pizza"
        , node PREFIX PAREN ","
          [ leaf SLUG "Multi-line\nstring '''\nexample"
          , leaf SLUG "Multi-line \"string\" w/\ntrailing EOL\n"
          , leaf PAGE "Multi-line\nstring '''\nexample"
          , leaf PAGE "Multi-line \"string\" w/\ntrailing EOL\n"
          ]
        , clump [ LEAF WORD "b", LEAF TEXT "Hi!"]
        , clump [ LEAF WORD "b", LEAF WORD "Hi_"]
        , clump [ LEAF SLUG "slug", LEAF WORD "Hi_"]
        , clump [ LEAF WORD "b", LEAF TEXT "Hello!"]
        ]
      , NODE PREFIX CLEAR "|" [
           leaf WORD "Hello",
           leaf TEXT "Hi world!",
           leaf WORD "Pizza"
        ]
      , NODE PREFIX CLEAR "|"
         [ leaf WORD "Hello"
         , leaf WORD "World"
         , leaf WORD "Pizza"
         , node PREFIX BRACK "," (word <$> ["hi", "pie"])
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf WORD "Pizza"
         , leaf SLUG "Multi-line\nstring '''\nexample"
         , leaf SLUG "Multi-line \"string\" w/\ntrailing EOL\n"
         , clump [ LEAF WORD "b", LEAF TEXT "Hi!"]
         , clump [ LEAF WORD "b", LEAF TEXT "Hello!"]
         , node PREFIX PAREN ""
             [ node PREFIX TIGHT "+"
                 [ word "pizza"
                 ]
             , node PREFIX TIGHT "+"
                 [ node PREFIX TIGHT "+"
                     [ word "pizza" ]
                 ]
             , clump
                 [ NODE PREFIX TIGHT "+"
                     [ word "pizza" ]
                 , NODE PREFIX TIGHT "+"
                     [ word "pizza" ]
                 ]
             ]
         , node PREFIX PAREN "|"
             [ slug "turtles"
             , slug "turtles"
             , slug "turtles"
             ]
         , node PREFIX PAREN "" [(clump
             [ LEAF WORD "b"
             , LEAF SLUG "silly"
             ])]
         ]
     , NODE PREFIX PAREN "" [(clump
           [ LEAF WORD "b"
           , LEAF TEXT "final_heir"
           ])]
     ]

    putStrLn "" >> putStrLn ""
