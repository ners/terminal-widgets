{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Prettyprinter.Extra where

import Data.Text qualified as Text
import Prettyprinter
import Prelude

nullS :: SimpleDocStream ann -> Bool
nullS SFail = True
nullS SEmpty = True
nullS _ = False

-- | Return the number of lines in the stream
countLinesS :: SimpleDocStream ann -> Int
countLinesS SFail = 0
countLinesS SEmpty = 0
countLinesS (SLine _ rest) = 1 + countLinesS rest
countLinesS s = 0 + countLinesS (s ^. tailS)

-- | Skip the first token in the stream and return the rest of the stream
tailS :: Lens' (SimpleDocStream ann) (SimpleDocStream ann)
tailS = lens getter setter
  where
    getter :: SimpleDocStream ann -> SimpleDocStream ann
    getter SFail = SFail
    getter SEmpty = SEmpty
    getter (SChar _ rest) = rest
    getter (SText _ _ rest) = rest
    getter (SLine _ rest) = rest
    getter (SAnnPush _ rest) = rest
    getter (SAnnPop rest) = rest
    setter :: SimpleDocStream ann -> SimpleDocStream ann -> SimpleDocStream ann
    setter SFail = id
    setter SEmpty = id
    setter (SChar c _) = SChar c
    setter (SText len t _) = SText len t
    setter (SLine len _) = SLine len
    setter (SAnnPush ann _) = SAnnPush ann
    setter (SAnnPop _) = SAnnPop

tokenLenS :: SimpleDocStream ann -> Int
tokenLenS SFail = 0
tokenLenS SEmpty = 0
tokenLenS (SChar _ _) = 1
tokenLenS (SText len _ _) = len
tokenLenS (SLine len _) = len
tokenLenS (SAnnPush _ _) = 0
tokenLenS (SAnnPop _) = 0

takeLineS
    :: SimpleDocStream ann
    -> (SimpleDocStream ann, SimpleDocStream ann, SimpleDocStream ann)
takeLineS SFail = (SFail, SEmpty, SEmpty)
takeLineS SEmpty = (SEmpty, SEmpty, SEmpty)
takeLineS (SLine len rest) = (SEmpty, SLine 0 SEmpty, rest')
  where
    rest' =
        if len > 0
            then SText len (Text.replicate len " ") rest
            else rest
takeLineS s = (s', newLine, rest)
  where
    (line, newLine, rest) = takeLineS (s ^. tailS)
    s' = s & tailS .~ line

lineLenS :: forall ann. SimpleDocStream ann -> Int
lineLenS = go 0
  where
    go :: Int -> SimpleDocStream ann -> Int
    go n SFail = n
    go n SEmpty = n
    go n SLine{} = n
    go n s = go (n + tokenLenS s) (s ^. tailS)

lastLineLenS :: forall ann. SimpleDocStream ann -> Int
lastLineLenS = go 0
  where
    go :: Int -> SimpleDocStream ann -> Int
    go n SFail = n
    go n SEmpty = n
    go _ (SLine len rest) = go len rest
    go n s = go (n + tokenLenS s) (s ^. tailS)

type DifferenceStream ann = SimpleDocStream ann -> SimpleDocStream ann

findCommonPrefixS
    :: forall ann
     . (Eq ann)
    => SimpleDocStream ann
    -> SimpleDocStream ann
    -> (SimpleDocStream ann, SimpleDocStream ann, SimpleDocStream ann)
findCommonPrefixS a b = let (acc, a', b') = go id a b in (acc SEmpty, a', b')
  where
    go
        :: DifferenceStream ann
        -> SimpleDocStream ann
        -> SimpleDocStream ann
        -> (DifferenceStream ann, SimpleDocStream ann, SimpleDocStream ann)
    go acc (SChar c rest1) (SChar ((== c) -> True) rest2) = go (acc . SChar c) rest1 rest2
    go acc (SText len1 t1 rest1) (SText len2 t2 rest2)
        | len1 == len2, t1 == t2 = go (acc . SText len1 t1) rest1 rest2
        | Just (common, s1, s2) <- Text.commonPrefixes t1 t2 =
            go (acc . prepend common) (prepend s1 rest1) (prepend s2 rest2)
    go acc (SLine len rest1) (SLine ((== len) -> True) rest2) = go (acc . SLine len) rest1 rest2
    go acc (SAnnPush ann rest1) (SAnnPush ((== ann) -> True) rest2) = go (acc . SAnnPush ann) rest1 rest2
    go acc (SAnnPop rest1) (SAnnPop rest2) = go (acc . SAnnPop) rest1 rest2
    go acc a b = (acc, a, b)
    prepend :: Text -> SimpleDocStream ann -> SimpleDocStream ann
    prepend "" = id
    prepend t = SText (Text.length t) t
