module Prettyprinter.Extra where

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
