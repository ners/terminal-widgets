{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Render where

import Control.Monad.State.Strict qualified as State
import Data.Text qualified as Text
import Prettyprinter (Doc, SimpleDocStream (..))
import Prettyprinter qualified
import Prettyprinter.Extra
import Prelude hiding (putDoc, putLn, putSimpleDocStream)
import Prelude qualified

deriving stock instance Generic Position

type MonadCursor t m m' =
    ( MonadTrans t
    , MonadTerminal m'
    , m ~ t m'
    , MonadState Position m
    )

layoutPretty :: Doc ann -> SimpleDocStream ann
layoutPretty = Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions

type AttributeStack m = [Attribute m]

attributeStackToDocStream
    :: AttributeStack m
    -> SimpleDocStream (Attribute m)
    -> SimpleDocStream (Attribute m)
attributeStackToDocStream = foldr (\ann acc -> SAnnPush ann . acc) id

render
    :: forall m
     . (MonadTerminal m)
    => Maybe (Position, Doc (Attribute m))
    -> (Position, Doc (Attribute m))
    -> m ()
render (fromMaybe (Position{row = 0, col = 0}, "") -> (oldPos, oldDoc)) (newPos, newDoc) = do
    flip evalStateT oldPos do
        goLine 0 (layoutPretty oldDoc, mempty) (layoutPretty newDoc, mempty)
        moveToPosition newPos
  where
    goLine
        :: (MonadCursor t m' m)
        => Int
        -> (SimpleDocStream (Attribute m), AttributeStack m)
        -> (SimpleDocStream (Attribute m), AttributeStack m)
        -> m' ()
    goLine _ (nullS -> True, _) (nullS -> True, _) = do
        pure ()
    goLine _ (nullS -> True, _) (newStream, _) = do
        putSimpleDocStream newStream
    goLine _ _ (nullS -> True, _) = do
        lift $ eraseInDisplay EraseForward
    goLine line (oldStream, oldStack) (newStream, newStack) = do
        let (oldLine, oldNewLine, oldRest) = takeLine oldStream
            (newLine, newNewLine, newRest) = takeLine newStream
            (commonPrefix, oldSuffix, newSuffix) =
                if oldStack == newStack
                    then findCommonPrefix oldLine newLine
                    else (SEmpty, oldLine, newLine)
            commonPrefixLen = lineLen commonPrefix
            oldSuffixLen = lineLen oldSuffix
            newSuffixLen = lineLen newSuffix
            oldStackAfterPrefix = applyAnnotations commonPrefix oldStack
            newStackAfterPrefix = applyAnnotations commonPrefix newStack
        if nullS oldSuffix && nullS newSuffix
            then do
                when (newNewLine /= SEmpty && oldNewLine == SEmpty) putLn
                goLine (line + 1) (oldRest, oldStackAfterPrefix) (newRest, newStackAfterPrefix)
            else do
                moveToPosition Position{row = line, col = commonPrefixLen}
                -- not sure if newStackAfterPrefix is the right thing to use here ...
                unless (nullS newSuffix)
                    . putSimpleDocStream
                    . attributeStackToDocStream newStackAfterPrefix
                    $ newSuffix
                when (oldSuffixLen > newSuffixLen) . lift $ eraseInLine EraseForward
                putSimpleDocStream newNewLine
                when (newRest /= SEmpty) do
                    let oldStackAfterSuffix = applyAnnotations oldSuffix oldStackAfterPrefix
                        newStackAfterSuffix = applyAnnotations newSuffix newStackAfterPrefix
                    goLine (line + 1) (oldRest, oldStackAfterSuffix) (newRest, newStackAfterSuffix)
    applyAnnotations
        :: SimpleDocStream (Attribute m)
        -> AttributeStack m
        -> AttributeStack m
    applyAnnotations SFail = id
    applyAnnotations SEmpty = id
    applyAnnotations (SAnnPush ann rest) = applyAnnotations rest . (ann :)
    applyAnnotations (SAnnPop rest) = applyAnnotations rest . drop 1
    applyAnnotations stream = applyAnnotations (stream ^. tailS)

moveToRow :: (MonadCursor t m m') => Int -> m ()
moveToRow newRow = do
    oldRow <- State.gets (.row)
    lift $ case compare newRow oldRow of
        GT -> moveCursorDown $ newRow - oldRow
        LT -> moveCursorUp $ oldRow - newRow
        EQ -> pure ()
    State.modify $ #row .~ newRow

moveToColumn :: (MonadCursor t m m') => Int -> m ()
moveToColumn newCol = do
    oldCol <- State.gets (.col)
    when (oldCol /= newCol) do
        lift $ setCursorColumn newCol
        State.modify $ #col .~ newCol

moveToPosition :: (MonadCursor t m m') => Position -> m ()
moveToPosition Position{..} = moveToRow row >> moveToColumn col

putSimpleDocStream
    :: (MonadCursor t m m')
    => SimpleDocStream (Attribute m')
    -> m ()
putSimpleDocStream s = do
    lift $ Prelude.putSimpleDocStream s
    State.modify $
        if numLines > 0
            then #row %~ (+ numLines) >>> #col .~ len
            else #col %~ (+ len)
  where
    numLines = countLinesS s
    len = lastLineLen s

putDoc
    :: forall t m m'
     . (MonadCursor t m m')
    => Doc (Attribute m')
    -> m ()
putDoc = putSimpleDocStream . layoutPretty

putLn :: forall t m m'. (MonadCursor t m m') => m ()
putLn = do
    lift Prelude.putLn
    State.modify $ #row %~ (+ 1) >>> #col .~ 0

takeLine
    :: SimpleDocStream ann
    -> (SimpleDocStream ann, SimpleDocStream ann, SimpleDocStream ann)
takeLine SFail = (SFail, SEmpty, SEmpty)
takeLine SEmpty = (SEmpty, SEmpty, SEmpty)
takeLine (SLine len rest) = (SEmpty, SLine 0 SEmpty, rest')
  where
    rest' =
        if len > 0
            then SText len (Text.replicate len " ") rest
            else rest
takeLine s = (s', newLine, rest)
  where
    (line, newLine, rest) = takeLine (s ^. tailS)
    s' = s & tailS .~ line

lineLen :: forall ann. SimpleDocStream ann -> Int
lineLen = go 0
  where
    go :: Int -> SimpleDocStream ann -> Int
    go n SFail = n
    go n SEmpty = n
    go n SLine{} = n
    go n s = go (n + tokenLenS s) (s ^. tailS)

lastLineLen :: forall ann. SimpleDocStream ann -> Int
lastLineLen = go 0
  where
    go :: Int -> SimpleDocStream ann -> Int
    go n SFail = n
    go n SEmpty = n
    go _ (SLine len rest) = go len rest
    go n s = go (n + tokenLenS s) (s ^. tailS)

type DifferenceStream ann = SimpleDocStream ann -> SimpleDocStream ann

textStream :: Text -> SimpleDocStream ann -> SimpleDocStream ann
textStream "" = id
textStream t = SText (Text.length t) t

findCommonPrefix
    :: forall ann
     . (Eq ann)
    => SimpleDocStream ann
    -> SimpleDocStream ann
    -> (SimpleDocStream ann, SimpleDocStream ann, SimpleDocStream ann)
findCommonPrefix a b = let (acc, a', b') = go id a b in (acc SEmpty, a', b')
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
            go (acc . textStream common) (textStream s1 rest1) (textStream s2 rest2)
    go acc (SLine len rest1) (SLine ((== len) -> True) rest2) = go (acc . SLine len) rest1 rest2
    go acc (SAnnPush ann rest1) (SAnnPush ((== ann) -> True) rest2) = go (acc . SAnnPush ann) rest1 rest2
    go acc (SAnnPop rest1) (SAnnPop rest2) = go (acc . SAnnPop) rest1 rest2
    go acc a b = (acc, a, b)
