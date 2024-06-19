{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Render where

import Control.Monad.State.Strict qualified as State
import Data.Text qualified as Text
import Prettyprinter (Doc, SimpleDocStream (..))
import Prettyprinter qualified
import Prelude hiding (putDoc, putSimpleDocStream, putLn)
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

render
    :: forall m. (MonadTerminal m)
    => Maybe (Position, Doc (Attribute m))
    -> (Position, Doc (Attribute m))
    -> m ()
render (fromMaybe (Position{row = 0, col = 0}, "") -> (oldPos, oldDoc)) (newPos, newDoc) = do
    flip evalStateT oldPos do
        goLine 0 (layoutPretty oldDoc, mempty) (layoutPretty newDoc, mempty)
        moveToPosition newPos
  where
    final :: SimpleDocStream ann -> Bool
    final SFail = True
    final SEmpty = True
    final _ = False
    goLine
        :: (MonadCursor t m' m)
        => Int
        -> (SimpleDocStream (Attribute m), AttributeStack m)
        -> (SimpleDocStream (Attribute m), AttributeStack m)
        -> m' ()
    goLine _ (final -> True, _) (final -> True, _) = do
        --traceM "goLine 1"
        pure ()
    goLine _ (final -> True, _) (newStream, _) = do
        --traceM "goLine 2"
        putSimpleDocStream newStream
    goLine _ _ (final -> True, _) = do
        --traceM "goLine 3"
        lift $ eraseInDisplay EraseForward
    --goLine _ old new | old == new = do
    --    --traceM "goLine 4"
    --    pure ()
    goLine line (oldStream, oldStack) (newStream, newStack) = do
        --traceM "goLine 5"
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
        if oldSuffix == SEmpty && newSuffix == SEmpty
            then do
                --traceM " -> goLine 5.1"
                when (newNewLine /= SEmpty && oldNewLine == SEmpty) putLn
                goLine (line + 1) (oldRest, oldStackAfterPrefix) (newRest, newStackAfterPrefix)
            else do
                --traceM " -> goLine 5.2"
                moveToPosition Position{row = line, col = commonPrefixLen}
                putSimpleDocStream newSuffix
                when (oldSuffixLen > newSuffixLen) . lift $ eraseInLine EraseForward
                putSimpleDocStream newNewLine
                when (newRest /= SEmpty) do
                    --traceM " -> goLine 5.3"
                    let oldStackAfterSuffix = applyAnnotations oldSuffix oldStackAfterPrefix
                        newStackAfterSuffix = applyAnnotations newSuffix newStackAfterPrefix
                    goLine (line + 1) (oldRest, oldStackAfterSuffix) (newRest, newStackAfterSuffix)
    applyAnnotations :: SimpleDocStream (Attribute m) -> AttributeStack m -> AttributeStack m
    applyAnnotations SFail stack = stack
    applyAnnotations SEmpty stack = stack
    applyAnnotations (SAnnPush ann rest) stack = applyAnnotations rest $ ann : stack
    applyAnnotations (SAnnPop rest) stack = applyAnnotations rest $ drop 1 stack
    applyAnnotations stream stack = applyAnnotations (stream ^. stail) stack

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
    numLines = countLines s
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

renderLine :: (MonadScreen m) => Text -> Text -> m ()
renderLine oldText newText =
    unless (oldText == newText) do
        let (Text.length -> prefixLen, Text.length -> oldSuffixLen, newSuffix) =
                commonPrefixes oldText newText
        setCursorColumn prefixLen
        putText newSuffix
        when (oldSuffixLen > Text.length newSuffix) $ eraseInLine EraseForward

-- | Skip the first token in the stream and return the rest of the stream
stail :: Lens' (SimpleDocStream ann) (SimpleDocStream ann)
stail = lens get set
  where
    get :: SimpleDocStream ann -> SimpleDocStream ann
    get SFail = SFail
    get SEmpty = SEmpty
    get (SChar _ rest) = rest
    get (SText _ _ rest) = rest
    get (SLine _ rest) = rest
    get (SAnnPush _ rest) = rest
    get (SAnnPop rest) = rest
    set :: SimpleDocStream ann -> SimpleDocStream ann -> SimpleDocStream ann
    set SFail rest = rest
    set SEmpty rest = rest
    set (SChar c _) rest = SChar c rest
    set (SText len t _) rest = SText len t rest
    set (SLine len _) rest = SLine len rest
    set (SAnnPush ann _) rest = SAnnPush ann rest
    set (SAnnPop _) rest = SAnnPop rest

-- | Return the number of lines in the stream
countLines :: SimpleDocStream ann -> Int
countLines SFail = 0
countLines SEmpty = 0
countLines (SLine _ rest) = 1 + countLines rest
countLines s = 0 + countLines (s ^. stail)

tokenLen :: SimpleDocStream ann -> Int
tokenLen SFail = 0
tokenLen SEmpty = 0
tokenLen (SChar _ _) = 1
tokenLen (SText len _ _) = len
tokenLen (SLine len _) = len
tokenLen (SAnnPush _ _) = 0
tokenLen (SAnnPop _) = 0

takeLine :: SimpleDocStream ann -> (SimpleDocStream ann, SimpleDocStream ann, SimpleDocStream ann)
takeLine SFail = (SFail, SEmpty, SEmpty)
takeLine SEmpty = (SEmpty, SEmpty, SEmpty)
takeLine (SLine len rest) = (SEmpty, SLine 0 SEmpty, rest')
    where rest' = if len > 0
            then SText len (Text.replicate len " ") rest
            else rest
takeLine s = (s', newLine, rest)
  where
    (line, newLine, rest) = takeLine (s ^. stail)
    s' = s & stail .~ line

lineLen :: forall ann. SimpleDocStream ann -> Int
lineLen = go 0
  where
    go :: Int -> SimpleDocStream ann -> Int
    go n SFail = n
    go n SEmpty = n
    go n SLine{} = n
    go n s = go (n + tokenLen s) (s ^. stail)

lastLineLen :: forall ann. SimpleDocStream ann -> Int
lastLineLen = go 0
  where
    go :: Int -> SimpleDocStream ann -> Int
    go n SFail = n
    go n SEmpty = n
    go _ (SLine len rest) = go len rest
    go n s = go (n + tokenLen s) (s ^. stail)

type DifferenceStream ann = SimpleDocStream ann -> SimpleDocStream ann

findCommonPrefix :: forall ann. (Eq ann) => SimpleDocStream ann -> SimpleDocStream ann -> (SimpleDocStream ann, SimpleDocStream ann, SimpleDocStream ann)
findCommonPrefix a b = let (acc, a', b') = go id a b in (acc SEmpty, a', b')
    where
        go :: DifferenceStream ann -> SimpleDocStream ann -> SimpleDocStream ann -> (DifferenceStream ann, SimpleDocStream ann, SimpleDocStream ann)
        go acc (SChar c rest1) (SChar ((== c) -> True) rest2) = go (acc . SChar c) rest1 rest2
        go acc (SText len t rest1) (SText ((== len) -> True) ((== t) -> True) rest2) = go (acc . SText len t) rest1 rest2
        go acc (SLine len rest1) (SLine ((== len) -> True) rest2) = go (acc . SLine len) rest1 rest2
        go acc (SAnnPush ann rest1) (SAnnPush ((== ann) -> True) rest2) = go (acc . SAnnPush ann) rest1 rest2
        go acc (SAnnPop rest1) (SAnnPop rest2) = go (acc . SAnnPop) rest1 rest2
        go acc a b = (acc, a, b)
