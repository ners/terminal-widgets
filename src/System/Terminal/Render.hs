{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Render where

import Control.Monad.State.Strict qualified as State
import Data.Text qualified as Text
import Prettyprinter
import Prelude hiding (putDoc, putSimpleDocStream)
import Prelude qualified

deriving stock instance Generic Position

type MonadCursor t m m' =
    ( MonadTrans t
    , MonadTerminal m'
    , m ~ t m'
    , MonadState Position m
    )

render
    :: (MonadTerminal m)
    => Maybe (Position, Doc (Attribute m))
    -> (Position, Doc (Attribute m))
    -> m ()
render (fromMaybe (Position{row = 0, col = 0}, "") -> (oldPos, oldDoc)) (newPos, newDoc) = do
    flip evalStateT oldPos do
        go (toStream oldDoc) (toStream newDoc)
        moveToPosition newPos
  where
    toStream :: Doc ann -> SimpleDocStream ann
    toStream = layoutPretty defaultLayoutOptions
    final :: SimpleDocStream ann -> Bool
    final SFail = True
    final SEmpty = True
    final _ = False
    go
        :: (MonadCursor t m m')
        => SimpleDocStream (Attribute m')
        -> SimpleDocStream (Attribute m')
        -> m ()
    go (final -> True) (final -> True) = pure ()
    go (final -> True) new = putSimpleDocStream new
    go _ (final -> True) = lift $ eraseInDisplay EraseForward
    go old new = do
        moveToPosition $ Position{row = 0, col = 0}
        lift $ eraseInDisplay EraseForward
        putSimpleDocStream new

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
putDoc = putSimpleDocStream . layoutPretty defaultLayoutOptions

renderLine :: (MonadScreen m) => Text -> Text -> m ()
renderLine oldText newText =
    unless (oldText == newText) do
        let (Text.length -> prefixLen, Text.length -> oldSuffixLen, newSuffix) =
                commonPrefixes oldText newText
        setCursorColumn prefixLen
        putText newSuffix
        when (oldSuffixLen > Text.length newSuffix) $ eraseInLine EraseForward

-- | Skip the first token in the stream and return the rest of the stream
stail :: SimpleDocStream ann -> SimpleDocStream ann
stail SFail = SFail
stail SEmpty = SEmpty
stail (SChar _ rest) = rest
stail (SText _ _ rest) = rest
stail (SLine _ rest) = rest
stail (SAnnPush _ rest) = rest
stail (SAnnPop rest) = rest

-- | Return the number of lines in the stream
countLines :: SimpleDocStream ann -> Int
countLines SFail = 0
countLines SEmpty = 0
countLines (SLine _ rest) = 1 + countLines rest
countLines (stail -> rest) = 0 + countLines rest

tokenLen :: SimpleDocStream ann -> Int
tokenLen SFail = 0
tokenLen SEmpty = 0
tokenLen (SChar _ _) = 1
tokenLen (SText len _ _) = len
tokenLen (SLine len _) = len
tokenLen (SAnnPush _ _) = 0
tokenLen (SAnnPop _) = 0

lastLineLen :: forall ann. SimpleDocStream ann -> Int
lastLineLen = go 0
  where
    go :: Int -> SimpleDocStream ann -> Int
    go n SFail = n
    go n SEmpty = n
    go _ (SLine len rest) = go len rest
    go n s = go (n + tokenLen s) (stail s)
