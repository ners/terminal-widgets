{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Render where

import Data.Text qualified as Text
import Prettyprinter
import Prelude

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
render maybeOld (newPos, newDoc) = flip evalStateT (maybe Position{row = 0, col = 0} fst maybeOld) do
    moveToPosition Position{row = 0, col = 0}
    lift $ eraseInDisplay EraseForward
    putDoc newDoc
    moveToPosition newPos
  where
    moveToRow :: (MonadCursor t m m') => Int -> m ()
    moveToRow newRow = do
        oldRow <- gets (.row)
        lift $ case compare newRow oldRow of
            GT -> moveCursorDown $ newRow - oldRow
            LT -> moveCursorUp $ oldRow - newRow
            EQ -> pure ()
        modify $ #row .~ newRow

    moveToColumn :: (MonadCursor t m m') => Int -> m ()
    moveToColumn newCol = do
        oldCol <- gets (.col)
        when (oldCol /= newCol) do
            lift (setCursorColumn newCol)
            modify $ #col .~ newCol

    moveToPosition :: (MonadCursor t m m') => Position -> m ()
    moveToPosition Position{..} = moveToRow row >> moveToColumn col

    putDoc :: forall t m m'. (MonadCursor t m m') => Doc (Attribute m') -> m ()
    putDoc d = do
        lift $ Prelude.putDoc d
        let doc = layoutPretty defaultLayoutOptions d
        let numLines = countLines doc
        let len = lastLineLen doc
        modify $
            if numLines > 1
                then #row %~ (+ (numLines - 1)) >>> #col .~ len
                else #col %~ (+ len)

renderLine :: (MonadScreen m) => Text -> Text -> m ()
renderLine oldText newText =
    unless (oldText == newText) do
        let (Text.length -> prefixLen, Text.length -> oldSuffixLen, newSuffix) =
                commonPrefixes oldText newText
        setCursorColumn prefixLen
        putText newSuffix
        when (oldSuffixLen > Text.length newSuffix) $ eraseInLine EraseForward

stail :: SimpleDocStream (Attribute m) -> SimpleDocStream (Attribute m)
stail SFail = SFail
stail SEmpty = SEmpty
stail (SChar _ rest) = rest
stail (SText _ _ rest) = rest
stail (SLine _ rest) = rest
stail (SAnnPush _ rest) = rest
stail (SAnnPop rest) = rest

countLines :: SimpleDocStream (Attribute m) -> Int
countLines SFail = 0
countLines SEmpty = 0
countLines (SLine _ rest) = 1 + countLines rest
countLines (stail -> rest) = 0 + countLines rest

tokenLen :: SimpleDocStream (Attribute m) -> Int
tokenLen SFail = 0
tokenLen SEmpty = 0
tokenLen (SChar _ _) = 1
tokenLen (SText len _ _) = len
tokenLen (SLine len _) = len
tokenLen (SAnnPush _ _) = 0
tokenLen (SAnnPop _) = 0

lastLineLen :: forall m. SimpleDocStream (Attribute m) -> Int
lastLineLen = go 0
  where
    go :: Int -> SimpleDocStream (Attribute m) -> Int
    go n SFail = n
    go n SEmpty = n
    go _ (SLine len rest) = go len rest
    go n s = go (n + tokenLen s) (stail s)
