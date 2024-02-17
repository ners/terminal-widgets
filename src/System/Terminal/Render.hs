{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module System.Terminal.Render where

import Data.Generics.Product qualified as Lens
import Data.Text qualified as Text
import GHC.Records qualified as GHC
import Internal.Prelude
import Internal.Prelude qualified as Prelude

data Cursor = Cursor
    { row :: !Int
    , col :: !Int
    }
    deriving stock (Generic, Eq, Show)

type HasCursor w =
    ( GHC.HasField "cursor" w Cursor
    , Lens.HasField "cursor" w w Cursor Cursor
    )

type MonadCursor t m m' =
    ( MonadTrans t
    , MonadScreen m'
    , m ~ t m'
    , MonadState Cursor m
    )

render :: (MonadScreen m) => Maybe (Cursor, Text) -> (Cursor, Text) -> m ()
render maybeOld new = flip evalStateT (maybe (Cursor 0 0) fst maybeOld) do
    let oldLines = maybe [""] (Text.lines . snd) maybeOld
    let newLines = (Text.lines . snd) new
    let deltas =
            filter (\(_, oldText, newText) -> oldText /= newText)
                $ zip3 [0 :: Int ..] oldLines newLines

    forM_ deltas $ \(row, oldText, newText) -> do
        moveToRow row
        lift $ renderLine oldText newText
        #col .= Text.length newText

    -- Clear the remaining old lines
    when (length oldLines > length newLines) do
        moveToRow $ length newLines
        moveToColumn 0
        lift $ eraseInDisplay EraseForward

    -- Print the remaining new lines
    when (length newLines > length oldLines) do
        moveToRow $ length oldLines - 1
        putLn
        sequence_ $ intersperse putLn $ putText <$> drop (length oldLines) newLines

    moveToPosition $ fst new
  where
    moveToRow :: (MonadCursor t m m') => Int -> m ()
    moveToRow newRow = do
        oldRow <- use #row
        let dy = newRow - oldRow
        when (dy > 0) $ lift $ moveCursorDown dy
        when (dy < 0) $ lift $ moveCursorUp $ negate dy
        #row .= newRow

    moveToColumn :: (MonadCursor t m m') => Int -> m ()
    moveToColumn newCol = do
        oldCol <- use #col
        when (oldCol /= newCol) do
            lift (setCursorColumn newCol) >> #col .= newCol

    moveToPosition :: (MonadCursor t m m') => Cursor -> m ()
    moveToPosition Cursor{..} = moveToRow row >> moveToColumn col

    putLn :: (MonadCursor t m m') => m ()
    putLn = lift Prelude.putLn >> moveToColumn 0 >> #row %= succ

    putText :: (MonadCursor t m m') => Text -> m ()
    putText t = lift (Prelude.putText t) >> #col %= (+ Text.length t)

renderLine :: (MonadScreen m) => Text -> Text -> m ()
renderLine oldText newText =
    unless (oldText == newText) do
        let (Text.length -> prefixLen, Text.length -> oldSuffixLen, newSuffix) =
                commonPrefixes oldText newText
        setCursorColumn prefixLen
        putText newSuffix
        when (oldSuffixLen > Text.length newSuffix) $ eraseInLine EraseForward
