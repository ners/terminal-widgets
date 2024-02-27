{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Render where

import Data.Text qualified as Text
import Prelude

deriving stock instance Generic Position

type MonadCursor t m m' =
    ( MonadTrans t
    , MonadScreen m'
    , m ~ t m'
    , MonadState Position m
    )

render :: (MonadScreen m) => Maybe (Position, Text) -> (Position, Text) -> m ()
render maybeOld new = flip evalStateT (maybe Position{row = 0, col = 0} fst maybeOld) do
    let oldLines = maybe [""] (Text.lines . snd) maybeOld
    let newLines = (Text.lines . snd) new
    let deltas =
            filter (\(_, oldText, newText) -> oldText /= newText)
                $ zip3 [0 :: Int ..] oldLines newLines

    forM_ deltas $ \(row, oldText, newText) -> do
        moveToRow row
        lift $ renderLine oldText newText
        modify $ #col .~ Text.length newText

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

    putLn :: (MonadCursor t m m') => m ()
    putLn = lift Prelude.putLn >> moveToColumn 0 >> modify (#row %~ succ)
    putText :: (MonadCursor t m m') => Text -> m ()
    putText t = lift (Prelude.putText t) >> modify (#col %~ (+ Text.length t))

renderLine :: (MonadScreen m) => Text -> Text -> m ()
renderLine oldText newText =
    unless (oldText == newText) do
        let (Text.length -> prefixLen, Text.length -> oldSuffixLen, newSuffix) =
                commonPrefixes oldText newText
        setCursorColumn prefixLen
        putText newSuffix
        when (oldSuffixLen > Text.length newSuffix) $ eraseInLine EraseForward
