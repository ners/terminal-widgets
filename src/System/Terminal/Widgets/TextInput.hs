{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module System.Terminal.Widgets.TextInput where

import Data.Text qualified as Text
import Data.Text.Rope.Zipper (RopeZipper)
import Data.Text.Rope.Zipper qualified as RopeZipper
import Internal.Prelude
import Internal.Prelude qualified as Prelude
import System.Terminal.Render
import System.Terminal.Widgets.Common

data TextInput = TextInput
    { prompt :: !Text
    , multiline :: !Bool
    , required :: !Bool
    , value :: !RopeZipper
    , valueTransform :: !(Text -> Text)
    }
    deriving stock (Generic)

instance Widget TextInput where
    cursor = #value . #cursor
    handleEvent (KeyEvent BackspaceKey []) = #value %~ RopeZipper.deleteBefore
    handleEvent (KeyEvent DeleteKey []) = #value %~ RopeZipper.deleteAfter
    handleEvent (KeyEvent (CharKey k) []) = #value %~ RopeZipper.insertText (Text.singleton k)
    handleEvent (KeyEvent (ArrowKey Leftwards) []) = #value %~ RopeZipper.moveBackward
    handleEvent (KeyEvent (ArrowKey Rightwards) []) = #value %~ RopeZipper.moveForward
    handleEvent (KeyEvent EnterKey []) = filtered (.multiline) . #value %~ RopeZipper.insertText "\n"
    handleEvent (KeyEvent (ArrowKey Upwards) []) = filtered (.multiline) . #value %~ RopeZipper.moveUp
    handleEvent (KeyEvent (ArrowKey Downwards) []) = filtered (.multiline) . #value %~ RopeZipper.moveDown
    handleEvent _ = id
    valid TextInput{..} = not $ required && RopeZipper.null value
    submitEvent t
        | valid t = Just $ KeyEvent EnterKey $ fromList [Alt | t.multiline]
        | otherwise = Nothing
    toText TextInput{..} = prompt <> valueTransform (RopeZipper.toText value)
    lineCount TextInput{..} = max 1 $ RopeZipper.lengthInLines value
    render (maybeOld, new) = flip evalStateT (maybe Position{row = 0, col = 0} (view cursor) maybeOld) do
        let getLines :: TextInput -> [Text]
            getLines =
                padLines
                    . Text.lines
                    . new.valueTransform
                    . RopeZipper.toText
                    . (.value)
            padLines :: [Text] -> [Text]
            padLines (x : xs) = (new.prompt <> x) : ((Text.replicate (Text.length new.prompt) " " <>) <$> xs)
            padLines [] = [new.prompt]
            oldLines = maybe [""] getLines maybeOld
            newLines = getLines new

        let deltas =
                filter (\(_, oldText, newText) -> oldText /= newText) $
                    zip3 [0 :: Int ..] oldLines newLines

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

        moveToPosition $ new ^. cursor
      where
        moveToRow :: (MonadCursor t m m') => Int -> m ()
        moveToRow newRow = do
            oldRow <- gets (.row)
            let dy = newRow - oldRow
            when (dy > 0) $ lift $ moveCursorDown dy
            when (dy < 0) $ lift $ moveCursorUp $ negate dy
            modify $ #row .~ newRow

        moveToColumn :: (MonadCursor t m m') => Int -> m ()
        moveToColumn ((+ Text.length new.prompt) -> newCol) = do
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
