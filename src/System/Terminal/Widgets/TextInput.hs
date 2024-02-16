{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module System.Terminal.Widgets.TextInput where

import Data.Generics.Product qualified as Lens
import Data.Text qualified as Text
import Data.Text.Rope.Zipper (RopeZipper)
import Data.Text.Rope.Zipper qualified as RopeZipper
import GHC.Records qualified as GHC
import Internal.Prelude
import Internal.Prelude qualified as Prelude
import System.Terminal.Render
import System.Terminal.Widgets.Common

data TextInput = TextInput
    { prompt :: !Text
    , multiline :: !Bool
    , required :: !Bool
    , value :: !RopeZipper
    }
    deriving stock (Generic, Eq)

instance GHC.HasField "cursor" TextInput Cursor where
    getField ((.value.cursor) -> c) = Cursor{row = fromIntegral c.posLine, col = fromIntegral c.posColumn}

instance {-# OVERLAPPING #-} Lens.HasField "cursor" TextInput TextInput Cursor Cursor where
    field = lens (.cursor) (\t c -> t & #value . #cursor .~ cursorToPosition c)
      where
        cursorToPosition Cursor{..} =
            RopeZipper.Position{posLine = fromIntegral row, posColumn = fromIntegral col}

instance Widget TextInput where
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
    toText TextInput{..} = prompt <> RopeZipper.toText value
    lineCount TextInput{..} = max 1 $ RopeZipper.lengthInLines value
    render (maybeOld, new) = flip evalStateT (maybe (Cursor 0 0) (.cursor) maybeOld) do
        let getLines :: TextInput -> [Text]
            getLines =
                (_head %~ (new.prompt <>))
                    . (_tail %~ fmap (Text.replicate (Text.length new.prompt) " " <>))
                    . RopeZipper.lines
                    . (<> "\n")
                    . (.value)
            oldLines = maybe [""] getLines maybeOld
            newLines = getLines new

        let deltas =
                filter (\(_, oldText, newText) -> oldText /= newText) $
                    zip3 [0 :: Int ..] oldLines newLines

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

        moveToPosition new.cursor
      where
        moveToRow :: (MonadCursor t m m') => Int -> m ()
        moveToRow newRow = do
            oldRow <- use #row
            let dy = newRow - oldRow
            when (dy > 0) $ lift $ moveCursorDown dy
            when (dy < 0) $ lift $ moveCursorUp $ negate dy
            #row .= newRow

        moveToColumn :: (MonadCursor t m m') => Int -> m ()
        moveToColumn ((+ Text.length new.prompt) -> newCol) = do
            oldCol <- use #col
            when (oldCol /= newCol) do
                lift (setCursorColumn newCol) >> #col .= newCol

        moveToPosition :: (MonadCursor t m m') => Cursor -> m ()
        moveToPosition Cursor{..} = moveToRow row >> moveToColumn col

        putLn :: (MonadCursor t m m') => m ()
        putLn = lift Prelude.putLn >> moveToColumn 0 >> #row %= succ

        putText :: (MonadCursor t m m') => Text -> m ()
        putText t = lift (Prelude.putText t) >> #col %= (+ Text.length t)
