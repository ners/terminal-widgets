{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.Common where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as Text
import Internal.Prelude
import System.Terminal.Render hiding (render)
import System.Terminal.Render qualified as Render

class (HasCursor w) => Widget w where
    handleEvent :: Event -> w -> w
    submitEvent :: w -> Maybe Event
    submitEvent _ = Just $ KeyEvent EnterKey []
    valid :: w -> Bool
    valid = const True
    toText :: w -> Text
    default toText :: (Show w) => w -> Text
    toText = Text.pack . show
    lineCount :: w -> Word
    lineCount = fromIntegral . Text.count "\n" . toText
    render :: (MonadTerminal m) => (Maybe w, w) -> m ()
    render (maybeOld, new) = Render.render (r <$> maybeOld) (r new)
      where
        r :: w -> (Cursor, Text)
        r w = (w.cursor, toText w)

data Modifier = Shift | Ctrl | Alt | Meta
    deriving stock (Bounded, Enum)

toModifiers :: Modifier -> Modifiers
toModifiers Shift = shiftKey
toModifiers Ctrl = ctrlKey
toModifiers Alt = altKey
toModifiers Meta = metaKey

instance IsList Modifiers where
    type Item Modifiers = Modifier
    fromList = mconcat . fmap toModifiers
    toList mods = filter hasMod [minBound .. maxBound]
      where
        hasMod c = mods .&. toModifiers c /= mempty

runWidget :: forall m w. (MonadTerminal m, Widget w) => w -> m w
runWidget = go Nothing
  where
    cleanup :: w -> m ()
    cleanup w = do
        let dy = fromIntegral (lineCount w) - w.cursor.row - 1
        when (dy > 0) $ moveCursorDown dy
        putLn
        resetAttributes
        showCursor
    go :: Maybe w -> w -> m w
    go maybeOld current = do
        render (maybeOld, current) >> flush
        awaitEvent >>= \case
            Left Interrupt -> do
                cleanup current
                liftIO $ exitWith $ ExitFailure 1
            Right e | Just e == submitEvent current -> do
                cleanup current
                pure current
            Right e -> do
                let new = handleEvent e current
                go (Just current) new
