{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.Common where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as Text
import System.Terminal.Render qualified as Render
import Prelude

class Widget w where
    cursor :: Lens' w Position
    handleEvent :: Event -> w -> w
    {-# MINIMAL handleEvent, cursor #-}
    submitEvent :: w -> Maybe Event
    submitEvent w
        | valid w = Just $ KeyEvent EnterKey []
        | otherwise = Nothing
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
        r :: w -> (Position, Text)
        r w = (w ^. cursor, toText w)

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

runWidget'
    :: forall m w
     . (MonadTerminal m, Widget w)
    => ((Maybe w, w) -> m ())
    -> ((Maybe w, w) -> m ())
    -> w
    -> m w
runWidget' preRender postRender = go Nothing
  where
    cleanup :: w -> m ()
    cleanup w = do
        let dy = fromIntegral (lineCount w) - (w ^. cursor . #row) - 1
        when (dy > 0) $ moveCursorDown dy
        putLn
        resetAttributes
        showCursor
    go :: Maybe w -> w -> m w
    go maybeOld current = do
        preRender (maybeOld, current)
        render (maybeOld, current)
        postRender (maybeOld, current)
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

runWidgetIO :: forall m w. (MonadIO m, Widget w) => w -> m w
runWidgetIO = liftIO . withTerminal . runTerminalT . runWidget

runWidget :: forall m w. (MonadTerminal m, Widget w) => w -> m w
runWidget = runWidget' (const $ pure ()) (const flush)
