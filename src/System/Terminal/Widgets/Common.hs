{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.Common where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Monoid hiding (Alt)
import Prettyprinter
import Prettyprinter.Extra (countLinesS)
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
    toDoc :: (MonadTerminal m) => w -> Doc (Attribute m)
    default toDoc :: (Show w) => w -> Doc (Attribute m)
    toDoc = ishow
    lineCount :: w -> Int
    lineCount =
        countLinesS
            . layoutPretty defaultLayoutOptions
            . toDoc @_ @(TerminalT LocalTerminal IO)
    render
        :: forall m
         . (MonadTerminal m)
        => Maybe w
        -> w
        -> m ()
    render = defaultRender

defaultRender
    :: forall w m
     . (Widget w, MonadTerminal m)
    => Maybe w
    -> w
    -> m ()
defaultRender maybeOld new = Render.render (r <$> maybeOld) (r new)
  where
    r :: w -> (Position, Doc (Attribute m))
    r w = (w ^. cursor, toDoc w)

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
    => (w -> m ())
    -> (Maybe w -> w -> m ())
    -> (Maybe w -> w -> m ())
    -> w
    -> m w
runWidget' setup render' cleanup w = do
    setup w
    go Nothing w
  where
    go :: Maybe w -> w -> m w
    go maybeOld current = do
        render' maybeOld current
        awaitEvent >>= \case
            Left Interrupt -> do
                cleanup maybeOld current
                liftIO $ exitWith $ ExitFailure 1
            Right e | Just e == submitEvent current -> do
                cleanup maybeOld current
                pure current
            Right e -> do
                let new = handleEvent e current
                go (Just current) new

runWidget
    :: forall m w
     . (MonadTerminal m, Widget w)
    => w
    -> m w
runWidget = runWidget' setup render' cleanup
  where
    setup :: w -> m ()
    setup _ = pure ()
    render', cleanup :: Maybe w -> w -> m ()
    render' maybeOld new = render maybeOld new >> flush
    cleanup _ w = do
        let dy = lineCount w - (w ^. cursor . #row) - 1
        when (dy > 0) $ moveCursorDown dy
        putLn
        resetAttributes
        showCursor

runWidgetIO :: forall m w. (MonadIO m, MonadMask m, Widget w) => w -> m w
runWidgetIO = withTerminal . runTerminalT . runWidget
