{-# LANGUAGE DuplicateRecordFields #-}

module System.Terminal.Widget
    ( module System.Terminal.Widget.Class
    , runWidget'
    , runWidget
    , runWidgetIO
    , module System.Terminal.Widget.Buttons
    , module System.Terminal.Widget.SearchSelect
    , module System.Terminal.Widget.Select
    , module System.Terminal.Widget.TextInput
    , module System.Terminal.Modifier
    )
where

import System.Terminal.Modifier (Modifier (..))
import System.Terminal.Widget.Buttons (Buttons (..))
import System.Terminal.Widget.Class
import System.Terminal.Widget.SearchSelect (SearchSelect (..))
import System.Terminal.Widget.Select (Select (..), SelectOption (..))
import System.Terminal.Widget.TextInput (TextInput (..))
import Prelude

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
