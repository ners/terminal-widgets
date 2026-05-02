{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widget.Class where

import Control.Applicative
import Prettyprinter
import Prettyprinter.Extra (countLinesS)
import System.Terminal.Modifier ()
import System.Terminal.Widget.Render qualified as Render
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
    toDocStream :: (MonadTerminal m) => w -> SimpleDocStream (Attribute m)
    default toDocStream :: (Show w) => w -> SimpleDocStream (Attribute m)
    toDocStream = layoutPretty defaultLayoutOptions . ishow
    lineCount :: w -> Int
    lineCount = countLinesS . toDocStream @_ @(TerminalT LocalTerminal IO)
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
    r :: w -> (Position, SimpleDocStream (Attribute m))
    r w = (w ^. cursor, toDocStream w)
