{-# LANGUAGE OverloadedLists #-}

module System.Terminal.Widgets.Select where

import Data.Text qualified as Text
import Internal.Prelude
import System.Terminal.Widgets.Common

data Select = Select
    { prompt :: !Text
    , options :: ![(Text, Bool)]
    , multiselect :: !Bool
    , cursorRow :: !Int
    }
    deriving stock (Generic, Eq)

instance Widget Select where
    cursor = lens getter setter
      where
        getter :: Select -> Position
        getter s = Position{row = s.cursorRow, col = 2}
        setter :: Select -> Position -> Select
        setter s Position{..} = s & #cursorRow .~ row
    toText s =
        let mkOption (t, selected) =
                mconcat
                    [ " "
                    , if s.multiselect then "[" else "("
                    , if selected then "*" else " "
                    , if s.multiselect then "]" else ")"
                    , " "
                    , t
                    ]
         in Text.unlines $ s.prompt : (mkOption <$> s.options)
    handleEvent (KeyEvent (ArrowKey Upwards) []) = moveUp
    handleEvent (KeyEvent (ArrowKey Downwards) []) = moveDown
    handleEvent (KeyEvent SpaceKey []) = filtered (not . (.multiselect)) %~ unsetAll >>> flipCurrent
    handleEvent _ = id
    valid Select{..} = multiselect || any snd options
    submitEvent s
        | valid s = Just $ KeyEvent EnterKey []
        | otherwise = Nothing

moveUp :: Select -> Select
moveUp = filtered (\s -> s.cursorRow > 1) . #cursorRow %~ pred

moveDown :: Select -> Select
moveDown = filtered (\s -> s.cursorRow < length s.options) . #cursorRow %~ succ

flipCurrent :: Select -> Select
flipCurrent s = s & #options . ix (s.cursorRow - 1) %~ second not

unsetAll :: Select -> Select
unsetAll = #options . traverse %~ second (const False)
