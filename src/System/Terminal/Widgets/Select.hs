{-# LANGUAGE OverloadedLists #-}

module System.Terminal.Widgets.Select where

import Data.Text qualified as Text
import Prelude
import System.Terminal.Widgets.Common

data SelectOption a = SelectOption
    { value :: a
    , checked :: Bool
    }
    deriving stock (Generic)

data Select a = Select
    { prompt :: Text
    , options :: [SelectOption a]
    , optionText :: a -> Text
    , minSelect :: Int
    , maxSelect :: Int
    , cursorRow :: Int
    }
    deriving stock (Generic)

instance (Show a) => Widget (Select a) where
    cursor = lens getter setter
      where
        getter :: Select a -> Position
        getter s = Position{row = s.cursorRow + 1, col = 2}
        setter :: Select a -> Position -> Select a
        setter s Position{..} = s & #cursorRow .~ row - 1
    handleEvent (KeyEvent (ArrowKey Upwards) []) s = moveUp s
    handleEvent (KeyEvent (ArrowKey Downwards) []) s = moveDown s
    handleEvent (KeyEvent SpaceKey []) s
        | s.maxSelect == 1 = s & flipCurrent . uncheckAll
        | numChecked s < s.maxSelect = flipCurrent s
    handleEvent _ s = s
    valid s = inRange (s.minSelect, s.maxSelect) $ numChecked s
    toText s =
        let mkOption SelectOption{..} =
                mconcat
                    [ " "
                    , Text.intercalate
                        (if checked then "*" else " ")
                        (if s.maxSelect > 1 then ["[", "]"] else ["(", ")"])
                    , " "
                    , s.optionText value
                    ]
         in Text.unlines $ s.prompt : (mkOption <$> s.options)

moveUp :: Select a -> Select a
moveUp = filtered (\s -> s.cursorRow > 0) . #cursorRow %~ pred

moveDown :: Select a -> Select a
moveDown = filtered (\s -> s.cursorRow < length s.options - 1) . #cursorRow %~ succ

flipCurrent :: Select a -> Select a
flipCurrent s = s & #options . ix s.cursorRow . #checked %~ not

uncheckAll :: Select a -> Select a
uncheckAll = #options . traverse . #checked .~ False

numChecked :: Select a -> Int
numChecked = length . filter (.checked) . (.options)
