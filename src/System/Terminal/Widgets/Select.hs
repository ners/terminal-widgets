{-# LANGUAGE OverloadedLists #-}

module System.Terminal.Widgets.Select where

import Data.Text qualified as Text
import System.Terminal.Widgets.Common
import Prelude

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
    , cursorOption :: Int
    }
    deriving stock (Generic)

instance (Show a) => Widget (Select a) where
    cursor = lens getter setter
      where
        getter :: Select a -> Position
        getter s = Position{row = s.cursorOption + 1, col = 2}
        setter :: Select a -> Position -> Select a
        setter s Position{..} = s & #cursorOption .~ row - 1
    handleEvent (KeyEvent (ArrowKey Upwards) []) s = moveUp s
    handleEvent (KeyEvent (ArrowKey Downwards) []) s = moveDown s
    handleEvent (KeyEvent SpaceKey []) s
        | s.maxSelect == 1 = s & flipCurrent . uncheckAll
        | numChecked s < s.maxSelect || s ^. current . #checked = flipCurrent s
    handleEvent _ s = s
    valid s = inRange (s.minSelect, s.maxSelect) $ numChecked s
    toDoc s =
        let mkOption SelectOption{..} =
                mconcat
                    [ " "
                    , Text.intercalate
                        (if checked then "*" else " ")
                        (if s.maxSelect > 1 then ["[", "]"] else ["(", ")"])
                    , " "
                    , s.optionText value
                    ]
         in pretty $ Text.unlines $ s.prompt : (mkOption <$> s.options)
    lineCount s = 1 + length s.options

moveUp :: Select a -> Select a
moveUp = filtered (\s -> s.cursorOption > 0) . #cursorOption %~ pred

moveDown :: Select a -> Select a
moveDown =
    filtered (\s -> s.cursorOption < length s.options - 1) . #cursorOption %~ succ

current :: Lens' (Select a) (SelectOption a)
current = lens getter setter
  where
    getter :: Select a -> SelectOption a
    getter s = s.options !! s.cursorOption
    setter :: Select a -> SelectOption a -> Select a
    setter s o = s & #options . ix s.cursorOption .~ o

flipCurrent :: Select a -> Select a
flipCurrent s = s & current . #checked %~ not

uncheckAll :: Select a -> Select a
uncheckAll = #options . traverse . #checked .~ False

numChecked :: Select a -> Int
numChecked = length . filter (.checked) . (.options)
