{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Text qualified as Text
import System.Terminal.Widgets.Buttons
import System.Terminal.Widgets.Common (runWidgetIO)
import System.Terminal.Widgets.SearchSelect
import System.Terminal.Widgets.Select
import System.Terminal.Widgets.TextInput
import Prelude

deriving stock instance Show TextInput

deriving stock instance Show Buttons

deriving stock instance (Show a) => Show (SelectOption a)

deriving stock instance (Show a) => Show (Select a)

deriving stock instance (Show a) => Show (SearchSelect a)

main :: IO ()
main = do
    print
        =<< runWidgetIO
            TextInput
                { prompt = "Single line text: "
                , multiline = False
                , required = False
                , value = ""
                , valueTransform = id
                }
    print
        =<< runWidgetIO
            TextInput
                { prompt = "Single line password: "
                , multiline = False
                , required = False
                , value = ""
                , valueTransform = Text.map (const '*')
                }
    print
        =<< runWidgetIO
            TextInput
                { prompt = "Multi line text: "
                , multiline = True
                , required = False
                , value = ""
                , valueTransform = id
                }
    print
        =<< runWidgetIO
            Buttons
                { prompt = "Buttons: "
                , buttons = [("Button 1", Just '1'), ("Button 2", Just '2')]
                , selected = 0
                }
    print
        =<< runWidgetIO
            Select
                { prompt = "Single select: "
                , options = [SelectOption{value, checked = False} | value <- [1 :: Int .. 5]]
                , optionText = ishow
                , minSelect = 1
                , maxSelect = 1
                , cursorOption = 0
                }
    print
        =<< runWidgetIO
            Select
                { prompt = "Multi select: "
                , options = [SelectOption{value, checked = False} | value <- [1 :: Int .. 5]]
                , optionText = ishow
                , minSelect = 0
                , maxSelect = 3
                , cursorOption = 0
                }
    let options :: [Int]
        options = [1000 .. 2000]
    print
        =<< runWidgetIO
            SearchSelect
                { prompt = "Search select (single): "
                , searchValue = ""
                , options = options
                , visibleOptions = options
                , selections = [1234]
                , optionText = ishow
                , minSelect = 1
                , maxSelect = 1
                , minSearchLength = 1
                , maxVisible = 5
                , cursorRow = 0
                , newOption = const Nothing
                }
    print
        =<< runWidgetIO
            SearchSelect
                { prompt = "Search select (multi): "
                , searchValue = ""
                , options = options
                , visibleOptions = options
                , selections = [1234]
                , optionText = ishow
                , minSelect = 1
                , maxSelect = 3
                , minSearchLength = 1
                , maxVisible = 5
                , cursorRow = 0
                , newOption = const Nothing
                }
