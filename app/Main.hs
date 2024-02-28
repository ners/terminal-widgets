{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String
import Data.Text qualified as Text
import System.Terminal
import System.Terminal.Widgets.Buttons
import System.Terminal.Widgets.Common (Widget)
import System.Terminal.Widgets.Common qualified as Terminal
import System.Terminal.Widgets.SearchSelect
import System.Terminal.Widgets.Select
import System.Terminal.Widgets.TextInput
import Text.Show.Functions ()
import Prelude

deriving stock instance Show TextInput

deriving stock instance Show Buttons

deriving stock instance (Show a) => Show (SelectOption a)

deriving stock instance (Show a) => Show (Select a)

deriving stock instance (Show a) => Show (SearchSelect a)

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

runWidget :: (Widget w) => w -> IO w
runWidget = liftIO . withTerminal . runTerminalT . Terminal.runWidget

main :: IO ()
main = do
    print
        =<< runWidget
            TextInput
                { prompt = "Single line text: "
                , multiline = False
                , required = False
                , value = ""
                , valueTransform = id
                }
    print
        =<< runWidget
            TextInput
                { prompt = "Single line password: "
                , multiline = False
                , required = False
                , value = ""
                , valueTransform = Text.map (const '*')
                }
    print
        =<< runWidget
            TextInput
                { prompt = "Multi line text: "
                , multiline = True
                , required = False
                , value = ""
                , valueTransform = id
                }
    print
        =<< runWidget
            Buttons
                { prompt = "Buttons: "
                , buttons = [("Button 1", Just '1'), ("Button 2", Just '2')]
                , selected = 0
                }
    print
        =<< runWidget @(Select Int)
            Select
                { prompt = "Single select: "
                , options = [SelectOption{value, checked = False} | value <- [1 .. 5]]
                , optionText = ishow
                , minSelect = 1
                , maxSelect = 1
                , cursorOption = 0
                }
    print
        =<< runWidget @(Select Int)
            Select
                { prompt = "Multi select: "
                , options = [SelectOption{value, checked = False} | value <- [1 .. 5]]
                , optionText = ishow
                , minSelect = 0
                , maxSelect = 3
                , cursorOption = 0
                }
    print
        =<< runWidget @(SearchSelect Int)
            SearchSelect
                { prompt = "Search select: "
                , searchValue = ""
                , options = [1000 .. 2000]
                , visibleOptions = []
                , selections = [1234]
                , optionText = ishow
                , minSelect = 1
                , maxSelect = 3
                , minSearchLength = 1
                , maxVisible = 5
                , cursorRow = 0
                , newOption = const Nothing
                }
