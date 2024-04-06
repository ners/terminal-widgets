{-# LANGUAGE OverloadedLists #-}

module System.Terminal.Widgets.SearchSelect where

import Data.Ord (Down (Down))
import Data.Text qualified as Text
import Data.Text.Rope.Zipper (RopeZipper)
import Data.Text.Rope.Zipper qualified as RopeZipper
import Internal.Prelude
import System.Terminal.Widgets.Common
import System.Terminal.Widgets.TextInput
import Text.Fuzzy qualified as Fuzzy

data SearchSelectOption a = SearchSelectOption
    { value :: a
    , visible :: Bool
    }
    deriving stock (Generic)

data SearchSelect a = SearchSelect
    { prompt :: Text
    , searchValue :: RopeZipper
    , options :: [SearchSelectOption a]
    , selections :: [a]
    , optionText :: a -> Text
    , minSelect :: Int
    , maxSelect :: Int
    , minSearchLength :: Int
    , maxVisible :: Int
    , cursorRow :: Int
    }
    deriving stock (Generic)

asTextInput :: SearchSelect a -> TextInput
asTextInput s =
    TextInput
        { prompt = fullPrompt s
        , multiline = False
        , required = False
        , value = s.searchValue
        , valueTransform = id
        }

overTextInput :: (TextInput -> TextInput) -> SearchSelect a -> SearchSelect a
overTextInput f s = s & #searchValue .~ (f $ asTextInput s).value

fullPrompt :: SearchSelect a -> Text
fullPrompt SearchSelect{..} = prompt <> mconcat [optionText o <> " " | o <- selections]

instance (Eq a, Show a) => Widget (SearchSelect a) where
    cursor = lens getter setter
      where
        getter :: SearchSelect a -> Position
        getter s
            | s.cursorRow == 0 = asTextInput s ^. cursor
            | otherwise = Position{row = s.cursorRow, col = 2}
        setter :: SearchSelect a -> Position -> SearchSelect a
        setter s Position{..} = s & #cursorRow .~ row
    handleEvent (KeyEvent (ArrowKey Upwards) []) s = moveUp s
    handleEvent (KeyEvent (ArrowKey Downwards) []) s = moveDown s
    handleEvent (KeyEvent BackspaceKey []) s | s.cursorRow == 0 && s.searchValue.cursor.posColumn == 0 = uncheckLast s
    handleEvent ev s | s.cursorRow == 0 = makeOptionsVisible $ overTextInput (handleEvent ev) s
    handleEvent (KeyEvent SpaceKey []) s
        | s.maxSelect == 1 = uncheckLast >>> flipCurrent >>> clearSearchValue $ s
        | numChecked s < s.maxSelect = flipCurrent >>> clearSearchValue $ s
    handleEvent _ s = s
    valid s = inRange (s.minSelect, s.maxSelect) $ numChecked s
    toText s =
        let mkOption SearchSelectOption{..} =
                mconcat
                    [ " "
                    , Text.intercalate
                        (if value `elem` s.selections then "*" else " ")
                        (if s.maxSelect > 1 then ["[", "]"] else ["(", ")"])
                    , " "
                    , s.optionText value
                    ]
         in Text.unlines $
                fullPrompt s
                    <> RopeZipper.toText s.searchValue
                    : (mkOption <$> filter (.visible) s.options)

clearSearchValue :: SearchSelect a -> SearchSelect a
clearSearchValue = #searchValue .~ ""

moveUp :: SearchSelect a -> SearchSelect a
moveUp s
    | s.cursorRow < 1 = s
    | otherwise = s & #cursorRow %~ pred

moveDown :: SearchSelect a -> SearchSelect a
moveDown s
    | s.cursorRow < numVisible = s & #cursorRow %~ succ
    | otherwise = s
  where
    numVisible = length $ filter (.visible) s.options

flipCurrent :: forall a. (Eq a) => SearchSelect a -> SearchSelect a
flipCurrent s
    | Just o <- current =
        s
            & #selections
            %~ if o.value `elem` s.selections
                then uncheck o.value
                else check o.value
    | otherwise = s
  where
    current = filter (.visible) s.options !? (s.cursorRow - 1)
    uncheck :: a -> [a] -> [a]
    uncheck v = filter (/= v)
    check :: a -> [a] -> [a]
    check v = (<> [v])

uncheckLast :: SearchSelect a -> SearchSelect a
uncheckLast s
    | Just (xs, _) <- unsnoc s.selections = s & (#selections .~ xs)
    | otherwise = s

numChecked :: SearchSelect a -> Int
numChecked = length . (.selections)

makeOptionsVisible :: forall a. (Show a) => SearchSelect a -> SearchSelect a
makeOptionsVisible s
    | Text.length filterText < s.minSearchLength =
        s & #options . traverse . #visible .~ False
    | otherwise = s & #options .~ newOptions
  where
    filterText = RopeZipper.toText s.searchValue
    score :: SearchSelectOption a -> Fuzzy.Fuzzy (SearchSelectOption a) Text
    score original =
        fromMaybe Fuzzy.Fuzzy{original, rendered = ishow original.value, score = -1} $
            Fuzzy.match filterText original "" "" (ishow . (.value)) False
    (newVisible, newInvisible) = splitAt s.maxVisible $ sortOn (Down . Fuzzy.score) $ score <$> s.options
    newOptions =
        (newVisible <&> (\x -> x.original & #visible .~ (x.score >= 0)))
            <> (newInvisible <&> ((.original) >>> #visible .~ False))
