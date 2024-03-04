{-# LANGUAGE OverloadedLists #-}

module System.Terminal.Widgets.SearchSelect where

import Data.Ord (Down (Down))
import Data.Text qualified as Text
import Data.Text.Rope.Zipper (RopeZipper)
import Data.Text.Rope.Zipper qualified as RopeZipper
import System.Terminal.Widgets.Common
import System.Terminal.Widgets.TextInput
import Text.Fuzzy qualified as Fuzzy
import Prelude

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

fullPrompt :: (Monoid b) => SearchSelect a -> (Text -> b) -> (Text -> b) -> b
fullPrompt SearchSelect{..} f g = f prompt <> mconcat (selection <$> selections)
  where
    selection o = g (" " <> optionText o <> " ") <> f " "

textInput :: Lens' (SearchSelect a) TextInput
textInput = lens getter setter
  where
    getter :: SearchSelect a -> TextInput
    getter s =
        TextInput
            { prompt = fullPrompt s id id
            , multiline = False
            , required = False
            , value = s.searchValue
            , valueTransform = id
            }
    setter :: SearchSelect a -> TextInput -> SearchSelect a
    setter s t = s & #searchValue .~ t.value

instance (Eq a, Show a) => Widget (SearchSelect a) where
    cursor = lens getter setter
      where
        getter :: SearchSelect a -> Position
        getter s
            | s.cursorRow == 0 = s ^. textInput . cursor
            | otherwise = Position{row = s.cursorRow, col = 2}
        setter :: SearchSelect a -> Position -> SearchSelect a
        setter s Position{..} = s & #cursorRow .~ row
    handleEvent (KeyEvent (ArrowKey Upwards) []) s = moveUp s
    handleEvent (KeyEvent (ArrowKey Downwards) []) s = moveDown s
    handleEvent (KeyEvent BackspaceKey []) s | s.cursorRow == 0 && s.searchValue.cursor.posColumn == 0 = uncheckLast s
    handleEvent ev s | s.cursorRow == 0 = textInput %~ handleEvent ev >>> makeOptionsVisible $ s
    handleEvent (KeyEvent SpaceKey []) s
        | s.maxSelect == 1 = uncheckLast >>> flipCurrent >>> clearSearchValue $ s
        | Just o <- s ^. current, o.value `elem` s.selections = flipCurrent s
        | numChecked s < s.maxSelect = flipCurrent >>> clearSearchValue $ s
    handleEvent _ s = s
    valid s = inRange (s.minSelect, s.maxSelect) $ numChecked s
    toDoc s =
        let prompt = fullPrompt s pretty (annotate inverted . pretty)
            mkOption SearchSelectOption{..} =
                mconcat
                    [ " "
                    , Text.intercalate
                        (if value `elem` s.selections then "*" else " ")
                        (if s.maxSelect > 1 then ["[", "]"] else ["(", ")"])
                    , " "
                    , s.optionText value
                    ]
            options =
                pretty . Text.unlines $
                    RopeZipper.toText s.searchValue : (mkOption <$> filter (.visible) s.options)
         in prompt <> options

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

current :: Lens' (SearchSelect a) (Maybe (SearchSelectOption a))
current = lens getter setter
  where
    getter :: SearchSelect a -> Maybe (SearchSelectOption a)
    getter s
        | s.cursorRow < 1 = Nothing
        | otherwise = filter (.visible) s.options !? (s.cursorRow - 1)
    setter :: SearchSelect a -> Maybe (SearchSelectOption a) -> SearchSelect a
    setter s mo
        | s.cursorRow < 1 = s
        | Just o <- mo = s & #options . ix (s.cursorRow - 1) .~ o
        | otherwise = s & #cursorRow .~ 0

flipCurrent :: forall a. (Eq a) => SearchSelect a -> SearchSelect a
flipCurrent s
    | Just o <- s ^. current =
        s
            & #selections
            %~ if o.value `elem` s.selections
                then uncheck o.value
                else check o.value
    | otherwise = s
  where
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
        (newVisible <&> (\x -> x.original & #visible .~ (x.score > 0)))
            <> (newInvisible <&> ((.original) >>> #visible .~ False))
