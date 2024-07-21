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

data SearchSelect a = SearchSelect
    { prompt :: Text
    , searchValue :: RopeZipper
    , options :: [a]
    , visibleOptions :: [a]
    , selections :: [a]
    , optionText :: a -> Text
    , newOption :: Text -> Maybe a
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

filterText :: Lens' (SearchSelect a) Text
filterText = #searchValue . lens RopeZipper.toText (const RopeZipper.fromText)

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
    handleEvent ev s | s.cursorRow == 0 = textInput %~ handleEvent ev >>> updateVisibleOptions $ s
    handleEvent (KeyEvent SpaceKey []) s
        | s.maxSelect == 1 = uncheckLast >>> flipCurrent >>> clearSearchValue $ s
        | Just o <- s ^. current, o `elem` s.selections = flipCurrent s
        | numChecked s < s.maxSelect = flipCurrent >>> clearSearchValue $ s
    handleEvent _ s = s
    valid s = inRange (s.minSelect, s.maxSelect) $ numChecked s
    toDoc s =
        let prompt = fullPrompt s pretty (annotate inverted . pretty)
            mkOption a =
                mconcat
                    [ " "
                    , Text.intercalate
                        (if a `elem` s.selections then "*" else " ")
                        (if s.maxSelect > 1 then ["[", "]"] else ["(", ")"])
                    , " "
                    , s.optionText a
                    ]
            options =
                pretty . Text.unlines $
                    RopeZipper.toText s.searchValue : (mkOption <$> s.visibleOptions)
         in prompt <> options
    lineCount s = 1 + length s.visibleOptions

clearSearchValue :: SearchSelect a -> SearchSelect a
clearSearchValue = #searchValue .~ ""

moveUp :: SearchSelect a -> SearchSelect a
moveUp s = s & #cursorRow .~ max 0 (s.cursorRow - 1)

moveDown :: SearchSelect a -> SearchSelect a
moveDown s = s & #cursorRow .~ min numVisible (succ s.cursorRow)
  where
    numVisible = length s.visibleOptions

current :: Lens' (SearchSelect a) (Maybe a)
current = lens getter setter
  where
    getter :: SearchSelect a -> Maybe a
    getter s
        | s.cursorRow < 1 = Nothing
        | otherwise = s.visibleOptions !? (s.cursorRow - 1)
    setter :: SearchSelect a -> Maybe a -> SearchSelect a
    setter s mo
        | s.cursorRow < 1 = s
        | Just o <- mo = s & #visibleOptions . ix (s.cursorRow - 1) .~ o
        | otherwise = s & #cursorRow .~ 0

flipCurrent :: forall a. (Eq a) => SearchSelect a -> SearchSelect a
flipCurrent s
    | Just o <- s ^. current =
        s
            & ( #selections
                    %~ if o `elem` s.selections
                        then uncheck o
                        else check o
              )
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

updateVisibleOptions :: forall a. (Show a) => SearchSelect a -> SearchSelect a
updateVisibleOptions s
    | Text.length (s ^. filterText) < s.minSearchLength =
        s & #visibleOptions .~ []
    | otherwise = s & #visibleOptions .~ newVisible <> newOptions
  where
    score :: a -> Fuzzy.Fuzzy a Text
    score original =
        fromMaybe Fuzzy.Fuzzy{original, rendered = ishow original, score = -1} $
            Fuzzy.match (s ^. filterText) original "" "" ishow False
    (newVisible, _) =
        splitAt s.maxVisible
            . fmap (.original)
            . takeWhile ((0 <=) . Fuzzy.score)
            . sortOn (Down . Fuzzy.score)
            $ score
                <$> s.options
    newOptions = maybeToList . s.newOption $ s ^. filterText
