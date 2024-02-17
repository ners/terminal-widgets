{-# LANGUAGE OverloadedLists #-}

module System.Terminal.Widgets.Buttons where

import Data.Char (toLower)
import Data.Generics.Product qualified as Lens
import Data.Text qualified as Text
import GHC.Records qualified as GHC
import Internal.Prelude
import Prettyprinter (Pretty (pretty), annotate)
import System.Terminal.Render
import System.Terminal.Widgets.Common

data Buttons = Buttons
    { prompt :: !Text
    , buttons :: ![(Text, Maybe Char)]
    , selected :: !Int
    }
    deriving stock (Generic, Eq)

instance GHC.HasField "cursor" Buttons Cursor where
    getField b = Cursor{col = b.selected, row = 1}

instance {-# OVERLAPPING #-} Lens.HasField "cursor" Buttons Buttons Cursor Cursor where
    field = lens (.cursor) (\t Cursor{..} -> t & #selected .~ col)

instance Widget Buttons where
    handleEvent (KeyEvent (ArrowKey Leftwards) []) = moveLeft
    handleEvent (KeyEvent (ArrowKey Rightwards) []) = moveRight
    handleEvent (KeyEvent (CharKey k) []) = handleAccessKey k
    handleEvent _ = id
    valid = const True
    submitEvent _ = Just $ KeyEvent EnterKey []
    toText _ = undefined
    lineCount _ = 1
    render (maybeOld, new) = do
        when (isNothing maybeOld) do
            hideCursor
            putText new.prompt
        setCursorColumn $ Text.length new.prompt
        forM_ (zip [0 ..] new.buttons) $ \(i, (label, accessKey)) -> do
            putText " "
            let (prefix, maybeSuffix) = second Text.uncons $ Text.break (accessKey `matches`) label
            let labelDoc =
                    case maybeSuffix of
                        Just (c, suffix) -> mconcat [pretty prefix, annotate underlined $ pretty c, pretty suffix]
                        Nothing -> pretty prefix
            let doc = mconcat ["[ ", labelDoc, " ]"]
            if i == new.selected
                then putDoc $ annotate inverted doc
                else putDoc doc
            putText " "

moveLeft :: Buttons -> Buttons
moveLeft = filtered (\s -> s.selected > 0) . #selected %~ pred

moveRight :: Buttons -> Buttons
moveRight = filtered (\s -> s.selected < length s.buttons - 1) . #selected %~ succ

matches :: Maybe Char -> Char -> Bool
matches Nothing _ = False
matches (Just k) c = toLower k == toLower c

handleAccessKey :: Char -> Buttons -> Buttons
handleAccessKey k b = b & #selected .~ selected
  where
    selected = fromMaybe b.selected $ findIndex ((`matches` k) . snd) b.buttons
