{-# LANGUAGE OverloadedLists #-}

module System.Terminal.Widgets.Select where

import Data.Generics.Product qualified as Lens
import Data.Text qualified as Text
import GHC.Records qualified as GHC
import Internal.Prelude
import System.Terminal.Render
import System.Terminal.Widgets.Common

data Select = Select
    { prompt :: !Text
    , options :: ![(Text, Bool)]
    , multiselect :: !Bool
    , cursorRow :: !Int
    }
    deriving stock (Generic, Eq)

instance GHC.HasField "cursor" Select Cursor where
    getField ((.cursorRow) -> row) = Cursor{col = 2, ..}

instance {-# OVERLAPPING #-} Lens.HasField "cursor" Select Select Cursor Cursor where
    field = lens (.cursor) (\t Cursor{..} -> t & #cursorRow .~ row)

instance Widget Select where
    toText s =
        let mkOption (t, selected) =
                Text.concat
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
flipCurrent s = s & #options . ix (s.cursorRow - 1) . _2 %~ not

unsetAll :: Select -> Select
unsetAll = #options . traverse . _2 .~ False
