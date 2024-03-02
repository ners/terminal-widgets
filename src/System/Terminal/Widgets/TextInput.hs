{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module System.Terminal.Widgets.TextInput where

import Data.Text qualified as Text
import Data.Text.Rope.Zipper (RopeZipper)
import Data.Text.Rope.Zipper qualified as RopeZipper
import System.Terminal.Widgets.Common
import Prelude

data TextInput = TextInput
    { prompt :: Text
    , multiline :: Bool
    , required :: Bool
    , value :: RopeZipper
    , valueTransform :: Text -> Text
    }
    deriving stock (Generic)

instance Widget TextInput where
    cursor = lens getter setter
      where
        getter :: TextInput -> Position
        getter TextInput{..} = value ^. #cursor & #col %~ (+ Text.length prompt)
        setter :: TextInput -> Position -> TextInput
        setter t p = t & #value . #cursor .~ (p & #col %~ subtract (Text.length t.prompt))
    handleEvent (KeyEvent BackspaceKey []) = #value %~ RopeZipper.deleteBefore
    handleEvent (KeyEvent DeleteKey []) = #value %~ RopeZipper.deleteAfter
    handleEvent (KeyEvent (CharKey k) []) = #value %~ RopeZipper.insertText (Text.singleton k)
    handleEvent (KeyEvent (ArrowKey Leftwards) []) = #value %~ RopeZipper.moveBackward
    handleEvent (KeyEvent (ArrowKey Rightwards) []) = #value %~ RopeZipper.moveForward
    handleEvent (KeyEvent EnterKey []) = filtered (.multiline) . #value %~ RopeZipper.insertText "\n"
    handleEvent (KeyEvent (ArrowKey Upwards) []) = filtered (.multiline) . #value %~ RopeZipper.moveUp
    handleEvent (KeyEvent (ArrowKey Downwards) []) = filtered (.multiline) . #value %~ RopeZipper.moveDown
    handleEvent _ = id
    submitEvent t
        | valid t = Just $ KeyEvent EnterKey $ fromList [Alt | t.multiline]
        | otherwise = Nothing
    valid TextInput{..} = not $ required && RopeZipper.null value
    toDoc TextInput{..} =
        pretty
            . Text.intercalate "\n"
            . padLines
            . Text.split (== '\n')
            . valueTransform
            . RopeZipper.toText
            $ value
      where
        padLines :: [Text] -> [Text]
        padLines (x : xs) = (prompt <> x) : ((Text.replicate (Text.length prompt) " " <>) <$> xs)
        padLines [] = [prompt]
    lineCount TextInput{..} = fromIntegral $ max 1 $ RopeZipper.lengthInLines value
