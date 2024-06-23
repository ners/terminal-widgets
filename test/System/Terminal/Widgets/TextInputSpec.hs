{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.TextInputSpec where

import Data.Text qualified as Text
import System.Terminal
import System.Terminal.Widgets.Common qualified as Widget
import System.Terminal.Widgets.TextInput
import Prelude

deriving stock instance Eq TextInput

deriving stock instance Show TextInput

spec :: Spec
spec = do
    it "renders correctly" do
        let prompt = "p:" :: Text
            promptPrefix = Text.map (const ' ') prompt
            char = 'x'
            charText = Text.singleton char
            input =
                TextInput
                    { prompt
                    , value = ""
                    , valueTransform = id
                    , multiline = True
                    , required = False
                    }
        void $ runTestWidget input do
            assertCounter (`shouldNotBe` 0)
            for_ [0 .. 3] $ \i -> do
                resetCounter
                void . sendKeyEvent $ KeyEvent (CharKey char) mempty
                sendKeyEvent $ KeyEvent EnterKey mempty
                let expectedCursor = Position{row = i + 1, col = 2}
                actualWidgetCursor <- view Widget.cursor <$> getWidget
                liftIO $ actualWidgetCursor `shouldBe` expectedCursor
                assertCursor (`shouldBe` expectedCursor)
                let expectedLines = prompt <> charText : replicate i (promptPrefix <> charText)
                assertContent (`shouldSatisfy` linesMatch expectedLines)
                assertCursorCommands (`shouldBe` [])

            testRandomMovements

            sendKeyEvents . join . replicate 10 $
                flip KeyEvent mempty <$> [BackspaceKey, DeleteKey]
            assertContent (`shouldSatisfy` linesMatch [prompt])

lineMatches :: Text -> Text -> Bool
lineMatches x line = Text.justifyLeft (Text.length line) ' ' x == line

linesMatch :: [Text] -> [Text] -> Bool
linesMatch expected actual = and $ zipWith lineMatches (expected <> repeat "") actual
