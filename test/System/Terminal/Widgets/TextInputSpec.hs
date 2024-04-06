{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.TextInputSpec where

import Control.Monad.Trans.Reader (asks)
import Data.Text qualified as Text
import System.Terminal
import System.Terminal.Internal
import System.Terminal.Widgets.Common (Widget (submitEvent), cursor)
import System.Terminal.Widgets.TextInput
import Prelude

deriving stock instance Eq TextInput

deriving stock instance Show TextInput

spec :: Spec
spec = do
    it "renders textinput correctly" do
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
        (term, _input') <- runTestWidget' input do
            term <- asks (.terminal)
            for_ [0 .. 3] $ \i -> do
                void . sendEvent . Right $ KeyEvent (CharKey char) mempty
                input' <- sendEvent . Right $ KeyEvent EnterKey mempty
                let expectedCursor = Position{row = i + 1, col = 2}
                let actualWidgetCursor = view cursor input'
                liftIO $ actualWidgetCursor `shouldBe` expectedCursor
                actualCursor <- readTVarIO term.terminal.virtualCursor
                liftIO $ actualCursor `shouldBe` expectedCursor
                let expectedLines = prompt <> charText : replicate i (promptPrefix <> charText)
                actualLines <- fmap fromString <$> readTVarIO term.terminal.virtualWindow
                liftIO $ actualLines `shouldSatisfy` linesMatch expectedLines
            for_ (replicate 10 ()) $
                const . sendEvent . Right $
                    KeyEvent BackspaceKey mempty
            w <- sendEvent $ Right (KeyEvent BackspaceKey mempty)
            screenLines <- fmap fromString <$> readTVarIO term.terminal.virtualWindow
            liftIO $ screenLines `shouldSatisfy` linesMatch [prompt]
            sendEvent . Right . fromJust . submitEvent $ w
            pure term
        readTVarIO term.commandCounter `shouldNotReturn` 0

lineMatches :: Text -> Text -> Bool
lineMatches x line = Text.justifyLeft (Text.length line) ' ' x == line

linesMatch :: [Text] -> [Text] -> Bool
linesMatch expected actual = and $ zipWith lineMatches (expected <> repeat "") actual
