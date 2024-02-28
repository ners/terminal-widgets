{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.TextInputSpec where

import Control.Monad.Trans.Reader (asks)
import Data.Text qualified as Text
import System.Terminal
import System.Terminal.Internal
import System.Terminal.Widgets.Common (Widget (submitEvent))
import System.Terminal.Widgets.TextInput
import Prelude

deriving stock instance Eq TextInput

deriving stock instance Show TextInput

spec :: Spec
spec = do
    it "renders textinput correctly" do
        let input =
                TextInput
                    { prompt = "p:"
                    , value = ""
                    , valueTransform = id
                    , multiline = True
                    , required = False
                    }
        (term, input') <- runTestWidget' input do
            term <- asks (.terminal)
            forM_ (replicate 3 ()) $ \_ -> do
                void . sendEvent . Right $ KeyEvent (CharKey 'x') []
                void . sendEvent . Right $ KeyEvent EnterKey []
            screenLines <- fmap fromString <$> readTVarIO term.terminal.virtualWindow
            liftIO $ screenLines `shouldSatisfy` linesMatch ["p:x", "  x", "  x"]
            forM_ (replicate 100 ()) $ const . sendEvent . Right $ KeyEvent BackspaceKey []
            w <- sendEvent $ Right (KeyEvent BackspaceKey [])
            screenLines <- fmap fromString <$> readTVarIO term.terminal.virtualWindow
            liftIO $ screenLines `shouldSatisfy` linesMatch ["p:"]
            sendEvent . Right . fromJust . submitEvent $ w
            pure term
        readTVarIO term.commandCounter `shouldNotReturn` 0

lineMatches :: Text -> Text -> Bool
lineMatches x line = Text.justifyLeft (Text.length line) ' ' x == line

linesMatch :: [Text] -> [Text] -> Bool
linesMatch expected actual = and $ zipWith lineMatches (expected <> repeat "") actual
