{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.ButtonsSpec where

import System.Terminal
import System.Terminal.Widgets.Buttons
import Prelude

deriving stock instance Show Buttons

instance Arbitrary Buttons where
    arbitrary = do
        buttons <- arbitrary
        selected <- chooseInt (0, length buttons - 1)
        pure Buttons{prompt = "prompt", ..}

spec :: Spec
spec = do
    prop "renders buttons correctly" $ \(buttons :: Buttons) -> do
        (term, buttons') <- runTestWidget buttons [Right (KeyEvent EnterKey [])]
        buttons' `shouldBe` buttons
        readTVarIO term.commandCounter `shouldNotReturn` 0
