{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widget.ButtonsSpec where

import System.Terminal.Widget.Buttons
import Prelude

deriving stock instance Show Buttons

instance Arbitrary Buttons where
    arbitrary = do
        buttons <- (:) <$> arbitrary <*> arbitrary
        selected <- chooseInt (0, length buttons - 1)
        pure Buttons{prompt = "prompt", ..}

spec :: Spec
spec = do
    prop "renders correctly" $ \(buttons :: Buttons) -> void $ runTestWidget buttons do
        assertCounter (`shouldNotBe` 0)
