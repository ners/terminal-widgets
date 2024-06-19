{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.SelectSpec where

import System.Terminal
import System.Terminal.Widgets.Select
import Prelude

deriving stock instance Show (SelectOption Int)

deriving stock instance Eq (SelectOption Int)

deriving stock instance Show (Select Int)

deriving stock instance Eq (Select Int)

instance (Arbitrary a) => Arbitrary (SelectOption a) where
    arbitrary = do
        value <- arbitrary
        pure SelectOption{checked = False, ..}

instance (Arbitrary a, Show a) => Arbitrary (Select a) where
    arbitrary = do
        options <- (:) <$> arbitrary <*> arbitrary
        cursorOption <- chooseInt (0, length options - 1)
        pure
            Select
                { prompt = "prompt"
                , minSelect = 1
                , maxSelect = 1
                , cursorOption
                , optionText = ishow
                , ..
                }

spec :: Spec
spec = do
    prop "renders correctly" $ \(select :: Select Int) -> void $ runTestWidget select do
        assertCounter (`shouldNotBe` 0)
        testRandomMovements
        sendKeyEvent $ KeyEvent SpaceKey mempty
        assertCounter (`shouldNotBe` 0)
        assertWidget
            ( \w ->
                w
                    `shouldBe` ( select
                                    & (#cursorOption .~ w.cursorOption)
                                    & (#options . traverse . #checked .~ False)
                                    & (#options . ix w.cursorOption . #checked .~ True)
                               )
            )
        assertCounter (`shouldNotBe` 0)
