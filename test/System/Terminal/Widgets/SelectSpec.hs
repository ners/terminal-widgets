{-# LANGUAGE OverloadedLists #-}
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
        cursorRow <- chooseInt (1, length options)
        pure
            Select
                { prompt = "prompt"
                , minSelect = 1
                , maxSelect = 1
                , cursorRow
                , optionText = ishow
                , ..
                }

spec :: Spec
spec = do
    prop "renders select correctly" $ \(select :: Select Int) -> do
        (term, select') <-
            runTestWidget
                select
                [Right (KeyEvent SpaceKey []), Right (KeyEvent EnterKey [])]
        select'
            `shouldBe` (select & #options . ix (select.cursorRow - 1) . #checked .~ True)
        readTVarIO term.commandCounter `shouldNotReturn` 0
