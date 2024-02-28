{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.SelectSpec where

import Control.Monad.Trans.Reader (asks)
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
        cursorOption <- chooseInt (0, length options -1)
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
    prop "renders select correctly" $ \(select :: Select Int) -> do
        (term, select') <- runTestWidget' select do
            term <- asks (.terminal)
            c1 <- readTVarIO term.commandCounter
            void $ sendEvent $ Right (KeyEvent SpaceKey [])
            c2 <- readTVarIO term.commandCounter
            liftIO $ c2 - c1 `shouldBe` 0
            void $ sendEvent $ Right (KeyEvent EnterKey [])
            pure term
        select'
            `shouldBe` ( select
                            & (#options . traverse . #checked .~ False)
                            & (#options . ix select.cursorOption . #checked .~ True)
                       )
        readTVarIO term.commandCounter `shouldNotReturn` 0
