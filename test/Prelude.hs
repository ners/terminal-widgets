{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Prelude
    ( module Prelude
    , module Control.Arrow
    , module Control.Monad
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.Generics.Internal.VL
    , module Data.List.Extra
    , module Data.Maybe
    , module Data.Ord
    , module Data.String
    , module Data.Text
    , module Debug.Trace
    , module Test.Hspec
    , module Test.Hspec.QuickCheck
    , module Test.QuickCheck
    , module UnliftIO
    , module UnliftIO.Async
    , module UnliftIO.STM
    )
where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Foldable (for_)
import Data.Function
import Data.Functor
import Data.Functor.Identity (Identity)
import Data.Generics.Internal.VL
import Data.Generics.Labels ()
import Data.List.Extra
import Data.Maybe
import Data.Ord (clamp)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Strict
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Zipper (TextZipper)
import Data.Text.Lazy.Zipper qualified as TextZipper
import Debug.Trace
import System.Terminal
import System.Terminal.Internal
import System.Terminal.Widgets.Common
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Show.Functions ()
import UnliftIO
import UnliftIO.Async
import UnliftIO.STM
import "base" Prelude

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

fromText :: (IsString s) => Text -> s
fromText = fromString . fromText

instance Eq (a -> b) where
    _ == _ = True

infixr 4 %~

(%~) :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
(%~) = over

filtered :: (Applicative f) => (a -> Bool) -> (a -> f a) -> a -> f a
filtered p f s = if p s then f s else pure s

ix :: (Ord a, Num a, Applicative f) => a -> (t -> f t) -> [t] -> f [t]
ix k f xs0
    | k < 0 = pure xs0
    | otherwise = go xs0 k
  where
    go [] _ = pure []
    go (a : as) 0 = (: as) <$> f a
    go (a : as) i = (a :) <$> (go as $! i - 1)

instance Arbitrary Strict.Text where
    arbitrary = fromString <$> listOf (elements "abcdefg\n")

instance Arbitrary Lazy.Text where
    arbitrary = Lazy.fromStrict <$> arbitrary

instance Arbitrary TextZipper where
    arbitrary = TextZipper.fromParts <$> arbitrary <*> arbitrary

data CountingTerminal t = CountingTerminal
    { terminal :: t
    , commandCounter :: TVar Integer
    }

instance (Terminal t) => Terminal (CountingTerminal t) where
    termType = termType . (.terminal)
    termEvent = termEvent . (.terminal)
    termInterrupt = termInterrupt . (.terminal)
    termCommand t c = do
        let commandLength = fromIntegral . Text.length . defaultEncode $ c
        atomically $ modifyTVar t.commandCounter (+ commandLength)
        termCommand t.terminal c
    termFlush _ = pure ()
    termGetWindowSize = termGetWindowSize . (.terminal)
    termGetCursorPosition = termGetCursorPosition . (.terminal)

type EventOrInterrupt = Either Interrupt Event

data TestState w = TestTerminal
    { eventQueue :: TQueue EventOrInterrupt
    , done :: TMVar w
    , terminal :: CountingTerminal VirtualTerminal
    }

runTestWidget'
    :: (Widget w)
    => w
    -> ReaderT (TestState w) IO a
    -> IO (a, w)
runTestWidget' w runEvents = do
    eventQueue <- newTQueueIO
    let settings =
            VirtualTerminalSettings
                { virtualType = ""
                , virtualWindowSize = pure $ Size 80 25
                , virtualEvent =
                    peekTQueue eventQueue >>= \case
                        Left _ -> retrySTM
                        Right e -> readTQueue eventQueue >> pure e
                , virtualInterrupt =
                    peekTQueue eventQueue >>= \case
                        Left i -> readTQueue eventQueue >> pure i
                        Right _ -> retrySTM
                }
    done <- newEmptyTMVarIO
    commandCounter <- newTVarIO 0
    withVirtualTerminal settings $ \terminal -> do
        let countingTerminal = CountingTerminal{..}
        let testTerminal = TestTerminal{terminal = countingTerminal, ..}
        concurrently (atomically (takeTMVar done) >> runReaderT runEvents testTerminal) do
            let setup _ = pure ()
                cleanup _ _ = pure ()
                preRender _ _ = pure ()
                postRender _ w' = do
                    flush
                    atomically $ putTMVar done w'
            flip runTerminalT countingTerminal do
                w' <- runWidget' setup preRender postRender cleanup w
                atomically $ putTMVar done w'
                pure w'

sendEvent :: EventOrInterrupt -> ReaderT (TestState w) IO w
sendEvent e = do
    TestTerminal{..} <- ask
    atomically $ writeTQueue eventQueue e
    atomically $ takeTMVar done

runTestWidget
    :: (Widget w)
    => w
    -> [EventOrInterrupt]
    -> IO (CountingTerminal VirtualTerminal, w)
runTestWidget w events = runTestWidget' w do
    mapM_ sendEvent events
    asks (.terminal)
