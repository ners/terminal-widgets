{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-monomorphism-restriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Prelude
    ( module Prelude
    , module Control.Arrow
    , module Control.Monad
    , module Control.Monad.Trans.Reader
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.Generics.Internal.VL
    , module Data.Maybe
    , module Data.Ord
    , module Data.String
    , module Data.Text
    , module Data.Traversable
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
import Test.HUnit.Base
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Foldable (for_, toList)
import Data.Function
import Data.Functor
import Data.Functor.Identity (Identity)
import Data.Generics.Internal.VL
import Data.Generics.Labels ()
import Data.Maybe
import Data.Ord (clamp)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Strict
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Zipper (TextZipper)
import Data.Text.Lazy.Zipper qualified as TextZipper
import Data.Traversable (for)
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
import "base" Prelude hiding (unzip)

infixl 4 <$$>

(<$$>) :: Functor f1 => Functor f2 => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

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
    , commandCounter :: TVar Int
    , cursorCommands :: TVar [Command]
    }

instance (Terminal t) => Terminal (CountingTerminal t) where
    termType = termType . (.terminal)
    termEvent = termEvent . (.terminal)
    termInterrupt = termInterrupt . (.terminal)
    termCommand t c = do
        let commandLength = Text.length . defaultEncode $ c
        atomically $ modifyTVar t.commandCounter (+ commandLength)
        let isCursorCommand :: Bool
            isCursorCommand =
                case c of
                    MoveCursorUp{} -> True
                    MoveCursorDown{} -> True
                    MoveCursorForward{} -> True
                    MoveCursorBackward{} -> True
                    SetCursorRow{} -> True
                    SetCursorColumn{} -> True
                    HideCursor{} -> True
                    ShowCursor{} -> True
                    SaveCursor{} -> True
                    RestoreCursor{} -> True
                    SetCursorPosition{} -> True
                    GetCursorPosition{} -> True
                    _ -> False
        when isCursorCommand $ atomically $ modifyTVar t.cursorCommands (c :)
        termCommand t.terminal c
    termFlush _ = pure ()
    termGetWindowSize = termGetWindowSize . (.terminal)
    termGetCursorPosition = termGetCursorPosition . (.terminal)

type EventOrInterrupt = Either Interrupt Event

data TestState w = TestTerminal
    { eventQueue :: TQueue EventOrInterrupt
    , done :: TMVar ()
    , terminal :: CountingTerminal VirtualTerminal
    , widget :: TVar w
    }

getCursor :: (MonadIO m) => ReaderT (TestState w) m Position
getCursor = readTVarIO =<< asks (.terminal.terminal.virtualCursor)

assertCursor :: (HasCallStack, MonadIO m) => (HasCallStack => Position -> Expectation) -> ReaderT (TestState w) m ()
assertCursor expectation = liftIO . expectation =<< getCursor

getCursorCommands :: (MonadIO m) => ReaderT (TestState w) m [Command]
getCursorCommands = readTVarIO =<< asks (.terminal.cursorCommands)

assertCursorCommands :: (HasCallStack, MonadIO m) => (HasCallStack => [Command] -> Expectation) -> ReaderT (TestState w) m ()
assertCursorCommands expectation = liftIO . expectation =<< getCursorCommands

getContent :: (MonadIO m) => ReaderT (TestState w) m [Text]
getContent = fromString <$$> (readTVarIO =<< asks (.terminal.terminal.virtualWindow))

assertContent :: (HasCallStack, MonadIO m) => (HasCallStack => [Text] -> Expectation) -> ReaderT (TestState w) m ()
assertContent expectation = liftIO . expectation =<< getContent

getCounter :: (MonadIO m) => ReaderT (TestState w) m Int
getCounter = readTVarIO =<< asks (.terminal.commandCounter)

resetCounter :: (MonadIO m) => ReaderT (TestState w) m ()
resetCounter = do
    atomically . flip writeTVar 0 =<< asks (.terminal.commandCounter)
    atomically . flip writeTVar [] =<< asks (.terminal.cursorCommands)

assertCounter :: (HasCallStack, MonadIO m) => (HasCallStack => Int -> Expectation) -> ReaderT (TestState w) m ()
assertCounter expectation = liftIO . expectation =<< getCounter

getWidget :: (MonadIO m) => ReaderT (TestState w) m w
getWidget = readTVarIO =<< asks (.widget)

assertWidget :: (HasCallStack, MonadIO m) => (HasCallStack => w -> Expectation) -> ReaderT (TestState w) m ()
assertWidget expectation = liftIO . expectation =<< getWidget

runTestWidget
    :: (Widget w)
    => w
    -> ReaderT (TestState w) IO a
    -> IO (a, w)
runTestWidget w runEvents = do
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
    widget <- newTVarIO w
    commandCounter <- newTVarIO 0
    cursorCommands <- newTVarIO []
    withVirtualTerminal settings $ \terminal -> do
        let countingTerminal = CountingTerminal{..}
        let testTerminal = TestTerminal{terminal = countingTerminal, ..}
        concurrently (atomically (takeTMVar done) >> runReaderT (runEvents <* sendSubmitEvent) testTerminal) do
            let setup _ = pure ()
                cleanup _ _ = pure ()
                preRender _ _ = pure ()
                postRender _ w' = flush >> updateWidget w'
                updateWidget w' = atomically $ putTMVar done () >> writeTVar widget w'
            flip runTerminalT countingTerminal do
                w' <- runWidget' setup preRender postRender cleanup w
                updateWidget w'
                pure w'

sendEvent :: (MonadIO m) => EventOrInterrupt -> ReaderT (TestState w) m ()
sendEvent e = do
    TestTerminal{..} <- ask
    atomically $ writeTQueue eventQueue e
    atomically $ takeTMVar done

sendKeyEvent :: (MonadIO m) => Event -> ReaderT (TestState w) m ()
sendKeyEvent = sendEvent . Right

sendEvents
    :: (Traversable f, MonadIO m)
    => f EventOrInterrupt
    -> ReaderT (TestState w) m ()
sendEvents es = last . toList <$> for es sendEvent

sendKeyEvents
    :: (Traversable f, MonadIO m)
    => f Event
    -> ReaderT (TestState w) m ()
sendKeyEvents es = last . toList <$> for es sendKeyEvent

sendSubmitEvent :: (Widget w, MonadIO m) => ReaderT (TestState w) m ()
sendSubmitEvent = sendKeyEvent . fromJust . submitEvent =<< getWidget

deriving stock instance Bounded Direction

deriving stock instance Enum Direction

newtype MovementKey = MovementKey Key
    deriving newtype (Show)

instance Arbitrary MovementKey where
    arbitrary = elements $ MovementKey <$> HomeKey : EndKey : arrowKeys
      where
        arrowKeys = ArrowKey <$> [minBound .. maxBound]

movementEvent :: MovementKey -> EventOrInterrupt
movementEvent (MovementKey key) = Right $ KeyEvent key mempty

sendRandomMovements :: ReaderT (TestState w) IO Int
sendRandomMovements = do
    movementKeys :: [MovementKey] <- liftIO (generate arbitrary)
    sendEvents $ movementEvent <$> movementKeys
    pure $ length movementKeys

testRandomMovements :: ReaderT (TestState w) IO ()
testRandomMovements = do
    resetCounter
    w <- sendRandomMovements
    assertCounter (`shouldBeLTE` (w * 4))
    assertCursorCommands $ (`shouldBeLTE` w) . length

data BinaryOp a = BinaryOp {
    op :: a -> a -> Bool,
    symbol :: String
}

assertBinaryOp
  :: (HasCallStack, Show a)
  => String -- ^ The message prefix
  -> BinaryOp a
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> IO ()
assertBinaryOp preface BinaryOp{..} a b =
  unless (op a b) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected " ++ show a ++ " " ++ symbol ++ " " ++ show b

shouldBeLT :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeLT = assertBinaryOp "" (BinaryOp (<) "<")

shouldBeLTE :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeLTE = assertBinaryOp "" (BinaryOp (<=) "<=")

shouldBeGT :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeGT = assertBinaryOp "" (BinaryOp (>) ">")

shouldBeGTE :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeGTE = assertBinaryOp "" (BinaryOp (>=) ">=")

shouldBeEQ :: (HasCallStack, Ord a, Show a) => a -> a -> Expectation
shouldBeEQ = assertBinaryOp "" (BinaryOp (==) "==")
