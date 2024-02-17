{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Prelude
    ( module Control.Arrow
    , module Control.Lens.Combinators
    , module Control.Lens.Operators
    , module Control.Monad
    , module Control.Monad.Catch
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Trans
    , module Control.Monad.Trans.Class
    , module Data.Bits
    , module Data.Foldable
    , module Data.List
    , module Data.Maybe
    , module Data.String
    , module Data.Text
    , module GHC.Generics
    , module GHC.IsList
    , module Prelude
    , module System.Exit
    , module System.IO
    , module System.Terminal
    , module System.Terminal.Internal
    , ishow
    , fromText
    , commonPrefixes
    )
where

import Control.Arrow
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Data.Bits ((.&.))
import Data.Foldable hiding (toList)
import Data.Generics.Labels ()
import Data.Generics.Product qualified as Lens
import Data.List (findIndex, intersperse)
import Data.Maybe hiding (mapMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Rope.Zipper (RopeZipper)
import Data.Text.Rope.Zipper qualified as RopeZipper
import GHC.Generics (Generic)
import GHC.IsList
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (BufferMode (NoBuffering), IO, hSetBuffering, stdout)
import System.Terminal
import System.Terminal.Internal (LocalTerminal, Terminal)
import Prelude hiding (putChar)

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

fromText :: (IsString s) => Text -> s
fromText = fromString . fromText

instance Lens.HasField' "cursor" RopeZipper RopeZipper.Position where
    field' = lens RopeZipper.cursor (flip RopeZipper.setCursor)

instance
    {-# OVERLAPPING #-}
    Lens.HasField
        "cursor"
        RopeZipper
        RopeZipper
        RopeZipper.Position
        RopeZipper.Position
    where
    field = lens RopeZipper.cursor (flip RopeZipper.setCursor)

deriving stock instance Generic RopeZipper.Position

commonPrefixes :: Text -> Text -> (Text, Text, Text)
commonPrefixes t1 t2 = fromMaybe ("", t1, t2) $ Text.commonPrefixes t1 t2
