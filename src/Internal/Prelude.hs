{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Prelude
    ( module Control.Arrow
    , module Control.Monad
    , module Control.Monad.Catch
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Trans
    , module Control.Monad.Trans.Class
    , module Data.Bits
    , module Data.Ix
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.List
    , module Data.List.Extra
    , module Data.Maybe
    , module Data.String
    , module Data.Text
    , module GHC.Generics
    , module GHC.IsList
    , module Data.Generics.Internal.VL
    , module Prelude
    , module System.Exit
    , module System.IO
    , module System.Terminal
    , module System.Terminal.Internal
    , ishow
    , fromText
    , commonPrefixes
    , (%~)
    , filtered
    , ix
    )
where

import Control.Arrow
import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, evalStateT, gets, modify)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Data.Bits ((.&.))
import Data.Foldable hiding (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.Generics.Internal.VL
import Data.Generics.Labels ()
import Data.Generics.Product qualified as Lens
import Data.Ix
import Data.List (findIndex, intersperse, uncons)
import Data.List.Extra
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

instance {-# OVERLAPPING #-} Lens.HasField' "cursor" RopeZipper Position where
    field' = lens getter setter
      where
        getter :: RopeZipper -> Position
        getter ((.cursor) -> c) = Position{row = fromIntegral c.posLine, col = fromIntegral c.posColumn}
        setter :: RopeZipper -> Position -> RopeZipper
        setter rz Position{..} =
            RopeZipper.setCursor
                RopeZipper.Position{posLine = fromIntegral row, posColumn = fromIntegral col}
                rz

instance {-# OVERLAPPING #-} Lens.HasField "cursor" RopeZipper RopeZipper Position Position where
    field = Lens.field' @"cursor" @RopeZipper @Position

deriving stock instance Generic RopeZipper.Position

commonPrefixes :: Text -> Text -> (Text, Text, Text)
commonPrefixes t1 t2 = fromMaybe ("", t1, t2) $ Text.commonPrefixes t1 t2

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
