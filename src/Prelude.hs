{-# OPTIONS_GHC -Wno-orphans #-}

module Prelude
    ( module Control.Applicative
    , module Control.Arrow
    , module Control.Monad
    , module Control.Monad.Catch
    , module Control.Monad.Reader
    , module Control.Monad.State
    , module Control.Monad.Trans
    , module Control.Monad.Trans.Class
    , module Data.Bits
    , module Data.Foldable
    , module Data.Function
    , module Data.Functor
    , module Data.Generics.Internal.VL
    , module Data.Ix
    , module Data.List
    , module Data.List.Extra
    , module Data.Maybe
    , module Data.Monoid
    , module Data.String
    , module Data.Text
    , module Prettyprinter
    , module Debug.Trace
    , module GHC.Exts
    , module GHC.Generics
    , module Prelude
    , module System.Exit
    , module System.IO
    , module System.Terminal
    , module System.Terminal.Internal
    )
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Data.Bits ((.&.))
import Data.Coerce
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
import Data.Monoid hiding (Alt)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Rope.Zipper (RopeZipper)
import Data.Text.Rope.Zipper qualified as RopeZipper
import Debug.Trace
import GHC.Exts (IsList (..), fromList)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), annotate)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (BufferMode (NoBuffering), IO, hSetBuffering, stdout)
import System.Terminal
import System.Terminal.Internal (LocalTerminal, Terminal)
import "base" Prelude hiding (putChar)

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

(#.) :: (Coercible c b) => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce (\x -> x :: b) :: forall a b. (Coercible b a) => a -> b

type Getting r s a = (a -> Const r a) -> s -> Const r s

(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)

is :: a -> Getting (First c) a c -> Bool
a `is` c = isJust $ a ^? c
