module Prelude
    ( module Prelude
    , module Data.String
    , module Data.Text
    )
where

import Data.String
import Data.Text (Text)
import Text.Show.Functions ()
import "base" Prelude hiding (unzip)

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show
