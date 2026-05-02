{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Modifier where

import Prelude

data Modifier = Shift | Ctrl | Alt | Meta
    deriving stock (Bounded, Enum)

toModifiers :: Modifier -> Modifiers
toModifiers Shift = shiftKey
toModifiers Ctrl = ctrlKey
toModifiers Alt = altKey
toModifiers Meta = metaKey

instance IsList Modifiers where
    type Item Modifiers = Modifier
    fromList = mconcat . fmap toModifiers
    toList mods = filter hasMod [minBound .. maxBound]
      where
        hasMod c = mods .&. toModifiers c /= mempty
