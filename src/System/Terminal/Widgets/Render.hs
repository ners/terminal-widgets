{-# LANGUAGE UndecidableInstances #-}

module System.Terminal.Render where

import Data.Generics.Product qualified as Lens
import Data.Text qualified as Text
import Prettyprinter
import GHC.Records qualified as GHC
import Internal.Prelude

data Cursor = Cursor
    { row :: !Int
    , col :: !Int
    }
    deriving stock (Generic, Eq, Show)

type HasCursor w =
    ( GHC.HasField "cursor" w Cursor
    , Lens.HasField "cursor" w w Cursor Cursor
    )

type MonadCursor t m m' =
    ( MonadTrans t
    , MonadScreen m'
    , m ~ t m'
    , MonadState Cursor m
    )

data RenderInstruction m
    = Char Char
    | Text Text
    | Push (Attribute m)
    | Pop
    | NewLine

deriving stock instance (Show (Attribute m)) => Show (RenderInstruction m)

deriving stock instance (Eq (Attribute m)) => Eq (RenderInstruction m)

type Line m = [RenderInstruction m]

renderDoc
    :: (MonadTerminal m)
    => Maybe (Cursor, Doc (Attribute m))
    -> Doc (Attribute m)
    -> m ()
renderDoc maybeOld new = flip evalStateT (maybe (Cursor 0 0) fst maybeOld) do
    pure ()

docToRenderInstructions
    :: LayoutOptions -> Doc (Attribute m) -> [RenderInstruction m]
docToRenderInstructions opts = go . layoutSmart opts
  where
    go :: SimpleDocStream (Attribute m) -> [RenderInstruction m]
    go SFail = []
    go SEmpty = []
    go (SChar c xx) = Char c : go xx
    go (SText _ t xx) = Text t : go xx
    go (SLine i xx) = NewLine : Text (Text.replicate i " ") : go xx
    go (SAnnPush a xx) = Push a : go xx
    go (SAnnPop xx) = Pop : go xx

splitRenderInstructions :: [RenderInstruction m] -> [Line m]
splitRenderInstructions = snd . go []
  where
    go :: [Attribute m] -> [RenderInstruction m] -> (Line m, [Line m])
    go attrs (Push a : is) = first (Push a :) $ go (a : attrs) is
    go (_ : attrs) (Pop : is) = first (Pop :) $ go attrs is
    go [] (Pop : is) = go [] is
    go attrs (NewLine : is) =
        let (currentLine, lines) = go [] is
            pushs = Push <$> attrs
            pops = Pop <$ attrs
         in (pushs, reverse (pops <> currentLine) : lines)
    go attrs (i : is) = first (i :) $ go attrs is
    go _ [] = ([], [])

docToLines :: LayoutOptions -> Doc (Attribute m) -> [Line m]
docToLines opts = splitRenderInstructions . docToRenderInstructions opts

{-
 - [Attribute m]

    f [] sdoc
    where
        f _       SFail          = pure ()
        f _       SEmpty         = pure ()
        f    aa  (SChar c    xx) = putChar c                             >> f    aa  xx
        f    aa  (SText _ t  xx) = putText t                             >> f    aa  xx
        f    aa  (SLine i    xx) = putLn >> putText (T.replicate i " ")  >> f    aa  xx
        f    aa  (SAnnPush a xx) = setAttribute a                        >> f (a:aa) xx
        f    []  (SAnnPop    xx) =                                          f    []  xx
        f (a:aa) (SAnnPop    xx) = case Prelude.filter (resetsAttribute a) aa of
            []    -> resetAttribute a >> f aa xx
            (e:_) -> setAttribute   e >> f aa xx

-}
