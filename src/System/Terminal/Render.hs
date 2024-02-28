{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Render where

import Data.List.Extra qualified as List
import Data.Text qualified as Text
import Prettyprinter
import Prelude

deriving stock instance Generic Position

type MonadCursor t m m' =
    ( MonadTrans t
    , MonadTerminal m'
    , m ~ t m'
    , MonadState Position m
    )

render
    :: (MonadTerminal m)
    => Maybe (Position, Doc (Attribute m))
    -> (Position, Doc (Attribute m))
    -> m ()
render maybeOld (newPos, newDoc) = flip evalStateT (maybe Position{row = 0, col = 0} fst maybeOld) do
    moveToPosition Position{row = 0, col = 0}
    lift $ eraseInDisplay EraseForward
    putDoc newDoc
    moveToPosition newPos
  where
    moveToRow :: (MonadCursor t m m') => Int -> m ()
    moveToRow newRow = do
        oldRow <- gets (.row)
        lift $ case compare newRow oldRow of
            GT -> moveCursorDown $ newRow - oldRow
            LT -> moveCursorUp $ oldRow - newRow
            EQ -> pure ()
        modify $ #row .~ newRow

    moveToColumn :: (MonadCursor t m m') => Int -> m ()
    moveToColumn newCol = do
        oldCol <- gets (.col)
        when (oldCol /= newCol) do
            lift (setCursorColumn newCol)
            modify $ #col .~ newCol

    moveToPosition :: (MonadCursor t m m') => Position -> m ()
    moveToPosition Position{..} = moveToRow row >> moveToColumn col

    putDoc :: forall t m m'. (MonadCursor t m m') => Doc (Attribute m') -> m ()
    putDoc d = do
        lift $ Prelude.putDoc d
        let lines = List.split (`is` #_TLine) $ tokenise @m' d
        let lastLineLen = sum . fmap tokenWidth . last $ lines
        modify $
            if length lines > 1
                then #row %~ (+ (length lines - 1)) >>> #col .~ lastLineLen
                else #col %~ (+ lastLineLen)

renderLine :: (MonadScreen m) => Text -> Text -> m ()
renderLine oldText newText =
    unless (oldText == newText) do
        let (Text.length -> prefixLen, Text.length -> oldSuffixLen, newSuffix) =
                commonPrefixes oldText newText
        setCursorColumn prefixLen
        putText newSuffix
        when (oldSuffixLen > Text.length newSuffix) $ eraseInLine EraseForward

data Token m
    = TText Int Text
    | TLine
    | TAnnPush (Attribute m)
    | TAnnPop
    deriving stock (Generic)

tokenWidth :: Token m -> Int
tokenWidth (TText len _) = len
tokenWidth _ = 0

tokenise :: forall m. (MonadTerminal m) => Doc (Attribute m) -> [Token m]
tokenise = go . layoutPretty defaultLayoutOptions
  where
    go :: SimpleDocStream (Attribute m) -> [Token m]
    go SFail = []
    go SEmpty = []
    go (SChar c rest) = TText 1 (Text.singleton c) : go rest
    go (SText len text rest) = TText len text : go rest
    go (SLine indentation rest) = TLine : TText indentation (Text.replicate indentation " ") : go rest
    go (SAnnPush ann rest) = TAnnPush ann : go rest
    go (SAnnPop rest) = TAnnPop : go rest
