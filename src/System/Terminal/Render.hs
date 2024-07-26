{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Render where

import Control.Monad.State.Strict qualified as State
import Prettyprinter (Doc, SimpleDocStream (..))
import Prettyprinter qualified
import Prettyprinter.Extra
import Prelude hiding (putDoc, putLn, putSimpleDocStream)
import Prelude qualified

deriving stock instance Generic Position

type MonadCursor t m m' =
    ( MonadTrans t
    , MonadTerminal m'
    , m ~ t m'
    , MonadState Position m
    )

layoutPretty :: Doc ann -> SimpleDocStream ann
layoutPretty = Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions

type AttributeStack m = [Attribute m]

attributeStackToDocStream
    :: AttributeStack m
    -> SimpleDocStream (Attribute m)
    -> SimpleDocStream (Attribute m)
attributeStackToDocStream = foldr (\ann acc -> SAnnPush ann . acc) id

render
    :: forall m
     . (MonadTerminal m)
    => Maybe (Position, Doc (Attribute m))
    -> (Position, Doc (Attribute m))
    -> m ()
render (fromMaybe (Position{row = 0, col = 0}, "") -> (oldPos, oldDoc)) (newPos, newDoc) =
    flip evalStateT oldPos do
        goLine 0 0 (layoutPretty oldDoc, mempty) (layoutPretty newDoc, mempty)
        moveToPosition newPos
  where
    goLine
        :: (MonadCursor t m' m)
        => Int
        -> Int
        -> (SimpleDocStream (Attribute m), AttributeStack m)
        -> (SimpleDocStream (Attribute m), AttributeStack m)
        -> m' ()
    goLine _ _ (nullS -> True, _) (nullS -> True, _) = pure ()
    goLine _ _ (nullS -> True, _) (newStream, _) = putSimpleDocStream newStream
    goLine line col _ (nullS -> True, _) = do
        moveToPosition Position{row = line - 1, col}
        lift $ eraseInDisplay EraseForward
    goLine line _ (oldStream, oldStack) (newStream, newStack) = do
        let (oldLine, oldNewLine, oldRest) = takeLineS oldStream
            (newLine, newNewLine, newRest) = takeLineS newStream
            (commonPrefix, oldSuffix, newSuffix) =
                if oldStack == newStack
                    then findCommonPrefixS oldLine newLine
                    else (SEmpty, oldLine, newLine)
            commonPrefixLen = lineLenS commonPrefix
            oldSuffixLen = lineLenS oldSuffix
            newSuffixLen = lineLenS newSuffix
            oldStackAfterPrefix = applyAnnotations commonPrefix oldStack
            newStackAfterPrefix = applyAnnotations commonPrefix newStack
        if nullS oldSuffix && nullS newSuffix
            then do
                when (newNewLine /= SEmpty && oldNewLine == SEmpty) do
                    moveToPosition Position{row = line, col = commonPrefixLen}
                    putLn
                goLine
                    (line + 1)
                    commonPrefixLen
                    (oldRest, oldStackAfterPrefix)
                    (newRest, newStackAfterPrefix)
            else do
                moveToPosition Position{row = line, col = commonPrefixLen}
                unless (nullS newSuffix)
                    . putSimpleDocStream
                    . attributeStackToDocStream newStackAfterPrefix
                    $ newSuffix
                when (oldSuffixLen > newSuffixLen) . lift $ eraseInLine EraseForward
                putSimpleDocStream newNewLine
                if nullS newRest
                    then lift $ eraseInDisplay EraseForward
                    else do
                        let oldStackAfterSuffix = applyAnnotations oldSuffix oldStackAfterPrefix
                            newStackAfterSuffix = applyAnnotations newSuffix newStackAfterPrefix
                        goLine
                            (line + 1)
                            (commonPrefixLen + newSuffixLen)
                            (oldRest, oldStackAfterSuffix)
                            (newRest, newStackAfterSuffix)
    applyAnnotations
        :: SimpleDocStream (Attribute m)
        -> AttributeStack m
        -> AttributeStack m
    applyAnnotations SFail = id
    applyAnnotations SEmpty = id
    applyAnnotations (SAnnPush ann rest) = applyAnnotations rest . (ann :)
    applyAnnotations (SAnnPop rest) = applyAnnotations rest . drop 1
    applyAnnotations stream = applyAnnotations (stream ^. tailS)

moveToRow :: (MonadCursor t m m') => Int -> m ()
moveToRow newRow = do
    oldRow <- State.gets (.row)
    lift $ case compare newRow oldRow of
        GT -> moveCursorDown $ newRow - oldRow
        LT -> moveCursorUp $ oldRow - newRow
        EQ -> pure ()
    State.modify $ #row .~ newRow

moveToColumn :: (MonadCursor t m m') => Int -> m ()
moveToColumn newCol = do
    oldCol <- State.gets (.col)
    when (oldCol /= newCol) do
        lift $ setCursorColumn newCol
        State.modify $ #col .~ newCol

moveToPosition :: (MonadCursor t m m') => Position -> m ()
moveToPosition Position{..} = moveToRow row >> moveToColumn col

putSimpleDocStream
    :: (MonadCursor t m m')
    => SimpleDocStream (Attribute m')
    -> m ()
putSimpleDocStream s = do
    lift $ Prelude.putSimpleDocStream s
    State.modify $
        if numLines > 0
            then #row %~ (+ numLines) >>> #col .~ len
            else #col %~ (+ len)
  where
    numLines = countLinesS s
    len = lastLineLenS s

putDoc
    :: forall t m m'
     . (MonadCursor t m m')
    => Doc (Attribute m')
    -> m ()
putDoc = putSimpleDocStream . layoutPretty

putLn :: forall t m m'. (MonadCursor t m m') => m ()
putLn = do
    lift Prelude.putLn
    State.modify $ #row %~ (+ 1) >>> #col .~ 0
