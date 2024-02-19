{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}

module System.Terminal.Widgets.SearchSelect where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Internal.Prelude
import System.Terminal.Render qualified as Render
import System.Terminal.Widgets.Common
import System.Terminal.Widgets.Select
import System.Terminal.Widgets.TextInput

-- | SearchSelect is a combination of 'TextInput' and 'Select'; it allows the
-- selections to be searched and sorted via the user's text input, similar to
-- [vertico for Emacs](https://github.com/minad/vertico).
--
-- Note: FIXME: A 'Map' is used internally. Therefore, the order of the search
-- result is determined by the output of 'Data.Map.toList'. For as long as 'Select'
-- continues to use a list, it may be preferable to use the converted output
-- to initialize 'selectWidget'. This prevents the jarring transition upon
-- first user input.
--
-- Example:
-- >>> textInput = TextInput "M-x " False False mempty id
-- >>> opts = Map.toList $ Map.fromList [("foo", False), ("bar", True), ("baz", False)]
-- >>> select = Select "Options" opts False 0
-- >>> searchSelect = SearchSelect textInput select (Map.fromList opts) True 10
-- >>> withTerminal . runTerminalT $ runWidget searchSelect
-- @
-- M-x <search>
-- Options
--  (*) [bar]
--  ( ) baz
--  ( ) foo
-- @
data SearchSelect = SearchSelect
    { searchWidget :: TextInput
    -- ^ The field the user uses to search the options.
    , selectWidget :: Select
    -- ^ The options presented to a user. Searched through via the searchField.
    -- This widget should /not/ be used to query all available options;
    -- it only shows the filtered output.
    , globalOptions :: Map Text Bool
    -- ^ Used internally to keep track of all options and their statuses, even
    -- when not shown to the user.
    , showOnNoInput :: Bool
    -- ^ Whether to show the options when the user has yet to type any input.
    , showCount :: Int
    -- ^ The maximum number of items to show at any given time.
    }
    deriving stock (Generic)

filterSS :: Text -> SearchSelect -> SearchSelect
filterSS t ss = ss{selectWidget = newSelect}
  where
    filteredEntries =
        take ss.showCount
            . Map.toList
            $ Map.filterWithKey (const . T.isInfixOf t) ss.globalOptions
    filterLen = length filteredEntries
    newCursorRow
        | ss.selectWidget.cursorRow > filterLen =
            -- 'max' prevents hookups, since 'Select' is 1-indexed
            max filterLen 1
        | otherwise = ss.selectWidget.cursorRow
    newSelect = ss.selectWidget{options = filteredEntries, cursorRow = newCursorRow}

updateGlobal :: Select -> Map Text Bool -> Map Text Bool
updateGlobal sel
    | sel.multiselect = Map.union (Map.fromList sel.options)
    | otherwise = Map.union updates . Map.map (const False)
  where
    updates = Map.fromList $ filter snd sel.options

highlightSelected :: Select -> Select
highlightSelected s = s & #options . ix (s.cursorRow - 1) %~ first (`T.intercalate` ["[", "]"])

instance Widget SearchSelect where
    cursor :: Lens' SearchSelect Position
    cursor = #searchWidget . cursor

    -- For some reason, this widget emits a press of the spacebar
    -- as `CharKey ' '` instead of SpaceKey.
    handleEvent (KeyEvent (CharKey ' ') []) ss@(SearchSelect{selectWidget, globalOptions})
        | null selectWidget.options = ss
        | otherwise =
            let newSelect = handleEvent (KeyEvent SpaceKey []) selectWidget
                newGlobal = updateGlobal newSelect globalOptions
             in ss{selectWidget = newSelect, globalOptions = newGlobal}
    handleEvent ev@(KeyEvent (ArrowKey _) []) ss =
        ss & #selectWidget %~ handleEvent ev
    handleEvent ev ss =
        let newSearch = handleEvent ev ss.searchWidget
            search = valueText newSearch
         in filterSS search ss & #searchWidget %~ handleEvent ev
    valid = valid . (.selectWidget)
    submitEvent ss
        | valid ss.selectWidget =
            Just $ KeyEvent EnterKey []
        | otherwise = Nothing
    toText ss =
        toText ss.searchWidget
            <> "\n"
            <> toText (highlightSelected ss.selectWidget)
    render (maybeOld, new) = do
        Render.render (r <$> maybeOld) (r new)
        let promptOffset = T.length new.searchWidget.prompt
            queryOffset = T.length (valueText new.searchWidget)
        setCursorColumn $ promptOffset + queryOffset
      where
        r :: SearchSelect -> (Position, Text)
        r ss = (ss ^. cursor, toText ss)
