{-# LANGUAGE NamedFieldPuns #-}

module Monomer.Widgets.Extra.FileDialog.Column
  ( sizeColumn
  , extnColumn
  , nameColumn
  , dateColumn
  , scrollToTop
  ) where

import Data.Proxy

import Monomer.Hagrid

import Monomer.Widgets.Extra.FileDialog.Model

import Monomer.Widgets.Extra.FileDialog.Internal

import Monomer.Widgets.Composite (EventResponse)

import Data.Text qualified as T

import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..))

import Monomer.Widgets.Containers.Box

import Monomer.Widgets.Singles.Label

import Monomer.Core.Combinators hiding (minWidth)
import Monomer.Core.WidgetTypes

import Data.Typeable

-- Needs to be changed so that it sorts on
-- 
sizeColumn :: Column FileDialogEvent FileData
-- sizeColumn = textColumn "Size" getFileSizeT 
sizeColumn = Column
  { name = "Size"
  , widget = LabelWidget (const getFileSizeT)
  , footerWidget = NoFooterWidget
  , align = ColumnAlignRight
  , initialWidth = 120
  , sortKey = SortWith fdSize -- since it's already a Maybe type
  , minWidth = 80
  , paddingW = 10
  , paddingH = 10
  , resizeHandler = Nothing
  , sortHandler = Nothing
  }

extnColumn :: Column FileDialogEvent FileData
-- extnColumn = textColumn "File Type" getExtn
extnColumn = Column
  { name = "File Type"
  , widget = LabelWidget (const getExtn)
  , footerWidget = NoFooterWidget
  , align = ColumnAlignLeft
  , initialWidth = 120
  , sortKey = SortWith (getOrd fdExtn) -- since it's already a Maybe type
  , minWidth = 80
  , paddingW = 10
  , paddingH = 10
  , resizeHandler = Nothing
  , sortHandler = Nothing
  }


nameColumn :: Column FileDialogEvent FileData
-- nameColumn = showOrdColumn "File Name" (fdName)
nameColumn = Column
  { name = "File Name"
  -- , widget = LabelWidget (const (showFilePath . fdName))
  , widget = CustomWidget clickLabel
  , footerWidget = NoFooterWidget
  , align = ColumnAlignLeft
  , initialWidth = 320
  , sortKey = SortWith (getOrd fdName)
  , minWidth = 120
  , paddingW = 10
  , paddingH = 10
  , resizeHandler = Nothing
  , sortHandler = Nothing
  }

dateColumn :: Column FileDialogEvent FileData
dateColumn = Column
  { name = "Date Modified"
  , widget = LabelWidget (const getFileTime)
  , footerWidget = NoFooterWidget
  , align = ColumnAlignLeft
  , initialWidth = 200
  , sortKey = SortWith (getOrd fdTime)
  , minWidth = 160
  , paddingW = 10
  , paddingH = 10
  , resizeHandler = Nothing
  , sortHandler = Nothing
  }


newtype OrdPath a = OrdPath { getOrdTuple :: (PathKind, a)}
  deriving newtype (Ord, Show, Eq)

getOrd :: (FileData -> a) -> FileData -> OrdPath a
getOrd f fd = OrdPath (fdKind fd, f fd)

{-
textColumn ::
  -- | Name of the column, to display in the header.
  Text ->
  -- | Called with the item for each row to get the text to display for that row.
  (a -> Text) ->
  Column e a
textColumn name get = (defaultColumn name widget) {sortKey}
  where
    widget = LabelWidget (const get)
    sortKey = SortWith get

defaultColumn :: Text -> ColumnWidget e a -> Column e a
defaultColumn name widget =
  Column
    { name,
      widget,
      footerWidget = NoFooterWidget,
      align = ColumnAlignLeft,
      initialWidth = defaultColumnInitialWidth,
      sortKey = DontSort,
      minWidth = defaultColumnMinWidth,
      paddingW = defaultColumnPadding,
      paddingH = defaultColumnPadding,
      resizeHandler = Nothing,
      sortHandler = Nothing
    }
-}

clickLabel :: forall s. WidgetModel s => Int -> FileData -> WidgetNode s FileDialogEvent
clickLabel _n fd = case (fdKind fd) of
  PathDir  -> box_ [onClick (ChangeDir (fdPath fd))] (label (showFilePath $ fdName fd))
  PathFile -> box_ [onClick (FocusFile (fdPath fd))] (label (showFilePath $ fdName fd))

scrollToTop :: forall s e sp ep a. (Typeable a, Typeable e) => Proxy a -> WidgetKey -> EventResponse s e sp ep
scrollToTop _ wkey = scrollToRow wkey callback
  where
    callback :: Seq.Seq (ItemWithIndex a) -> Maybe Int
    callback Empty     = Nothing
    callback (_ :<| _) = Just 0

{- 
-- defaultColumn is not exported.
textOrdColumn 
  :: (Ord b)
  -- | Name of the column
  => T.Text
  -- | How to convert the value to an `Ord`erable value.
  -> (a -> b)
  -- | How to convert the value to a `T.Text` value.
  -> (a -> T.Text)
  -> Column e a
textOrdColumn name getOrd getText = (defaultColumn name widget) {sortKey}
  where
    widget = LabelWidget (const getText)
    sortKey = SortWith getOrd

-}