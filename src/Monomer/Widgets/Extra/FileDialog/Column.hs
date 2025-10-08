{-# LANGUAGE NamedFieldPuns #-}

module Monomer.Widgets.Extra.FileDialog.Column
  ( sizeColumn
  , extnColumn
  , nameColumn

  ) where

import Monomer.Hagrid

import Monomer.Widgets.Extra.FileDialog.Model

import Monomer.Widgets.Extra.FileDialog.Internal

import Data.Text qualified as T

-- Needs to be changed so that it sorts on
-- 
sizeColumn :: Column FileDialogEvent FileData
sizeColumn = textColumn "Size" getFileSizeT 

extnColumn :: Column FileDialogEvent FileData
extnColumn = textColumn "File Type" getExtn

nameColumn :: Column FileDialogEvent FileData
nameColumn = showOrdColumn "File Name" (fdName)



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