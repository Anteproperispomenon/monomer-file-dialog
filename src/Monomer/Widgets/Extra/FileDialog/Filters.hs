{-|
Module      : Monomer.Widgets.Extra.FileDialog.Filters
Copyright   : (c) 2025 David Wilson
License     : BSD-3-Clause (see the LICENSE file)

Working with the filtering capability of
a file dialog.

-}

module Monomer.Widgets.Extra.FileDialog.Filters
  ( FileFormat(..)
  , FileKind(..)
  , FilterData(..)
  , ExtTrie
  -- , kindToTrie
  , makeFilterData
  -- , makeFilterData'
  , allFiles
  -- , showExtensions
  ) where

import Monomer.Widgets.Extra.FileDialog.Filters.Internal
import Monomer.Widgets.Extra.FileDialog.OsString.Trie
