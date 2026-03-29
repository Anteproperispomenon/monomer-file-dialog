{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : Monomer.Widgets.Extra.FileDialog.Filters.Common
Copyright   : (c) 2025 David Wilson
License     : BSD-3-Clause (see the LICENSE file)

This module contains a number of *common* Filters
to be used with various file types. It does not mean
common in the sense of common code that is shared
between modules.

-}

module Monomer.Widgets.Extra.FileDialog.Filters.Common
  ( imageKind

  ) where

import System.OsString.Compat (osstr)

import Monomer.Widgets.Extra.FileDialog.Filters.Internal

import Data.List.NonEmpty (NonEmpty(..))

import Data.Text qualified as T

pngFormat :: FileFormat
pngFormat = FileFormat ([osstr|.png|] :| []) "PNG Image"

jpgFormat :: FileFormat
jpgFormat = FileFormat ([osstr|.jpg|] :| [ [osstr|.jpeg|] ]) "JPEG Image"

bmpFormat :: FileFormat
bmpFormat = FileFormat ([osstr|.bmp|] :| []) "Bitmap Image"

-- | The kind of Images.
imageKind :: FileKind
imageKind = FileKind "Images" (pngFormat :| [jpgFormat, bmpFormat])




