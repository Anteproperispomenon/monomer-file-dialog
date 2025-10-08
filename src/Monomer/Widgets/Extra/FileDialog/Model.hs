module Monomer.Widgets.Extra.FileDialog.Model
  (

  ) where

import System.OsPath

import System.Directory.OsPath

import Data.Sequence qualified as Seq

data FileDialogModel = FileDialogModel
  { _currentDir :: OsPath
  , _backButton :: Seq.Seq OsPath
  , _fwdButton  :: Seq.Seq OsPath
  , _dirFiles   :: [OsPath] -- use `takeFileName` to get these.

  } where


