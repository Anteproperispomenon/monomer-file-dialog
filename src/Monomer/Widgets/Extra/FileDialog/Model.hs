{-# LANGUAGE TemplateHaskell #-}

module Monomer.Widgets.Extra.FileDialog.Model
  ( FileDialogModel(..)
  , currentDir
  , backButton
  , fwdButton
  , dirFiles
  , pathSelect
  , dialogType
  , DialogType(..)
  , FileDialogEvent(..)

  ) where

import Monomer.Widgets.Extra.FileDialog.Internal

import System.OsPath

import System.Directory.OsPath

import Data.Sequence qualified as Seq

import Data.Text qualified as T

import Control.Lens.TH

data DialogType
  = Open
  | Save
  deriving (Show, Eq)

data FileDialogModel = FileDialogModel
  { _currentDir :: OsPath
  , _backButton :: Seq.Seq OsPath
  , _fwdButton  :: Seq.Seq OsPath
  , _dirFiles   :: [FileData]
  , _pathSelect :: T.Text
  , _dialogType :: DialogType
  } deriving (Show, Eq)

makeLenses 'FileDialogModel

data FileDialogEvent
  = DirBack
  | DirForward
  | DirUp
  | DirJump T.Text
  | FileSelect
  deriving (Show, Eq)

