{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

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
  , goBack
  , goFwd
  , goUp
  , defFileModel
  ) where

import Monomer.Widgets.Extra.FileDialog.OS

import Data.Function

import Control.Lens.Operators

import Monomer.Widgets.Extra.FileDialog.Internal

import System.OsPath

import System.Directory.OsPath

import Data.Sequence qualified as Seq
import Data.Sequence (Seq(..))

import Data.Text qualified as T

import Control.Lens.TH

import Data.Default

import System.OsString (osstr)

data DialogType
  = Open
  | Save
  deriving (Show, Eq)

data FileDialogModel = FileDialogModel
  { _currentDir :: OsPath
  , _backButton :: Seq.Seq OsPath
  , _fwdButton  :: Seq.Seq OsPath
  , _dirFiles   :: Seq.Seq FileData
  , _pathSelect :: T.Text
  , _dialogType :: DialogType
  , _maxBacklog :: Int
  } deriving (Show, Eq)

makeLenses 'FileDialogModel

defFileModel :: FileDialogModel
defFileModel = def

instance Default FileDialogModel where
  def = FileDialogModel
    { _currentDir = baseDir
    , _backButton = Empty
    , _fwdButton  = Empty
    , _dirFiles   = Empty
    , _pathSelect = ""
    , _dialogType = Open
    , _maxBacklog  = 15
    }

data FileDialogEvent
  = DirBack
  | DirForward
  | DirUp
  | DirJump T.Text
  | SetDir OsPath -- Set the dir, but don't populate the file list.
  | ChangeDir OsPath -- Set the dir and populate the file list (but don't change back/fwd).
  | ChangeToDir OsPath -- Set the dir, populate the file list, modify the back/fwd lists.
  | Refresh -- Populate the dir with the "current" directory.
  | SetFiles [FileData]
  | FileSelect
  | SetupDialog -- set the current dir to the pwd
  | NullEvent   -- Do Nothing
  deriving (Show, Eq)

-- | To be used whenever changing to a new directory.
-- refresh :: FileDialogModel -> IO FileDialogEvent
-- refresh model = SetFiles <$> 

goBack :: FileDialogModel -> (FileDialogModel, FileDialogEvent)
goBack model
  | (Seq.null (_backButton model)) = (model, NullEvent) -- Can't go back
  | (Seq.length (_fwdButton model) >= mx)
  , (bckRst :|> x) <- (_backButton model)
  , (fwdRst :|> _) <- (_fwdButton  model)
  = (model
      & currentDir .~ x
      & backButton .~ bckRst
      & fwdButton  .~ (pwd <| fwdRst)
    , Refresh -- I guess?
    ) 
  | (bckRst :|> x) <- (_backButton model)
  , fwdRst         <- (_fwdButton  model)
  = (model
      & currentDir .~ x
      & backButton .~ bckRst
      & fwdButton  .~ (pwd <| fwdRst)
    , Refresh -- I guess?
    )
  | otherwise = (model, NullEvent)
  where 
    mx  = _maxBacklog model
    pwd = _currentDir model

goFwd :: FileDialogModel -> (FileDialogModel, FileDialogEvent)
goFwd model
  | (Seq.null (_fwdButton model)) = (model, NullEvent) -- Can't go forward
  | (Seq.length (_backButton model) >= mx)
  , (_ :<| bckRst) <- (_backButton model)
  , (x :<| fwdRst) <- (_fwdButton  model)
  = (model
      & currentDir .~ x
      & backButton .~ (bckRst |> pwd)
      & fwdButton  .~ fwdRst
    , Refresh -- I guess?
    ) 
  | bckRst         <- (_backButton model)
  , (x :<| fwdRst) <- (_fwdButton  model)
  = (model
      & currentDir .~ x
      & backButton .~ (bckRst |> pwd)
      & fwdButton  .~ fwdRst
    , Refresh -- I guess?
    )
  | otherwise = (model, NullEvent)
  where 
    mx  = _maxBacklog model
    pwd = _currentDir model

goUp :: FileDialogModel -> (FileDialogModel, FileDialogEvent)
goUp model
  | (oldDir == newDir) = (model, NullEvent)
  | (Seq.length (_backButton model) >= mx)
  , (_ :<| bckRst) <- (_backButton model)
  = ( model
       & currentDir .~ newDir
       & fwdButton  .~ Empty
       & backButton .~ (bckRst |> oldDir)
    , Refresh
    )
  | bckRst <- (_backButton model)
  = ( model
       & currentDir .~ newDir
       & fwdButton  .~ Empty
       & backButton .~ (bckRst |> oldDir)
    , Refresh
    )
  | otherwise = (model, NullEvent)
  where 
    oldDir = (_currentDir model)
    newDir = takeDirectory oldDir
    mx  = _maxBacklog model



