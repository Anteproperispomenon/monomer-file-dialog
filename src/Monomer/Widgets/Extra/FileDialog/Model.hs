{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : Monomer.Widgets.Extra.FileDialog.Model
Copyright   : (c) 2025 David Wilson
License     : BSD-3-Clause (see the LICENSE file)

Internal model and events for the file dialog.

-}

module Monomer.Widgets.Extra.FileDialog.Model
  ( FileDialogModel(..)
  , currentDir
  , backButton
  , fwdButton
  , dirFiles
  -- , pathSelect
  , dialogType
  , manualPath
  , focusFile
  , fileError
  , errVis
  , confVis
  , isLoading
  , dirCount
  , DialogType(..)
  , FileDialogEvent(..)
  , goBack
  , goFwd
  , goUp
  , goDir
  , setOpen
  , setSave
  , defFileModel
  , defFileModelOpen
  , defFileModelSave
  ) where

import Data.Word

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

-- | This is the model you need to add to your
--   app's main model to use the file dialog.
--   It is exported opaque, since you don't need
--   to (and in fact should not) modify anything
--   about the model yourself, aside from whether
--   it's in Open mode or Save mode. To modify that,
--   you can use `setOpen` and `setSave` together with
--   the `(Control.Lens.Operators.%~)` operator.
data FileDialogModel = FileDialogModel
  { _currentDir :: OsPath
  , _backButton :: Seq.Seq OsPath
  , _fwdButton  :: Seq.Seq OsPath
  , _dirFiles   :: Seq.Seq FileData
  -- , _pathSelect :: T.Text
  , _dialogType :: DialogType
  , _maxBacklog :: Int
  , _manualPath :: T.Text -- Entered by user.
  , _focusFile  :: Maybe OsPath
  , _fileError  :: T.Text
  , _errVis     :: Bool
  , _confVis    :: Bool
  , _isLoading  :: Bool
  , _dirCount   :: Word16
  } deriving (Show, Eq)

makeLenses 'FileDialogModel

-- | The default file model to use.
--   It defaults to working in "Open" mode.
defFileModel :: FileDialogModel
defFileModel = def

-- | Synonym for `defFileModel`.
defFileModelOpen :: FileDialogModel
defFileModelOpen = defFileModel

-- | The default file model to use when
--   saving a file.
defFileModelSave :: FileDialogModel
defFileModelSave = defFileModel {_dialogType = Save}

instance Default FileDialogModel where
  def = FileDialogModel
    { _currentDir = baseDir
    , _backButton = Empty
    , _fwdButton  = Empty
    , _dirFiles   = Empty
    -- , _pathSelect = ""
    , _dialogType = Open
    , _maxBacklog = 15
    , _manualPath = ""
    , _focusFile  = Nothing
    , _fileError  = ""
    , _errVis     = False
    , _confVis    = False
    , _isLoading  = False
    , _dirCount   = 0
    }

-- | Change the underlying `FileDialogModel` to
--   work in "Open File" mode. This will reject
--   returning any files that don't exist. You can
--   use this with `(%~)` to change the value,
--   e.g. @[Model (model & fdModel %~ setOpen)]@.
setOpen :: FileDialogModel -> FileDialogModel
setOpen model = model {_dialogType = Open}

-- | Change the underlying `FileDialogModel` to
--   work in "Save File" mode. This will notify
--   the user before returning files that already
--   exist. You can use this with `(%~)` to change the 
--   value, e.g. @[Model (model & fdModel %~ setSave)]@.
setSave :: FileDialogModel -> FileDialogModel
setSave model = model {_dialogType = Save}

-- Serious feature/event creep...
data FileDialogEvent
  = DirBack
  | DirForward
  | DirUp
  | DirJump T.Text
  | SetDir OsPath -- Set the dir without changing fwd/back.
  | ChangeDir OsPath -- Set the dir and change fwd/back.
  | ChangeDirSafe OsPath -- Confirm that the dir IS a dir, then change.
  | Refresh -- Populate the dir with the "current" directory.
  | SetFiles Word16 [FileData]
  -- | FileSelect
  | SetupDialog -- set the current dir to the pwd
  | FocusFile OsPath
  | ErrEvent T.Text
  | Jump
  | CheckFile
  | OverwriteFile OsPath
  | ClosePopups
  | ConfirmOverwrite
  | DoneFile OsPath
  | CancelDialog -- Stop Searching for a file
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


goDir :: OsPath -> FileDialogModel -> (FileDialogModel, FileDialogEvent)
goDir newDir model
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
    oldDir = _currentDir model
    mx     = _maxBacklog model

