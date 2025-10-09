{-|
Module      : Monomer.Widgets.Extra.FileDialog
Copyright   : (c) 2025 David Wilson
License     : BSD-3-Clause (see the LICENSE file)

A widget for a file dialog, meant to be used when
opening or saving a file. Note that it does not
actually save or open a file, it merely returns
a valid `System.OsPath.OsPath` that can then be
opened or saved.

-}

module Monomer.Widgets.Extra.FileDialog
  -- * Widgets
  ( fileDialog
  -- * Model
  , FileDialogModel
  , defFileModel
  , defFileModelOpen
  , defFileModelSave
  -- ** Operations
  , setOpen
  , setSave
  ) where

import Monomer.Graphics.ColorTable
import Monomer.Graphics.Util

import Data.Proxy

import Control.Lens

import Data.Sequence qualified as Seq

import Data.Typeable

import Monomer.Hagrid

import Monomer.Widgets.Containers.Keystroke
-- import Monomer.Widgets.Containers.SelectList 
import Monomer.Widgets.Containers.Scroll
import Monomer.Widgets.Containers.ZStack
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Containers.Box
import Monomer.Widgets.Composite

import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Singles.Spacer
import Monomer.Widgets.Singles.TextField

import Monomer.Widgets.Extra.FileDialog.Internal
import Monomer.Widgets.Extra.FileDialog.Model

import System.OsPath

import System.Directory.OsPath

import Monomer.Core.Combinators

import Data.Text qualified as T

import Data.Time.Clock
import Data.Time.Format

import Monomer.Widgets.Extra.FileDialog.Column

import Monomer.Core.StyleUtil

import Monomer.Widgets.Containers.Popup

-- | The main widget creator for a file dialog. Note that
--   you can use the same model for multiple different file
--   dialogs, so long as only one dialog is active at a time.
--   It's probably best to embed this either in a `popup` or
--   a `zstack`.
fileDialog 
  :: (CompositeEvent ep, CompParentModel sp) 
  -- | The parent event to be called with the working `OsPath`.
  => (OsPath -> ep) 
  -- | The parent event to call to close the FileDialog without opening/saving anything.
  -> ep 
  -- | The lens to `FileDialogModel` in the parent model.
  -> ALens' sp FileDialogModel 
  -- | The full widget.
  -> WidgetNode sp ep
fileDialog mkEvt cancelEvt modelLens
  = composite_
      "FileDialog"
      modelLens
      buildUI
      (handleEvent mkEvt cancelEvt)
      [onInit SetupDialog]



-- type EventHandler s e sp ep = WidgetEnv s e -> WidgetNode s e -> s -> e -> [EventResponse s e sp ep]
handleEvent 
  :: (CompositeEvent ep, CompParentModel sp)
  => (OsPath -> ep)
  -> ep
  -> WidgetEnv  FileDialogModel FileDialogEvent 
  -> WidgetNode FileDialogModel FileDialogEvent
  -> FileDialogModel
  -> FileDialogEvent
  -> [EventResponse FileDialogModel FileDialogEvent sp ep]
handleEvent mkEvent cancelEvt wenv wnode model evt = case evt of
  DirBack     -> let (newModel, doEvent) = goBack model in [Model newModel, Event doEvent]
  DirForward  -> let (newModel, doEvent) = goFwd  model in [Model newModel, Event doEvent]
  DirUp       -> let (newModel, doEvent) = goUp   model in [Model newModel, Event doEvent]
  SetupDialog -> [Task (SetDir <$> getCurrentDirectory)]
  Refresh     -> let newCount = (model ^. dirCount) + 1 in
    [Task (SetFiles newCount <$> getDirData' (model ^. currentDir))
    , scrollToTop (Proxy :: Proxy FileData) "FileDialogGrid"
    , Model (model & isLoading .~ True & dirCount .~ newCount)
    ]
  -- Don't set the files if the task is old.
  (SetFiles setCount fils) -> 
    if (setCount == (model ^. dirCount))
      then [Model (model & dirFiles .~ (Seq.fromList fils) & isLoading .~ False)]
      else []
  (SetDir drt)    -> [Model (model & currentDir .~ drt), Event Refresh]
  (ChangeDir drt) -> let (newModel, doEvent) = goDir drt model in [Model newModel, Event doEvent]
  (ChangeDirSafe drt) -> [Task $ goDirSafe drt]
  (FocusFile osPath)  -> [(Model (model & manualPath .~ (showFilePath osPath)))]
  CancelDialog        -> [Report cancelEvt]
  Jump -> case (model ^. manualPath) of
    ""  -> []
    txt -> [Task $ goDirSafeT (model ^. currentDir) txt]
  CheckFile -> case (model ^. manualPath) of
    ""  -> [Event (ErrEvent "Empty manual path")] -- maybe check focusFile
    txt -> case (model ^. dialogType) of
      Open -> [Task $ openFileT (model ^. currentDir) txt]
      Save -> [Task $ saveFileT (model ^. currentDir) txt]
  (DoneFile pth) -> [Report (mkEvent pth)]
  (OverwriteFile fp) -> 
    [Model 
      (model
        & focusFile .~ (Just fp)
        & confVis   .~ True
      )
    ]
  ConfirmOverwrite -> case (model ^. focusFile) of
    Nothing    -> [ Event ClosePopups, Event (ErrEvent ("No focus file listed."))]
    (Just fil) -> [ Event ClosePopups, Report (mkEvent fil)]
  ClosePopups -> [Model (model & confVis .~ False & errVis .~ False)]
  NullEvent -> []
  (ErrEvent err) -> [Model (model & fileError .~ err & errVis .~ True)]
  _ -> []

-- type UIBuilder s e = WidgetEnv s e -> s -> WidgetNode s e

buildUI :: WidgetEnv FileDialogModel FileDialogEvent -> FileDialogModel -> WidgetNode FileDialogModel FileDialogEvent
buildUI wenv model = {-makeLoader (model ^. isLoading) $-} keystroke_ 
  [ ("Esc", CancelDialog)
  , ("Alt-Left", DirBack)
  , ("Alt-Right", DirForward)
  , ("Alt-Up", DirUp)
  , ("Enter" , CheckFile) -- Fix this
  ]
  [ignoreChildrenEvts]
  $ vstack_ [childSpacing_ 3]
      [ hstack_ [childSpacing_ 3]
         [ button "<-" DirBack
         , button "->" DirForward
         , button "Up" DirUp
         , button "Ref" Refresh
         -- , textField_ currentDir [readOnly]
         , label (showFilePath (model ^. currentDir))
         , filler
         , button "X" CancelDialog
         ]
      -- , label ("Error: " <> (model ^. fileError))
      -- , label (if (model ^. isLoading) then "Loading..." else "Loaded")
      , popup errVis  (box errWidget `styleBasic` [border 3 black, radius 5, bgColor darkGray, padding 10])
      , popup confVis (box ovrWidget `styleBasic` [border 3 black, radius 5, bgColor darkGray, padding 10])
      , makeLoader (model ^. isLoading) ((hagrid_ [initialSort 0 SortAscending] [nameColumn, extnColumn, sizeColumn, dateColumn] (model ^. dirFiles))
          `nodeKey` "FileDialogGrid")
      , hstack_ [childSpacing_ 3]
        [ textField manualPath
        , button "Jump" Jump
        , button (T.pack $ show (_dialogType model)) CheckFile
    
        ]
  ]
  where
    errWidget = vstack_ [childSpacing_ 8]
      [ label "Error" `styleBasic` [textSize 24, textLeft]
      , spacer
      , label_ (model ^. fileError) [multiline]
      , hstack [filler, button "Okay" ClosePopups]
      ]
    ovrWidget = vstack_ [childSpacing_ 8]
      [ label "Overwrite File?" `styleBasic` [textSize 24, textLeft]
      , spacer
      , case (model ^. focusFile) of
          Nothing    -> label "The file already exists." -- ???
          (Just fil) -> label_ ("The file \"" <> (showFilePath fil) <> "\" already exists.\nOverwrite it?") [multiline]
      , spacer
      , hstack_ [childSpacing_ 30]
        [ filler 
        , button "Cancel" ClosePopups
        , mainButton "Save" ConfirmOverwrite
        ]

      ]

-- | Check that a directory exists before
--   changing to it.
goDirSafe :: OsPath -> IO FileDialogEvent
goDirSafe osPath = do
  bl <- doesDirectoryExist osPath
  -- str <- T.pack <$> decodeFS osPath
  if bl
    then (return (ChangeDir osPath))
    else do 
      str <- T.pack <$> decodeFS osPath
      (return (ErrEvent ("Directory does not exist: " <> str)))

goDirSafeT :: OsPath -> T.Text -> IO FileDialogEvent
goDirSafeT pwd txt = do
  newDir <- encodeFS (T.unpack txt)
  if isAbsolute newDir
    then do
      bl <- doesDirectoryExist newDir
      if bl
        then return (ChangeDir newDir)
        else do 
          bl2 <- doesPathExist newDir
          if bl2
            then return (ErrEvent $ txt <> " is a file, not a directory.")
            else return (ErrEvent $ txt <> " does not exist.")
    else do
      let theDir = pwd </> newDir
      bl <- doesDirectoryExist theDir
      if bl
        then return (ChangeDir theDir)
        else do
          bl2 <- doesPathExist theDir
          if bl2
            then return (ErrEvent $ txt <> " is a file, not a directory.")
            else return (ErrEvent $ txt <> " does not exist.")
  
openFileT :: OsPath -> T.Text -> IO FileDialogEvent
openFileT pwd txt = do
  newFile <- encodeFS (T.unpack txt)
  if isAbsolute newFile
    then do 
      bl <- doesFileExist newFile
      if bl
        then return (DoneFile newFile)
        else do
          bl2 <- doesDirectoryExist newFile
          if bl2
            then return (ChangeDir newFile) -- since it's a dir that exists.
            else return (ErrEvent $ txt <> " does not exist.")
    else do
      let theFile = pwd </> theFile
      bl <- doesFileExist theFile
      if bl
        then return (DoneFile theFile)
        else do
          bl2 <- doesDirectoryExist theFile
          if bl2
            then return (ChangeDir theFile) -- since it's a dir that exists.
            else return (ErrEvent $ txt <> " does not exist.")

saveFileT :: OsPath -> T.Text -> IO FileDialogEvent
saveFileT pwd txt = do
  newFile <- encodeFS (T.unpack txt)
  if isAbsolute newFile
    then do 
      bl <- doesFileExist newFile
      if bl
        then return (OverwriteFile newFile)
        else do
          bl2 <- doesDirectoryExist newFile
          if bl2
            then return (ChangeDir newFile)
            else do
              let fileDir = dropFileName newFile
              bl3 <- doesDirectoryExist fileDir
              if bl3
                then return (DoneFile newFile)
                else return (ErrEvent ("Directory " <> (showFilePath fileDir) <> " does not exist."))
    else do
      let theFile = pwd </> theFile
      bl <- doesFileExist theFile
      if bl
        then return (OverwriteFile theFile)
        else do
          bl2 <- doesDirectoryExist theFile
          if bl2
            then return (ChangeDir theFile)
            else do
              let fileDir = dropFileName theFile
              bl3 <- doesDirectoryExist fileDir
              if bl3
                then return (DoneFile theFile)
                else return (ErrEvent ("Directory " <> (showFilePath fileDir) <> " does not exist."))

{-
  { fdName :: OsPath
  , fdPath :: OsPath -- Full, absolutePath.
  , fdExtn :: Maybe OsString
  , fdKind :: PathKind
  , fdSize :: Maybe Integer
  , fdTime :: UTCTime
-}

makeLoader :: (Typeable s, WidgetEvent e) => Bool -> WidgetNode s e -> WidgetNode s e
makeLoader False wnode = zstack [wnode]
makeLoader True  wnode = zstack [wnode, newWidget]
  where
    newWidget 
      = box 
         (label "Loading..." `styleBasic` [textSize 20, textCenter])
         `styleBasic` [bgColor (rgba 70 70 70 0.4)]

