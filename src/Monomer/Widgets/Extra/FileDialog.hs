
module Monomer.Widgets.Extra.FileDialog
  ( fileDialog
  , FileDialogModel
  , defFileModel
  , setOpen
  , setSave
  ) where

import Monomer.Graphics.ColorTable

import Data.Proxy

import Control.Lens

import Data.Sequence qualified as Seq

import Monomer.Hagrid

import Monomer.Widgets.Containers.Keystroke
-- import Monomer.Widgets.Containers.SelectList 
import Monomer.Widgets.Containers.Scroll
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

fileDialog :: (CompositeEvent ep, CompParentModel sp) => (OsPath -> ep) -> ep -> ALens' sp FileDialogModel -> WidgetNode sp ep
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
  Refresh     -> [Task (SetFiles <$> getDirData' (model ^. currentDir)), scrollToTop (Proxy :: Proxy FileData) "FileDialogGrid"]
  (SetFiles fils) -> [Model (model & dirFiles .~ (Seq.fromList fils))]
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
    Nothing    -> [Event ClosePopups, Event (ErrEvent ("No focus file listed."))]
    (Just fil) -> [ Event ClosePopups, Report (mkEvent fil)]
  ClosePopups -> [Model (model & confVis .~ False & errVis .~ False)]
  NullEvent -> []
  (ErrEvent err) -> [Model (model & fileError .~ err & errVis .~ True)]
  _ -> []

-- type UIBuilder s e = WidgetEnv s e -> s -> WidgetNode s e

buildUI :: WidgetEnv FileDialogModel FileDialogEvent -> FileDialogModel -> WidgetNode FileDialogModel FileDialogEvent
buildUI wenv model = keystroke_ 
  [ ("Esc", CancelDialog)
  , ("Alt-Left", DirBack)
  , ("Alt-Right", DirForward)
  , ("Alt-Up", DirUp)
  , ("Enter" , NullEvent) -- Fix this
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
         ]
      , label ("Error: " <> (model ^. fileError))
      , popup errVis  (box errWidget `styleBasic` [border 3 black, bgColor darkGray])
      , popup confVis (box ovrWidget `styleBasic` [border 3 black, bgColor darkGray])
      , (hagrid_ [initialSort 0 SortAscending] [nameColumn, extnColumn, sizeColumn, dateColumn] (model ^. dirFiles))
          `nodeKey` "FileDialogGrid"
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
      , button "Okay" ClosePopups
      ]
    ovrWidget = vstack_ [childSpacing_ 8]
      [ label "Overwrite File?" `styleBasic` [textSize 24, textLeft]
      , spacer
      , case (model ^. focusFile) of
          Nothing    -> label "The file already exists." -- ???
          (Just fil) -> label_ ("The file \"" <> (showFilePath fil) <> "\" already exists. Overwrite it?") [multiline]
      , hstack_ [childSpacing_ 30]
        [ button "Cancel" ClosePopups
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

