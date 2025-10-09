
module Monomer.Widgets.Extra.FileDialog
  ( fileDialog
  , FileDialogModel
  , defFileModel
  ) where

import Data.Proxy

import Control.Lens

import Data.Sequence qualified as Seq

import Monomer.Hagrid

import Monomer.Widgets.Containers.Keystroke
-- import Monomer.Widgets.Containers.SelectList 
import Monomer.Widgets.Containers.Scroll
import Monomer.Widgets.Containers.Stack
import Monomer.Widgets.Containers.Grid
import Monomer.Widgets.Composite

import Monomer.Widgets.Singles.Button
import Monomer.Widgets.Singles.Label
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
      Save -> []
  (DoneFile pth) -> [Report (mkEvent pth)]
  NullEvent -> []
  (ErrEvent err) -> [Model (model & fileError .~ err)]
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
         , label (T.pack $ show (model ^. currentDir))
         ]
      , label ("Error: " <> (model ^. fileError))
      , (hagrid_ [initialSort 0 SortAscending] [nameColumn, extnColumn, sizeColumn, dateColumn] (model ^. dirFiles))
          `nodeKey` "FileDialogGrid"
      , hstack_ [childSpacing_ 3]
        [ textField manualPath
        , button "Jump" Jump
        , button (T.pack $ show (_dialogType model)) CheckFile
    
        ]
  {-  
  , scroll $ vstack_ [childSpacing_ 1] $ (model ^. dirFiles) <&> \fd -> hstack_ [childSpacing_ 8]
      [ label (T.pack $ show (fdName fd))
      , label (getExtn fd)
      , label (getFileSizeT fd)
      , label (getFileTime  fd)
      ]
  -}
  -- , label_ (T.pack $ show model) [multiline] -- for debug only
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

{-
  { fdName :: OsPath
  , fdPath :: OsPath -- Full, absolutePath.
  , fdExtn :: Maybe OsString
  , fdKind :: PathKind
  , fdSize :: Maybe Integer
  , fdTime :: UTCTime
-}

