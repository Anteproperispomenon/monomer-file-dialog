
module Monomer.Widgets.Extra.FileDialog
  ( fileDialog
  , FileDialogModel
  , defFileModel
  ) where

import Control.Lens

import Monomer.Widgets.Containers.SelectList
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



fileDialog :: (CompositeEvent ep, CompParentModel sp) => (OsPath -> ep) -> ALens' sp FileDialogModel -> WidgetNode sp ep
fileDialog mkEvt modelLens
  = composite_
      "FileDialog"
      modelLens
      buildUI
      (handleEvent mkEvt)
      [onInit SetupDialog]



-- type EventHandler s e sp ep = WidgetEnv s e -> WidgetNode s e -> s -> e -> [EventResponse s e sp ep]
handleEvent 
  :: (CompositeEvent ep, CompParentModel sp)
  => (OsPath -> ep)
  -> WidgetEnv  FileDialogModel FileDialogEvent 
  -> WidgetNode FileDialogModel FileDialogEvent
  -> FileDialogModel
  -> FileDialogEvent
  -> [EventResponse FileDialogModel FileDialogEvent sp ep]
handleEvent mkEvent wenv wnode model evt = case evt of
  DirBack     -> let (newModel, doEvent) = goBack model in [Model newModel, Event doEvent]
  DirForward  -> let (newModel, doEvent) = goFwd  model in [Model newModel, Event doEvent]
  DirUp       -> let (newModel, doEvent) = goUp   model in [Model newModel, Event doEvent]
  SetupDialog -> [Task (SetDir <$> getCurrentDirectory)]
  Refresh     -> [Task (SetFiles <$> getDirData' (model ^. currentDir))]
  (SetFiles fils) -> [Model (model & dirFiles .~ fils)]
  (SetDir drt)    -> [Model (model & currentDir .~ drt), Event Refresh]
  NullEvent -> []
  _ -> []

-- type UIBuilder s e = WidgetEnv s e -> s -> WidgetNode s e

buildUI :: WidgetEnv FileDialogModel FileDialogEvent -> FileDialogModel -> WidgetNode FileDialogModel FileDialogEvent
buildUI wenv model = vstack_ [childSpacing_ 3]
  [ hstack_ [childSpacing_ 3]
     [ button "<-" DirBack
     , button "->" DirForward
     , button "Up" DirUp
     , button "Ref" Refresh
     -- , textField_ currentDir [readOnly]
     , label (T.pack $ show (model ^. currentDir))
     ]
  , scroll $ vstack_ [childSpacing_ 1] $ (model ^. dirFiles) <&> \fd -> hstack_ [childSpacing_ 8]
      [ label (T.pack $ show (fdName fd))
      , label (getExtn fd)
      , label (getFileSizeT fd)
      , label (getFileTime  fd)
      ]
  -- , label_ (T.pack $ show model) [multiline] -- for debug only
  ]

getExtn :: FileData -> T.Text
getExtn fd = case (fdExtn fd) of
  Nothing -> "Directory"
  (Just ext) -> (T.pack $ show ext) <> " file"

getFileSizeT :: FileData -> T.Text
getFileSizeT fd = case (fdSize fd) of
  Nothing   -> "N/A"
  (Just sz) -> showSize sz

getFileTime :: FileData -> T.Text
getFileTime fd = T.pack $ formatTime theTimeFormat "%Y-%m-%d, %H:%M" (fdTime fd)


theTimeFormat :: TimeLocale
theTimeFormat = defaultTimeLocale
  { dateFmt = "%y-%m-%d"
  , timeFmt = "%H:%M" -- or %H:%M:%S
  , dateTimeFmt = "%Y-%m-%d, %H:%M"
  }

{-
  { fdName :: OsPath
  , fdPath :: OsPath -- Full, absolutePath.
  , fdExtn :: Maybe OsString
  , fdKind :: PathKind
  , fdSize :: Maybe Integer
  , fdTime :: UTCTime
-}

