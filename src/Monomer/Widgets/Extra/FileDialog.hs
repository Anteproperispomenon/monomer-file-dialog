
module Monomer.Widgets.Extra.FileDialog
  ( fileDialog

  ) where

import Control.Lens

import Monomer.Widgets.Containers.SelectList
import Monomer.Widgets.Composite

import Monomer.Widgets.Extra.FileDialog.Internal
import Monomer.Widgets.Extra.FileDialog.Model

import System.OsPath

import System.Directory.OsPath

import Monomer.Core.Combinators

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
  DirBack    -> let (newModel, doEvent) = goBack model in [Model newModel, Event doEvent]
  DirForward -> let (newModel, doEvent) = goFwd  model in [Model newModel, Event doEvent]
  SetupDialog -> [Task (SetDir <$> getCurrentDirectory)]
  NullEvent -> []


buildUI :: UIBuilder FileDialogModel FileDialogEvent
buildUI = undefined
