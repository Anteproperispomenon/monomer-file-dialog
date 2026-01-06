{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Monomer
import TextShow

import qualified Monomer.Lens as L

import Monomer.Widgets.Extra.FileDialog

import Monomer.Widgets.Extra.FileDialog.Filters
import Monomer.Widgets.Extra.FileDialog.Filters.Common

import System.OsPath

data AppModel = AppModel 
  { _clickCount :: Int
  , _thatFile   :: Maybe OsPath
  , _fileModel  :: FileDialogModel
  } deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | AppSetPath OsPath
  | AppSetSave
  | AppSetOpen
  | AppNull
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack 
    [ label "Hello world"
    , label (T.pack $ show (model ^. thatFile))
    , spacer
    , hstack 
       [ label $ "Click count: " <> showt (model ^. clickCount)
       , spacer
       , button "Increase count" AppIncrease
       , spacer
       , button "Save" AppSetSave
       , spacer
       , button "Open" AppSetOpen
       ]
    , fileDialog AppSetPath AppNull fileModel
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Model (model & fileModel %~ (turnOnFilter . changeFilters [imageKind]))]
  AppIncrease -> [Model (model & clickCount +~ 1)]
  (AppSetPath pth) -> [Model (model & thatFile .~ (Just pth))]
  AppSetOpen -> [Model (model & fileModel %~ setOpen)]
  AppSetSave -> [Model (model & fileModel %~ setSave)]
  _ -> []

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      -- appFontDef "Regular" "./assets/fonts/remixicon.ttf",
      appFontDef "Remix" "./assets/fonts/remixicon.ttf",
      appInitEvent AppInit
      ]
    model = AppModel 0 Nothing defFileModel
