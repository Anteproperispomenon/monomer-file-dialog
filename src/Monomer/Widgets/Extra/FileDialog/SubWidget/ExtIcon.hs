{-|
Module      : Monomer.Widgets.Extra.FileDialog.SubWidget.ExtIcon
Copyright   : (c) 2018 Francisco Vallarino, 2026 David Wilson
License     : BSD-3-Clause (see the LICENSE file)

Extended version of "Monomer.Widgets.Singles.Icon" that
allows user-defined drawings of icons.

@
icon IconPlus
@
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monomer.Widgets.Extra.FileDialog.SubWidget.ExtIcon (
  -- * Configuration
    ExtIconCfg
  , IconBlueprint
  -- * Constructors
  , extIcon
  , extIcon_
  -- * Example Blueprints
  , squareIcon
  , roundSquareIcon
) where

import Control.Lens ((^.))
import Control.Applicative ((<|>))
import Data.Default
import Data.Maybe

import qualified Data.Text as T

import Monomer.Graphics.Util

import Monomer.Widgets.Single

import qualified Monomer.Lens as L

import Monomer.Widgets.Extra.FileDialog.SubWidget.Util

-- | A simple type synoynm to describe
--   a vector image to be drawn, since
--   I'm not sure which type to use yet.
type IconBlueprint = [DrawStep]

-- | Different types of icons that can be displayed.
-- data IconType
--   = IconClose
--   | IconPlus
--   | IconMinus
--   deriving (Eq, Show)

{-|
Configuration options for icon:

- 'width': the maximum width and height of the icon.
-}
newtype ExtIconCfg = ExtIconCfg {
  _eicWidth :: Maybe Double
}

instance Default ExtIconCfg where
  def = ExtIconCfg {
    _eicWidth = Nothing
  }

instance Semigroup ExtIconCfg where
  (<>) i1 i2 = ExtIconCfg {
    _eicWidth = _eicWidth i2 <|> _eicWidth i1
  }

instance Monoid ExtIconCfg where
  mempty = def

instance CmbWidth ExtIconCfg where
  width w = def {
    _eicWidth = Just w
  }

-- | Creates an icon of the given type.
extIcon
  :: IconBlueprint  -- ^ The icon type.
  -> T.Text          -- ^ A one-word description of the icon
  -> WidgetNode s e  -- ^ The created icon.
extIcon iconType iconName = extIcon_ iconType iconName def

-- | Creates an icon of the given type. Accepts config.
extIcon_
  :: IconBlueprint  -- ^ The icon type.
  -> T.Text          -- ^ A one-word description of the icon
  -> [ExtIconCfg]       -- ^ The config options.
  -> WidgetNode s e  -- ^ The created icon.
extIcon_ iconType iconName configs = defaultWidgetNode widgetType widget where
  -- iconName = T.pack $ show iconType
  widgetType = WidgetType ("icon" <> iconName)
  config = mconcat configs
  widget = makeImage iconType config

makeImage :: IconBlueprint -> ExtIconCfg -> Widget s e
makeImage iconType config = widget where
  widget = createSingle () def {
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  getSizeReq wenv node = sizeReq where
    (w, h) = (16, 16)
    factor = 1
    sizeReq = (minSize w factor, minSize h factor)

  render wenv node renderer = do
    drawIcon renderer style iconType iconVp width
    where
      style = currentStyle wenv node
      contentArea = getContentArea node style
      vp = node ^. L.info . L.viewport
      dim = min (vp ^. L.w) (vp ^. L.h)
      width = fromMaybe (dim / 2) (_eicWidth config)
      iconVp = centeredSquare contentArea

centeredSquare :: Rect -> Rect
centeredSquare (Rect x y w h) = Rect newX newY dim dim where
  dim = min w h
  newX = x + (w - dim) / 2
  newY = y + (h - dim) / 2

drawIcon :: Renderer -> StyleState -> IconBlueprint -> Rect -> Double -> IO ()
drawIcon renderer style iconType viewport lw = runDrawSteps renderer style viewport lw iconType
{-
  IconClose ->
    drawTimesX renderer viewport lw (Just fgColor)

  IconPlus -> do
    beginPath renderer
    setFillColor renderer fgColor
    renderRect renderer (Rect (cx - hw) y lw h)
    renderRect renderer (Rect x (cy - hw) w lw)
    fill renderer

  IconMinus -> do
    beginPath renderer
    setFillColor renderer fgColor
    renderRect renderer (Rect x (cy - hw) w lw)
    fill renderer
  where
    Rect x y w h = viewport
    fgColor = fromMaybe (rgb 0 0 0) (style ^. L.fgColor)
    hw = lw / 2
    cx = x + w / 2
    cy = y + h / 2
    mx = x + w
    my = y + h
-}

-- ExampleIcons

squareIcon :: IconBlueprint
squareIcon = [DrawRect (Rect 0 0 1 1) Nothing Nothing]

roundSquareIcon :: IconBlueprint
roundSquareIcon = [DrawRect (Rect 0 0 1 1) Nothing (Just (eqRadius 3))]



