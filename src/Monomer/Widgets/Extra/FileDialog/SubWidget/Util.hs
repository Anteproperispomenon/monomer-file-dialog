{-|
Module      : Monomer.Widgets.Extra.FileDialog.SubWidget.Util
Copyright   : (c) 2025 David Wilson
License     : BSD-3-Clause (see the LICENSE file)

A companion module to "Monomer.Widgets.Util.Drawing"
that provides a type that can be used to make the
functions from that module into concrete data types.

__NOTE__ : Many of the functions here (/especially/ the
Bezier functions) have yet to be tested for correctness.

-}


module Monomer.Widgets.Extra.FileDialog.SubWidget.Util
  ( DrawStep(..)
  , runDrawStep
  , runDrawSteps
  , PathRun(..)
  , PathPart(..)
  , CubicBezierSteps(..)
  , CubicBezierSpline(..)
  , cubicBezSteps
  , cubicBezSpline
  , eqRadius
  ) where

import Control.Applicative ((<|>))
import Control.Lens ((^.), (^?!), non)
import Control.Monad (forM_, when)
import Data.Default
import Data.Maybe

import Monomer.Core
import Monomer.Graphics.Types
import Monomer.Graphics.Util (rgb)

import qualified Monomer.Common.Lens as L
import qualified Monomer.Core.Lens as L

import Monomer.Widgets.Util.Drawing

-- | A way to describe a point within a given viewport.
-- data RelPt
--   = CornerPt RelPtCorner
--   -- ^ One of the four corner points.
--   | BetweenCorners  RelPtCorner Double RelPtCorner
--   -- ^ Exactly (x * 100%) of the way between two corner points.
--   | WithinViewPort Double Double
--   -- ^ A relative point within a viewport; @(0,0) = 
--   deriving (Show, Eq)

-- data RelPtCorner
--   = TL
--   | TR
--   | BL
--   | BR
--   deriving (Show, Eq, Ord)

-- bindRelPt :: Rect -> RelPt -> Point

data DrawStep_
  = DrawInScissor_ Bool Rect [DrawStep_]
  | DrawInTranslation_ Point [DrawStep_]
  | DrawInScale_ Point       [DrawStep_]
  | DrawInRotation_ Double   [DrawStep_]
  | DrawInAlpha_    Double   [DrawStep_]
  | DrawTextLine_ StyleState TextLine
  | DrawLine_ Point Point Double (Maybe Color)
  | DrawRect_ (Maybe Color) (Maybe Radius)
  | DrawRectBorder_ Rect Border (Maybe Radius)
  | DrawRectBoxGradient_ Rect Double Double Color Color
  | DrawTriangle_ Point Point Point (Maybe Color)
  | DrawTriangleBorder_ Point Point Point Double (Maybe Color)
  | DrawArc_       Rect Double Double Winding (Maybe Color)
  | DrawArcBorder_ Rect Double Double Winding (Maybe Color) Double
  | DrawEllipse_       Rect (Maybe Color)
  | DrawEllipseBorder_ Rect (Maybe Color) Double
  | DrawArrowUp_   Rect (Maybe Color)
  | DrawArrowDown_ Rect (Maybe Color)
  -- | DrawStyledAction       Rect StyleState (Rect -> [DrawStep])
  -- | DrawStyledAction_ Bool Rect StyleState (Rect -> [DrawStep])
  | DrawRoundedRect_       Rect Radius
  | DrawRoundedRectBorder_ Rect Border Radius
  deriving (Show, Eq)

{-
runDrawStep_ :: Renderer -> StyleState -> DrawStep_ -> IO ()
runDrawStep_ rend sst stp = case stp of
  DrawInScissor_ bl rct stps   -> drawInScissor rend bl rct  (runDrawSteps_ rend sst stps)
  DrawInTranslation_ pt stps   -> drawInTranslation rend pt  (runDrawSteps_ rend sst stps)
  DrawInScale_       pt stps   -> drawInScale       rend pt  (runDrawSteps_ rend sst stps)
  DrawInRotation_ dbl stps     -> drawInRotation    rend dbl (runDrawSteps_ rend sst stps)
  DrawInAlpha_    alp stps     -> drawInAlpha       rend alp (runDrawSteps_ rend sst stps)
  DrawTextLine_   st' txt      -> drawTextLine rend st' txt
  DrawLine_ pt1 pt2 wdth col   -> drawLine rend pt1 pt2 wdth col
  DrawRect_ mcol mrad          -> drawRect rend mcol mrad
  DrawRectBorder_ rct brd mrad -> drawRectBorder rend rct brd mrad
  DrawRectBoxGradient_ rct rad fth col1 col2 -> drawRectBoxGradient rend rct rad fth col1 col2
  DrawTriangle_ pt1 pt2 pt3 mcol -> drawTriangle rend pt1 pt2 pt3 mcol
  DrawTriangleBorder_ pt1 pt2 pt3 wid mcol -> drawTriangle rend pt1 pt2 pt3 wid mcol
  DrawArc_       rct ang1 ang2 wdir mcol     -> drawArc       rend rct ang1 ang2 wdir mcol
  DrawArcBorder_ rct ang1 ang2 wdir mcol wdt -> drawArcBorder rend rct ang1 ang2 wdir mcol
  DrawEllipse_       rct mcol      -> drawEllipse       rend rct mcol
  DrawEllipseBorder_ rct mcol wdth -> drawEllipseBorder rend rct mcol
  DrawArrowUp_   rct mcol -> drawArrowUp   rend rct mcol
  DrawArrowDown_ rct mcol -> drawArrowDown rend rct mcol
  DrawRoundedRect_       rect     rad -> drawRoundedRect       rend rect     rad
  DrawRoundedRectBorder_ rect bdr rad -> drawRoundedRectBorder rend rect bdr rad

runDrawSteps_ :: Renderer -> StyleState -> [DrawStep_] -> IO ()
runDrawSteps_ rend sst rct = foldr (\x rst -> runDrawStep_ rend sst x >> rst) (return ())
-}

-- | A variant of `DrawStep_` meant for
--   relative drawing. As such, all points
--   and rectangles are interpreted as 
--   relative points in the range (0,1).
--   Widths are supplied by the calling
--   function.
data DrawStep
  = DrawInScissor Bool Rect [DrawStep]
  | DrawInTranslation Point [DrawStep]
  | DrawInScale Point       [DrawStep]
  | DrawInRotation Double   [DrawStep]
  | DrawInAlpha    Double   [DrawStep]
  | DrawTextLine {- StyleState -} TextLine
  | DrawLine Point Point {-Double-} (Maybe Color)
  | DrawRect Rect (Maybe Color) (Maybe Radius)
  | DrawRectBorder Rect Border (Maybe Radius)
  | DrawRectBoxGradient Rect Double Double Color Color
  | DrawTriangle Point Point Point (Maybe Color)
  | DrawTriangleBorder Point Point Point {-Double-} (Maybe Color)
  | DrawArc       Rect Double Double Winding (Maybe Color)
  | DrawArcBorder Rect Double Double Winding (Maybe Color) {-Double-}
  | DrawEllipse       Rect (Maybe Color)
  | DrawEllipseBorder Rect (Maybe Color) {-Double-}
  | DrawArrowUp   Rect (Maybe Color)
  | DrawArrowDown Rect (Maybe Color)
  -- | DrawStyledAction       Rect StyleState (Rect -> [DrawStep])
  -- | DrawStyledAction_ Bool Rect StyleState (Rect -> [DrawStep])
  | DrawRoundedRect       Rect Radius
  | DrawRoundedRectBorder Rect Border Radius
  | DrawPathRun PathRun
  deriving (Show, Eq)

runDrawStep :: Renderer -> StyleState -> Rect -> Double -> DrawStep -> IO ()
runDrawStep rend sst vp lwdth stp = case stp of
  DrawInScissor bl rct stps   -> drawInScissor rend bl (rszRect rct)  (runDrawSteps rend sst vp lwdth stps)
  DrawInTranslation pt stps   -> drawInTranslation rend (rszPt pt) (runDrawSteps rend sst vp lwdth stps)
  DrawInScale       pt stps   -> drawInScale       rend (rszPt pt) (runDrawSteps rend sst vp lwdth stps)
  DrawInRotation dbl stps     -> drawInRotation    rend dbl (runDrawSteps rend sst vp lwdth stps)
  DrawInAlpha    alp stps     -> drawInAlpha       rend alp (runDrawSteps rend sst vp lwdth stps)
  DrawTextLine   txt          -> drawTextLine rend sst txt
  DrawLine pt1 pt2 {-wdth-} mcol -> drawLine rend (rszPt pt1) (rszPt pt2) lwdth (rpCol mcol)
  DrawRect rct mcol mrad          -> drawRect rend (rszRect rct) (rpCol mcol) mrad
  DrawRectBorder rct brd mrad -> drawRectBorder rend (rszRect rct) brd mrad
  DrawRectBoxGradient rct rad fth col1 col2 -> drawRectBoxGradient rend (rszRect rct) rad fth col1 col2
  DrawTriangle pt1 pt2 pt3 mcol -> drawTriangle rend (rszPt pt1) (rszPt pt2) (rszPt pt3) (rpCol mcol)
  DrawTriangleBorder pt1 pt2 pt3 {-wid-} mcol -> drawTriangleBorder rend (rszPt pt1) (rszPt pt2) (rszPt pt3) lwdth (rpCol mcol)
  DrawArc       rct ang1 ang2 wdir mcol     -> drawArc       rend (rszRect rct) ang1 ang2 wdir (rpCol mcol)
  DrawArcBorder rct ang1 ang2 wdir mcol {-wdt-} -> drawArcBorder rend (rszRect rct) ang1 ang2 wdir (rpCol mcol) lwdth
  DrawEllipse       rct mcol      -> drawEllipse       rend (rszRect rct) (rpCol mcol)
  DrawEllipseBorder rct mcol {-wdth-} -> drawEllipseBorder rend (rszRect rct) (rpCol mcol) lwdth
  DrawArrowUp   rct mcol -> drawArrowUp   rend (rszRect rct) (rpCol mcol)
  DrawArrowDown rct mcol -> drawArrowDown rend (rszRect rct) (rpCol mcol)
  DrawRoundedRect       rect     rad -> drawRoundedRect       rend (rszRect rect)     rad
  DrawRoundedRectBorder rect bdr rad -> drawRoundedRectBorder rend (rszRect rect) bdr rad
  DrawPathRun prun -> renderPathRun rend (resizePathRun prun vp lwdth)
  where
    vpX, vpY, vpW, vpH :: Double
    vpX = _rX vp
    vpY = _rY vp
    vpW = _rW vp
    vpH = _rH vp
    rszRect :: Rect -> Rect
    rszRect (Rect x y w h) = 
      Rect 
        (vpX + (vpW * x)) 
        (vpY + (vpH * y)) 
        (w * vpW)
        (h * vpH)
    rszPt :: Point -> Point
    rszPt (Point x y) = Point (vpX + (vpW * x)) (vpY + (vpH * y))
    rpCol :: Maybe Color -> Maybe Color
    rpCol (Just x) = Just x
    rpCol Nothing  = case (_sstFgColor sst) of
      Nothing  -> Just (rgb 0 0 0)
      mcol     -> mcol
    -- rszRad :: Maybe Radius -> Maybe Radius
    -- rszRad Nothing = Nothing
    -- rszRad ()

runDrawSteps :: Renderer -> StyleState -> Rect -> Double -> [DrawStep] -> IO ()
runDrawSteps rend sst vp lw = foldr (\x rst -> runDrawStep rend sst vp lw x >> rst) (return ())

draw2Bezier1 :: Renderer -> Point -> Point -> Point -> Double -> Maybe Color -> IO ()
draw2Bezier1 _ _ _ _ _ Nothing = pure ()
draw2Bezier1 rndr pt1 c1 pt2 wdth (Just col) = do
  beginPath rndr
  setStrokeColor rndr col
  setStrokeWidth rndr wdth
  moveTo rndr pt1
  renderQuadTo rndr c1 pt2 
  stroke rndr


draw3Bezier1 :: Renderer -> Point -> Point -> Point -> Point -> Double -> Maybe Color -> IO ()
draw3Bezier1 _ _ _ _ _ _ Nothing = pure ()
draw3Bezier1 rndr pt1 c1 c2 pt2 wdth (Just col) = do
  beginPath rndr
  setStrokeColor rndr col
  setStrokeWidth rndr wdth
  moveTo rndr pt1
  renderBezierTo rndr c1 c2 pt2 
  stroke rndr

-- | Render a `PathRun` as it says in the definition.
renderPathRun :: Renderer -> PathRun -> IO ()
renderPathRun _ (PathRun _ _ _ Nothing) = return ()
renderPathRun _ (PathRun _ [] _ _) = return ()
renderPathRun rndr (PathRun strPt stps wdth (Just col)) = do
  beginPath rndr
  setStrokeColor rndr col
  setStrokeWidth rndr wdth
  moveTo rndr strPt
  mapM_ renderSection stps
  stroke rndr
  where
    renderSection :: PathPart -> IO ()
    renderSection (PathLineTo pt) = renderLineTo rndr pt
    renderSection (PathQuadTo ct pt) = renderQuadTo rndr ct pt
    renderSection (PathBezTo ct1 ct2 pt) = renderBezierTo rndr ct1 ct2 pt

-- | Render a `PathRun` with styled content.
{- renderPathRun :: Renderer -> StyleState -> Rect -> Double -> PathRun -> IO ()
renderPathRun _ (PathRun _ _ _ Nothing) = return ()
renderPathRun _ (PathRun _ [] _ _) = return ()
renderPathRun rndr (PathRun strPt stps wdth (Just col)) = do
  beginPath rndr
  setStrokeColor rndr col
  setStrokeWidth rndr wdth
  moveTo rndr strPt
  mapM_ renderSection pts
  stroke rndr
  where
    renderSection :: PathPart -> IO ()
    renderSection (PathLineTo pt) = renderLineTo rndr pt
    renderSection (PathQuadTo ct pt) = renderQuadTo rndr ct pt
    renderSection (PathBexTo ct1 ct2 pt) = renderBezierTo ct1 ct2 pt
-}

-- | A simple description of a path that
--   can be easily sent to the renderer.
data PathRun = PathRun
  { prStart :: Point
  , prSteps :: [PathPart]
  , prWidth :: Double
  , prColor :: Maybe Color
  } deriving (Show, Eq)

data PathPart
  = PathLineTo Point
  | PathQuadTo Point Point 
  | PathBezTo  Point Point Point
  deriving (Show, Eq)

data CubicBezierSteps = CubicBezierSteps
  { splineFirstControl :: Point
  , splinePath :: [(Point, Point)]
  } deriving (Show, Eq)

data CubicBezierSpline = CubicBezierSpline
  { splineStart :: Point
  , splineSteps :: CubicBezierSteps
  } deriving (Show, Eq)

cubicBezSpline :: CubicBezierSpline -> Maybe Color -> PathRun
cubicBezSpline (CubicBezierSpline stPt stps) mcol
  = PathRun
      stPt
      (cubicBezSteps stps)
      1
      mcol

-- | Convert a `CubicBezierSteps` to the corresponding
--   sequence of `PathPart`s.
cubicBezSteps :: CubicBezierSteps -> [PathPart]
cubicBezSteps (CubicBezierSteps _t1 []) = []
cubicBezSteps (CubicBezierSteps ct1 ((ct2,pt2):pts2)) = (PathBezTo ct1 ct2 pt2) : (go ct2 pt2 pts2)
  where
    addPt :: Point -> Point -> Point
    addPt (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
    subPt :: Point -> Point -> Point
    subPt (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)
    go :: Point -> Point -> [(Point,Point)] -> [PathPart]
    go _ _ [] = []
    go oldCt oldPt ((newCt, newPt):rst)
      = (PathBezTo (oldPt `addPt` (oldPt `subPt` oldCt)) newCt newPt) : go newCt newPt rst

resizePathRun :: PathRun -> Rect -> Double -> PathRun
resizePathRun (PathRun pt1 stps wd mcol) vp lineWidth
  = PathRun (rszPt pt1) (go stps) (wd * lineWidth) mcol
  where
    vpX, vpY, vpW, vpH :: Double
    vpX = _rX vp
    vpY = _rY vp
    vpW = _rW vp
    vpH = _rH vp
    rszPt :: Point -> Point
    rszPt (Point x y) = Point (vpX + (vpW * x)) (vpY + (vpH * y))
    go :: [PathPart] -> [PathPart]
    go [] = []
    go ((PathLineTo pt)      :rst) = (PathLineTo (rszPt pt)) : go rst
    go ((PathQuadTo ct pt)   :rst) = (PathQuadTo (rszPt ct) (rszPt pt)) : go rst
    go ((PathBezTo  c1 c2 pt):rst) = (PathBezTo  (rszPt c1) (rszPt c2) (rszPt pt)) : go rst

-- | Use the same radius for each corner.
eqRadius :: Double -> Radius
eqRadius rd = Radius
  (Just (RadiusCorner rd))
  (Just (RadiusCorner rd))
  (Just (RadiusCorner rd))
  (Just (RadiusCorner rd))
