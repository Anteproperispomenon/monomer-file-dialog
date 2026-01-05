{-|
Module      : Monomer.Widgets.Extra.FileDialog.SubWidget.ImageButton
Copyright   : (c) 2018 Francisco Vallarino, 2026 David Wilson
License     : BSD-3-Clause (see the LICENSE file)

Modified version of "Monomer.Widgets.Singles.Button" that
takes drawing an image instead of text for the button's
label

@
button "Increase count" AppIncrease
@
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Monomer.Widgets.Extra.FileDialog.SubWidget.ImageButton (
  -- * Configuration
  ImgButtonCfg,
  -- * Constructors
  mainImgButton,
  mainImgButton_,
  mainImgButtonD_,
  imgButton,
  imgButton_,
  imgButtonD_
) where

import Control.Applicative ((<|>))
import Control.Lens ((&), (^.), (.~))
import Data.Default
import Data.Maybe
import Data.Text (Text)

import qualified Data.Sequence as Seq

import Monomer.Widgets.Container
import Monomer.Widgets.Singles.Label
import Monomer.Widgets.Extra.FileDialog.Column

import qualified Monomer.Lens as L

import Monomer.Widgets.Extra.FileDialog.SubWidget.ExtIcon


data ImgButtonType
  = ImgButtonNormal
  | ImgButtonMain
  deriving (Eq, Show)

{-|
Configuration options for button:

- 'ignoreParentEvts': whether to ignore all other responses to the click or
  keypress that triggered the button, and only keep this button's response.
  Useful when the button is child of a _keystroke_ widget.
- 'trimSpaces': whether to remove leading/trailing spaces in the caption.
- 'ellipsis': if ellipsis should be used for overflown text.
- 'multiline': if text may be split in multiple lines.
- 'maxLines': maximum number of text lines to show.
- 'ignoreTheme': whether to load default style from theme or start empty.
- 'resizeFactor': flexibility to have more or less space assigned.
- 'resizeFactorW': flexibility to have more or less horizontal space assigned.
- 'resizeFactorH': flexibility to have more or less vertical space assigned.
- 'onFocus': event to raise when focus is received.
- 'onFocusReq': 'WidgetRequest' to generate when focus is received.
- 'onBlur': event to raise when focus is lost.
- 'onBlurReq': 'WidgetRequest' to generate when focus is lost.
- 'onClick': event to raise when button is clicked.
- 'onClickReq': 'WidgetRequest' to generate when button is clicked.
-}


data ImgButtonCfg s e = ImgButtonCfg {
  _imgbtnButtonType   :: Maybe ImgButtonType,
  _imgbtnIgnoreParent :: Maybe Bool,
  _imgbtnIgnoreTheme  :: Maybe Bool,
  -- _imgbtnLabelCfg  :: LabelCfg s e,
  _imgbtnIconCfg      :: ExtIconCfg,
  _imgbtnOnFocusReq   :: [Path -> WidgetRequest s e],
  _imgbtnOnBlurReq    :: [Path -> WidgetRequest s e],
  _imgbtnOnClickReq   :: [WidgetRequest s e]
}

instance Default (ImgButtonCfg s e) where
  def = ImgButtonCfg {
    _imgbtnButtonType = Nothing,
    _imgbtnIgnoreParent = Nothing,
    _imgbtnIgnoreTheme = Nothing,
    -- _imgbtnLabelCfg = def,
    _imgbtnIconCfg    = def,
    _imgbtnOnFocusReq = [],
    _imgbtnOnBlurReq = [],
    _imgbtnOnClickReq = []
  }

instance Semigroup (ImgButtonCfg s e) where
  (<>) t1 t2 = ImgButtonCfg {
      _imgbtnButtonType   = _imgbtnButtonType t2      <|> _imgbtnButtonType t1
    , _imgbtnIgnoreParent = _imgbtnIgnoreParent t2 <|> _imgbtnIgnoreParent t1
    , _imgbtnIgnoreTheme  = _imgbtnIgnoreTheme t2  <|> _imgbtnIgnoreTheme t1
    -- . _imgbtnLabelCfg     = _imgbtnLabelCfg t1      <> _imgbtnLabelCfg t2
    , _imgbtnIconCfg      = _imgbtnIconCfg t1       <> _imgbtnIconCfg t2
    , _imgbtnOnFocusReq   = _imgbtnOnFocusReq t1    <> _imgbtnOnFocusReq t2
    , _imgbtnOnBlurReq    = _imgbtnOnBlurReq t1     <> _imgbtnOnBlurReq t2
    , _imgbtnOnClickReq   = _imgbtnOnClickReq t1    <> _imgbtnOnClickReq t2
  }

instance Monoid (ImgButtonCfg s e) where
  mempty = def

instance CmbIgnoreParentEvts (ImgButtonCfg s e) where
  ignoreParentEvts_ ignore = def {
    _imgbtnIgnoreParent = Just ignore
  }

instance CmbIgnoreTheme (ImgButtonCfg s e) where
  ignoreTheme_ ignore = def {
    _imgbtnIgnoreTheme = Just ignore
  }

-- instance CmbTrimSpaces (ImgButtonCfg s e) where
--   trimSpaces_ trim = def {
--     _imgbtnLabelCfg = trimSpaces_ trim
--   }

-- instance CmbEllipsis (ImgButtonCfg s e) where
--   ellipsis_ ellipsis = def {
--     _imgbtnLabelCfg = ellipsis_ ellipsis
--   }

-- instance CmbMultiline (ImgButtonCfg s e) where
--   multiline_ multi = def {
--     _imgbtnLabelCfg = multiline_ multi
--   }

-- instance CmbMaxLines (ImgButtonCfg s e) where
--   maxLines count = def {
--     _btnLabelCfg = maxLines count
--   }

-- instance CmbResizeFactor (ImgButtonCfg s e) where
--   resizeFactor s = def {
--     _imgbtnLabelCfg = resizeFactor s
--   }

-- instance CmbResizeFactorDim (ImgButtonCfg s e) where
--   resizeFactorW w = def {
--     _btnLabelCfg = resizeFactorW w
--   }
--   resizeFactorH h = def {
--     _btnLabelCfg = resizeFactorH h
--   }

instance WidgetEvent e => CmbOnFocus (ImgButtonCfg s e) e Path where
  onFocus fn = def {
    _imgbtnOnFocusReq = [RaiseEvent . fn]
  }

instance CmbOnFocusReq (ImgButtonCfg s e) s e Path where
  onFocusReq req = def {
    _imgbtnOnFocusReq = [req]
  }

instance WidgetEvent e => CmbOnBlur (ImgButtonCfg s e) e Path where
  onBlur fn = def {
    _imgbtnOnBlurReq = [RaiseEvent . fn]
  }

instance CmbOnBlurReq (ImgButtonCfg s e) s e Path where
  onBlurReq req = def {
    _imgbtnOnBlurReq = [req]
  }

instance WidgetEvent e => CmbOnClick (ImgButtonCfg s e) e where
  onClick handler = def {
    _imgbtnOnClickReq = [RaiseEvent handler]
  }

instance CmbOnClickReq (ImgButtonCfg s e) s e where
  onClickReq req = def {
    _imgbtnOnClickReq = [req]
  }

mainConfig :: ImgButtonCfg s e
mainConfig = def {
  _imgbtnButtonType = Just ImgButtonMain
}

{-|
Creates a button with main styling. Useful to highlight an option, such as
\"Accept\", when multiple buttons are available.
-}
mainImgButton
  :: WidgetEvent e
  => IconBlueprint  
  -- ^ The caption.
  -> e               -- ^ The event to raise on click.
  -> WidgetNode s e  -- ^ The created button.
mainImgButton iconData handler = imgButton_ iconData handler [mainConfig]

{-|
Creates a button with main styling. Useful to highlight an option, such as
\"Accept\", when multiple buttons are available. Accepts config.
-}
mainImgButton_
  :: WidgetEvent e
  => IconBlueprint             -- ^ The caption.
  -> e                -- ^ The event to raise on click.
  -> [ImgButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e   -- ^ The created button.
mainImgButton_ iconData handler configs = imgButton_ iconData handler newConfigs where
  newConfigs = mainConfig : configs

{-|
Creates a button with main styling. Useful to highlight an option, such as
\"Accept\", when multiple buttons are available. Accepts config but does not
require an event. See 'buttonD_'.
-}
mainImgButtonD_
  :: WidgetEvent e
  => IconBlueprint      -- ^ The blueprint.
  -> [ImgButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e      -- ^ The created button.
mainImgButtonD_ blueprint configs = imgButtonD_ blueprint newConfigs where
  newConfigs = mainConfig : configs

-- | Creates a button with normal styling.
imgButton
  :: WidgetEvent e
  => IconBlueprint  -- ^ The blueprint.
  -> e               -- ^ The event to raise on click.
  -> WidgetNode s e  -- ^ The created button.
imgButton blueprint handler = imgButton_ blueprint handler def

-- | Creates a button with normal styling. Accepts config.
imgButton_
  :: WidgetEvent e
  => IconBlueprint             -- ^ The blueprint.
  -> e                -- ^ The event to raise on click.
  -> [ImgButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e   -- ^ The created button.
imgButton_ blueprint handler configs = buttonNode where
  buttonNode = imgButtonD_ blueprint (onClick handler : configs)

{-|
Creates a button without forcing an event to be provided. The other constructors
use this version, adding an 'onClick' handler in configs.

Using this constructor directly can be helpful in cases where the event to be
raised belongs in a "Monomer.Widgets.Composite" above in the widget tree,
outside the scope of the Composite that contains the button. This parent
Composite can be reached by sending a message ('SendMessage') to its 'WidgetId'
using 'onClickReq'.
-}
imgButtonD_
  :: WidgetEvent e
  => IconBlueprint             -- ^ The blueprint.
  -> [ImgButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e   -- ^ The created button.
imgButtonD_ blueprint configs = buttonNode where
  config = mconcat configs
  widget = makeButton blueprint config
  !buttonNode = defaultWidgetNode "button" widget
    & L.info . L.focusable .~ True

makeButton :: WidgetEvent e => IconBlueprint -> ImgButtonCfg s e -> Widget s e
makeButton !caption !config = widget where
  widget = createContainer () def {
    containerAddStyleReq = False,
    containerDrawDecorations = False,
    containerUseScissor = True,
    containerGetBaseStyle = getBaseStyle,
    containerInit = init,
    containerMerge = merge,
    containerHandleEvent = handleEvent,
    containerResize = resize
  }

  !buttonType = fromMaybe ImgButtonNormal (_imgbtnButtonType config)

  getBaseStyle wenv node
    | ignoreTheme = Nothing
    | otherwise = case buttonType of
        ImgButtonNormal -> Just (collectTheme wenv L.btnStyle)
        ImgButtonMain -> Just (collectTheme wenv L.btnMainStyle)
    where
      ignoreTheme = _imgbtnIgnoreTheme config == Just True

  createChildNode wenv node = newNode where
    nodeStyle = node ^. L.info . L.style
    iconCfg = _imgbtnIconCfg config
    -- labelCurrStyle = labelCurrentStyle childOfFocusedStyle
    !iconNode = extIcon_ caption [iconCfg]
      & L.info . L.style .~ nodeStyle
    -- !labelNode = label_ caption [ignoreTheme, labelCfg, labelCurrStyle]
    --   & L.info . L.style .~ nodeStyle
    !newNode = node
      & L.children .~ Seq.singleton iconNode

  init wenv node = result where
    result = resultNode (createChildNode wenv node)

  merge wenv node oldNode oldState = result where
    result = resultNode (createChildNode wenv node)

  handleEvent wenv node target evt = case evt of
    Focus prev -> handleFocusChange node prev (_imgbtnOnFocusReq config)
    Blur next -> handleFocusChange node next (_imgbtnOnBlurReq config)

    KeyAction mode code status
      | isSelectKey code && status == KeyPressed -> Just result
      where
        isSelectKey code = isKeyReturn code || isKeySpace code

    Click p _ _
      | isPointInNodeVp node p -> Just result

    ButtonAction p btn BtnPressed 1 -- Set focus on click
      | mainBtn btn && pointInVp p && not focused -> Just resultFocus

    _ -> Nothing
    where
      mainBtn btn = btn == wenv ^. L.mainButton

      focused = isNodeFocused wenv node
      pointInVp p = isPointInNodeVp node p
      ignoreParent = _imgbtnIgnoreParent config == Just True

      reqs = _imgbtnOnClickReq config ++ [IgnoreParentEvents | ignoreParent]
      result = resultReqs node reqs
      resultFocus = resultReqs node [SetFocus (node ^. L.info . L.widgetId)]

  resize wenv node viewport children = resized where
    assignedAreas = Seq.fromList [viewport]
    resized = (resultNode node, assignedAreas)

