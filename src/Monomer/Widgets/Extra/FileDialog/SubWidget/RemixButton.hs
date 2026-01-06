{-|
Module      : Monomer.Widgets.Extra.FileDialog.SubWidget.RemixButton
Copyright   : (c) 2018 Francisco Vallarino, 2026 David Wilson
License     : BSD-3-Clause (see the LICENSE file)

Simple variant of button that defaults to using the
'Remix' icon set.

In order for this code to work, you'll have to make
sure you have the following line in your app init
code:

@
appFontDef "Remix" "./assets/fonts/remixicon.ttf"
@
-}

module Monomer.Widgets.Extra.FileDialog.SubWidget.RemixButton
  -- * General Buttons
  -- ** Main Buttons
  ( mainRemixButton
  , mainRemixButton_
  , mainRemixButtonD_
  -- ** Normal Buttons
  , remixButton
  , remixButton_
  , remixButtonD_
  -- * Specific Buttons
  , leftButton
  , rightButton
  , fileUpButton
  , refreshButton
  , refreshButton2
  , closeButton
  , saveButton1
  , saveButton2
  , saveButton3
  , saveButton4
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

import Monomer.Graphics.RemixIcon

import Monomer.Widgets.Singles.Button


{-|
Creates a button with main styling. Useful to highlight an option, such as
\"Accept\", when multiple buttons are available.
-}
mainRemixButton
  :: WidgetEvent e
  => Text            -- ^ The Remix Icon.
  -> e               -- ^ The event to raise on click.
  -> WidgetNode s e  -- ^ The created button.
mainRemixButton icon handler = mainButton icon handler `styleBasic` [textFont "Remix", textMiddle, textCenter]

{-|
Creates a button with main styling. Useful to highlight an option, such as
\"Accept\", when multiple buttons are available. Accepts config.
-}
mainRemixButton_
  :: WidgetEvent e
  => Text             -- ^ The Remix Icon.
  -> e                -- ^ The event to raise on click.
  -> [ButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e   -- ^ The created button.
mainRemixButton_ caption handler configs = mainButton_ caption handler configs
  `styleBasic` [textFont "Remix", textMiddle, textCenter]

{-|
Creates a button with main styling. Useful to highlight an option, such as
\"Accept\", when multiple buttons are available. Accepts config but does not
require an event. See 'buttonD_'.
-}
mainRemixButtonD_
  :: WidgetEvent e
  => Text             -- ^ The Remix Icon.
  -> [ButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e   -- ^ The created button.
mainRemixButtonD_ caption configs = mainButtonD_ caption configs
  `styleBasic` [textFont "Remix", textMiddle, textCenter]

-- | Creates a Remix Icon button with otherwise normal styling.
remixButton
  :: WidgetEvent e
  => Text            -- ^ The Remix Icon.
  -> e               -- ^ The event to raise on click.
  -> WidgetNode s e  -- ^ The created button.
remixButton caption handler = (button caption handler)
  `styleBasic` [textFont "Remix", textMiddle, textCenter]

-- | Creates a Remix Icon button with otherwise normal styling. Accepts config.
remixButton_
  :: WidgetEvent e
  => Text             -- ^ The Remix ICon.
  -> e                -- ^ The event to raise on click.
  -> [ButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e   -- ^ The created button.
remixButton_ caption handler configs = button_ caption handler configs
  `styleBasic` [textFont "Remix", textMiddle, textCenter]

{-|
Creates a button without forcing an event to be provided. The other constructors
use this version, adding an 'onClick' handler in configs.

Using this constructor directly can be helpful in cases where the event to be
raised belongs in a "Monomer.Widgets.Composite" above in the widget tree,
outside the scope of the Composite that contains the button. This parent
Composite can be reached by sending a message ('SendMessage') to its 'WidgetId'
using 'onClickReq'.
-}
remixButtonD_
  :: WidgetEvent e
  => Text             -- ^ The Remix Icon.
  -> [ButtonCfg s e]  -- ^ The config options.
  -> WidgetNode s e   -- ^ The created button.
remixButtonD_ caption configs = buttonD_ caption configs
  `styleBasic` [textFont "Remix", textMiddle, textCenter]

leftButton :: WidgetEvent e => e -> WidgetNode s e
leftButton evt = remixButton remixArrowLeftFill evt

rightButton :: WidgetEvent e => e -> WidgetNode s e
rightButton evt = remixButton remixArrowRightFill evt

fileUpButton :: WidgetEvent e => e -> WidgetNode s e
fileUpButton evt = remixButton (toGlyph 0xF30C) evt
-- fileUpButton evt = remixButton remixCornerLeftUpFill evt

-- &#xF30C;

refreshButton :: WidgetEvent e => e -> WidgetNode s e
refreshButton evt = remixButton remixRefreshLine evt

refreshButton2 :: WidgetEvent e => e -> WidgetNode s e
refreshButton2 evt = remixButton remixRefreshFill evt

saveButton1 :: WidgetEvent e => e -> WidgetNode s e
saveButton1 evt = remixButton remixSave3Fill evt

saveButton2 :: WidgetEvent e => e -> WidgetNode s e
saveButton2 evt = remixButton remixSave2Fill evt

saveButton3 :: WidgetEvent e => e -> WidgetNode s e
saveButton3 evt = remixButton remixSave3Line evt

saveButton4 :: WidgetEvent e => e -> WidgetNode s e
saveButton4 evt = remixButton remixSave2Line evt

closeButton :: WidgetEvent e => e -> WidgetNode s e
closeButton = remixButton remixCloseFill
