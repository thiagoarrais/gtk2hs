-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget ToggleButton
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2004/08/03 02:58:25 $
--
--  Copyright (c) 1999..2002 Axel Simon
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
-- |
--
-- A ToggleButton is the base class for all buttons that have an inherit
-- state.
--

module ToggleButton(
  ToggleButton,
  ToggleButtonClass,
  castToToggleButton,
  toggleButtonNew,
  toggleButtonNewWithLabel,
  toggleButtonNewWithMnemonic,
  toggleButtonSetMode,
  toggleButtonGetMode,
  toggleButtonToggled,
  toggleButtonGetActive,
  toggleButtonSetActive,
  toggleButtonGetInconsistent,
  toggleButtonSetInconsistent,
  onToggled,
  afterToggled
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new ToggleButton widget.
--
toggleButtonNew :: IO ToggleButton
toggleButtonNew  = makeNewObject mkToggleButton $ liftM castPtr 
  {#call unsafe toggle_button_new#}


-- | Create a ToggleButton with a label in it.
--
toggleButtonNewWithLabel :: String -> IO ToggleButton
toggleButtonNewWithLabel lbl = withUTFString lbl (\strPtr ->
  makeNewObject mkToggleButton $ liftM castPtr $
  {#call unsafe toggle_button_new_with_label#} strPtr)

-- | Create a ToggleButton with a label in it. Underscores in label indicate the
-- mnemonic for the button.
--
toggleButtonNewWithMnemonic :: String -> IO ToggleButton
toggleButtonNewWithMnemonic lbl = withUTFString lbl (\strPtr ->
  makeNewObject mkToggleButton $ liftM castPtr $
  {#call unsafe toggle_button_new_with_mnemonic#} strPtr)

-- | Sets whether the button is displayed as a separate indicator and label.
-- You can call this function on a "CheckButton" or a "RadioButton" with @False@
-- to make the button look like a normal button.
--
toggleButtonSetMode :: ToggleButtonClass tb => tb -> Bool -> IO ()
toggleButtonSetMode tb mode =
  {#call toggle_button_set_mode#} (toToggleButton tb) (fromBool mode)

-- | Retrieves whether the button is displayed as a separate indicator and
-- label.
--
toggleButtonGetMode :: ToggleButtonClass tb => tb -> IO Bool
toggleButtonGetMode tb =
  liftM toBool $ {#call unsafe toggle_button_get_mode#} (toToggleButton tb)

-- | Emit the 'toggled' signal on the button.
--
toggleButtonToggled :: ToggleButtonClass tb => tb -> IO ()
toggleButtonToggled tb = {#call toggle_button_toggled#} (toToggleButton tb)

-- | Retrieve the current state of the button. Returns True if the button is
-- depressed.
--
toggleButtonGetActive :: ToggleButtonClass tb => tb -> IO Bool
toggleButtonGetActive tb = liftM toBool $
  {#call unsafe toggle_button_get_active#} (toToggleButton tb)

-- | Sets the state of the ToggleButton. True means the button should be
-- depressed.
--
toggleButtonSetActive :: ToggleButtonClass tb => Bool -> tb -> IO ()
toggleButtonSetActive active tb = 
  {#call toggle_button_set_active#} (toToggleButton tb) (fromBool active)

-- | Retrieve the inconsistent flag of the button. An inconsistent state only
-- visually affects the button. It will be displayed in an \"in-between\" state.
--
toggleButtonGetInconsistent :: ToggleButtonClass tb => tb -> IO Bool
toggleButtonGetInconsistent tb = liftM toBool $
  {#call unsafe toggle_button_get_inconsistent#} (toToggleButton tb)

-- | Sets the inconsistent flag of the ToggleButton.
--
toggleButtonSetInconsistent :: ToggleButtonClass tb => Bool -> tb -> IO ()
toggleButtonSetInconsistent incon tb = 
  {#call toggle_button_set_inconsistent#} (toToggleButton tb) (fromBool incon)

-- signals


-- | Whenever the state of the button is changed, the toggled signal is emitted.
--
onToggled, afterToggled :: ButtonClass b => b -> IO () -> IO (ConnectId b)
onToggled = connect_NONE__NONE "toggled" False
afterToggled = connect_NONE__NONE "toggled" True

