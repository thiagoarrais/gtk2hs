-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget RadioMenuItem
--
--  Author : Axel Simon
--          
--  Created: 21 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2004/08/03 02:58:26 $
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
-- * These are not the original Gtk functions as they involve handling a Gtk
--   owned GList. The interface is rather oriented towards the RadioButton
--   widget interface.
--
-- TODO

module RadioMenuItem(
  RadioMenuItem,
  RadioMenuItemClass,
  castToRadioMenuItem,
  radioMenuItemNew,
  radioMenuItemNewWithLabel,
  radioMenuItemNewWithMnemonic,
  radioMenuItemNewJoinGroup,
  radioMenuItemNewJoinGroupWithLabel,
  radioMenuItemNewJoinGroupWithMnemonic,

  -- * Compatibilty aliases
  radioMenuItemNewFromWidget,
  radioMenuItemNewWithLabelFromWidget,
  radioMenuItemNewWithMnemonicFromWidget
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new radio menu item.
--
radioMenuItemNew :: IO RadioMenuItem
radioMenuItemNew  = makeNewObject mkRadioMenuItem $ liftM castPtr $
  {#call unsafe radio_menu_item_new#} nullPtr

-- | Create a new radio menu item with a label in it.
--
radioMenuItemNewWithLabel :: String -> IO RadioMenuItem
radioMenuItemNewWithLabel label = withUTFString label $ \strPtr ->
  makeNewObject mkRadioMenuItem $ liftM castPtr $
  {#call unsafe radio_menu_item_new_with_label#} nullPtr strPtr

-- | Create a new radio menu item with a label in it. Underscores in the label
-- string indicate the mnemonic for the menu item.
--
radioMenuItemNewWithMnemonic :: String -> IO RadioMenuItem
radioMenuItemNewWithMnemonic label = withUTFString label $ \strPtr ->
  makeNewObject mkRadioMenuItem $ liftM castPtr $
  {#call unsafe radio_menu_item_new_with_mnemonic#} nullPtr strPtr

-- | Create a new radio button and attach it to the group of another radio
-- button.
--
radioMenuItemNewJoinGroup :: RadioMenuItem -> IO RadioMenuItem
radioMenuItemNewJoinGroup rmi = do
  groupPtr <- {#call unsafe radio_menu_item_get_group#} rmi
  makeNewObject mkRadioMenuItem $ liftM castPtr $
    {#call unsafe radio_menu_item_new#} groupPtr

-- | Create a new radio button with a label and attach it to the group of
-- another radio button.
--
radioMenuItemNewJoinGroupWithLabel :: RadioMenuItem -> String ->
                                      IO RadioMenuItem
radioMenuItemNewJoinGroupWithLabel rmi label = do
  groupPtr <- {#call unsafe radio_menu_item_get_group#} rmi
  withUTFString label $ \strPtr -> 
    makeNewObject mkRadioMenuItem $ liftM castPtr $ 
    {#call unsafe radio_menu_item_new_with_label#} groupPtr strPtr

-- | Create a new radio button with a label and attach it to the group of
-- another radio button. Underscores in the label string indicate the mnemonic
-- for the menu item.
--
radioMenuItemNewJoinGroupWithMnemonic :: RadioMenuItem -> String ->
                                      IO RadioMenuItem
radioMenuItemNewJoinGroupWithMnemonic rmi label = do
  groupPtr <- {#call unsafe radio_menu_item_get_group#} rmi
  withUTFString label $ \strPtr -> 
    makeNewObject mkRadioMenuItem $ liftM castPtr $ 
    {#call unsafe radio_menu_item_new_with_mnemonic#} groupPtr strPtr

-- These were added in gtk 2.4, the above Join methods simulate them in earlier
-- versions. These aliases are here for compatibility.

-- | Alias for 'radioMenuItemNewJoinGroup'.
radioMenuItemNewFromWidget = radioMenuItemNewJoinGroup

-- | Alias for 'radioMenuItemNewJoinGroupWithLabel'.
radioMenuItemNewWithLabelFromWidget = radioMenuItemNewJoinGroupWithLabel

-- | Alias for 'radioMenuItemNewJoinGroupWithMnemonic'.
radioMenuItemNewWithMnemonicFromWidget = radioMenuItemNewJoinGroupWithMnemonic
