-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Widget OptionMenu@
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2002/05/24 09:43:25 $
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
-- @description@ --------------------------------------------------------------
--
--
-- @documentation@ ------------------------------------------------------------
--
--
-- @todo@ ---------------------------------------------------------------------

module OptionMenu(
  OptionMenu,
  OptionMenuClass,
  castToOptionMenu,
  optionMenuNew,
  optionMenuGetMenu,
  optionMenuSetMenu,
  optionMenuRemoveMenu,
  optionMenuSetHistory,
  optionMenuGetHistory,
  onOMChanged,
  afterOMChanged
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @constructor optionMenuNew@ Create a new option menu.
--
optionMenuNew :: IO OptionMenu
optionMenuNew  = makeNewObject mkOptionMenu $ 
  liftM castPtr {#call unsafe option_menu_new#}

-- @method optionMenuGetMenu@ Get the menu that should be associated with this
-- option menu.
--
optionMenuGetMenu :: OptionMenuClass om => om -> IO Menu
optionMenuGetMenu om = makeNewObject mkMenu $ liftM castPtr $
  throwIfNull "optionMenuGetMenu: no menu associated with this option menu." $
  {#call unsafe option_menu_get_menu#} (toOptionMenu om)

-- @method optionMenuSetMenu@ Set a menu to associate with this option menu.
--
optionMenuSetMenu :: (OptionMenuClass om, MenuClass m) => om -> m -> IO ()
optionMenuSetMenu om m = {#call option_menu_set_menu#}
  (toOptionMenu om) (toWidget m)

-- @method optionMenuRemoveMenu@ Remove the association the menu.
--
optionMenuRemoveMenu :: OptionMenuClass om => om -> IO ()
optionMenuRemoveMenu om = 
  {#call unsafe option_menu_remove_menu#} (toOptionMenu om)

-- @method optionMenuSetHistory@ Set the state of the option menu. The options
-- are numbered from 0 up to n-1 for the nth item.
--
optionMenuSetHistory :: OptionMenuClass om => om -> Int -> IO ()
optionMenuSetHistory om item = {#call option_menu_set_history#}
  (toOptionMenu om) (fromIntegral item)

-- @method optionMenuGetHistory@ Retrieve the index of the selected item.
--
optionMenuGetHistory :: OptionMenuClass om => om -> IO Int
optionMenuGetHistory om = liftM fromIntegral $
  {#call unsafe option_menu_get_history#} (toOptionMenu om)

-- signals

-- @signal connectToOMChanged@ This signal is called if the selected option
-- has changed.
--
onOMChanged, afterOMChanged :: OptionMenuClass om => om -> IO () ->
                               IO (ConnectId om)
onOMChanged = connect_NONE__NONE "changed" False
afterOMChanged = connect_NONE__NONE "changed" True
