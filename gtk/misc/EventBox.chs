-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget EventBox
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.5 $ from $Date: 2004/05/23 16:07:53 $
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
-- This container can be used to receive 'Event's for a widget
-- that has no window on its own.
--
-- TODO
--
-- * check: Is this widget useful?
--
module EventBox(
  EventBox,
  EventBoxClass,
  castToEventBox,
  eventBoxNew
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new 'EventBox'.
--
eventBoxNew :: IO EventBox
eventBoxNew  = makeNewObject mkEventBox $ 
  liftM castPtr {#call unsafe event_box_new#}

