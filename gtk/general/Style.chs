-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry Styles@
--
--  Author : Axel Simon
--          
--  Created: 13 February 2003
--
--  Version $Revision: 1.3 $ from $Date: 2003/07/09 22:42:44 $
--
--  Copyright (c) 1999..2003 Axel Simon
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
-- * Customization of widgets.
--
-- @documentation@ ------------------------------------------------------------
--
-- * Styles are attached to widgets and determine how particular parts are
--   drawn and with what color. Thus they are should be seen as mandatory 
--   when one implements a new
--   custom widgets via @ref data DrawingArea@. Although the parameterized
--   drawing function don't have to be used, it is
--   strongly advisable (and more robust)
--   to make use of the predefined graphics contexts for the different
--   states of a widget (retrieved by @ref method widgetGetState@).
--
-- * When creating complicated objects in @ref data DrawingArea@ the predefined
--   graphics contexts and the single font in the canvas 
--   might not be enough to customize the rendering process. 
--   gtk_rc_get_style_by_paths is the solution for this.
--
--
-- @todo@ ---------------------------------------------------------------------
--
-- * It seems sensible to treat Styles as read only. The only way to modify
--   a style should be for the programmer to apply the RcStyle patches directly
--   to the widget.
--
-- * Bind the draw... functions, they might be useful.
--
module Style(
  Style,
  StyleClass,
  styleGetForeground,
  styleGetBackground,
  styleGetLight,
  styleGetMiddle,
  styleGetDark,
  styleGetText,
  styleGetBase,
  styleGetAntiAliasing
  ) where

import Monad		(liftM)
import FFI

{#import GObject#}	(makeNewGObject)
{#import Hierarchy#}
import Enums		(StateType)
import Structs		(styleGetForeground,
			 styleGetBackground,
			 styleGetLight,
			 styleGetMiddle,
			 styleGetDark,
			 styleGetText,
			 styleGetBase,
			 styleGetAntiAliasing)

