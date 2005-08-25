-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget FontSelection
--
--  Author : Duncan Coutts
--
--  Created: 2 August 2004
--
--  Version $Revision: 1.8 $ from $Date: 2005/08/25 01:16:15 $
--
--  Copyright (C) 2004-2005 Duncan Coutts
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- A widget for selecting fonts
--
module Graphics.UI.Gtk.Selectors.FontSelection (
-- * Detail
-- 
-- | The 'FontSelection' widget lists the available fonts, styles and sizes,
-- allowing the user to select a font. It is used in the 'FontSelectionDialog'
-- widget to provide a dialog box for selecting fonts.
--
-- To set the font which is initially selected, use
-- 'fontSelectionSetFontName'.
--
-- To get the selected font use 'fontSelectionGetFont' or
-- 'fontSelectionGetFontName'.
--
-- To change the text which is shown in the preview area, use
-- 'fontSelectionSetPreviewText'.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Box'
-- |                           +----'VBox'
-- |                                 +----FontSelection
-- @

-- * Types
  FontSelection,
  FontSelectionClass,
  castToFontSelection,

-- * Constructors
  fontSelectionNew,

-- * Methods
  fontSelectionGetFontName,
  fontSelectionSetFontName,
  fontSelectionGetPreviewText,
  fontSelectionSetPreviewText,

-- * Attributes
  fontSelectionFontName,
  fontSelectionPreviewText,
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

--------------------
-- Constructors

-- | Creates a new 'FontSelection'.
--
fontSelectionNew :: IO FontSelection
fontSelectionNew =
  makeNewObject mkFontSelection $
  liftM (castPtr :: Ptr Widget -> Ptr FontSelection) $
  {# call unsafe font_selection_new #}

--------------------
-- Methods

-- | Gets the currently-selected font name.
--
fontSelectionGetFontName :: FontSelectionClass self => self
 -> IO (Maybe String) -- ^ returns the name of the currently selected font, or
                      -- @Nothing@ if no font is selected.
fontSelectionGetFontName self =
  {# call unsafe font_selection_get_font_name #}
    (toFontSelection self)
  >>= maybePeek readUTFString

-- | Sets the currently-selected font.
--
fontSelectionSetFontName :: FontSelectionClass self => self
 -> String  -- ^ @fontname@ - a fontname.
 -> IO Bool -- ^ returns @True@ if the font was found.
fontSelectionSetFontName self fontname =
  liftM toBool $
  withUTFString fontname $ \fontnamePtr ->
  {# call font_selection_set_font_name #}
    (toFontSelection self)
    fontnamePtr

-- | Gets the text displayed in the preview area.
--
fontSelectionGetPreviewText :: FontSelectionClass self => self -> IO String
fontSelectionGetPreviewText self =
  {# call unsafe font_selection_get_preview_text #}
    (toFontSelection self)
  >>= peekUTFString

-- | Sets the text displayed in the preview area.
--
fontSelectionSetPreviewText :: FontSelectionClass self => self -> String -> IO ()
fontSelectionSetPreviewText self text =
  withUTFString text $ \textPtr ->
  {# call font_selection_set_preview_text #}
    (toFontSelection self)
    textPtr

--------------------
-- Attributes

-- | The X string that represents this font.
--
-- Default value: \"\"
--
fontSelectionFontName :: FontSelectionClass self => Attr self String
fontSelectionFontName = newAttrFromStringProperty "font_name"

-- | The text to display in order to demonstrate the selected font.
--
-- Default value: \"abcdefghijk ABCDEFGHIJK\"
--
fontSelectionPreviewText :: FontSelectionClass self => Attr self String
fontSelectionPreviewText = newAttr
  fontSelectionGetPreviewText
  fontSelectionSetPreviewText
