-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceView
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--
--  Created: 14 October 2003
--
--  Copyright (C) 2003-2005 Duncan Coutts, Axel Simon
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
module Graphics.UI.Gtk.SourceView (
  module Graphics.UI.Gtk.SourceView.SourceView,
  module Graphics.UI.Gtk.SourceView.SourceBuffer,
  module Graphics.UI.Gtk.SourceView.SourceLanguage,
  module Graphics.UI.Gtk.SourceView.SourceLanguagesManager,
  module Graphics.UI.Gtk.SourceView.SourceTag,
  module Graphics.UI.Gtk.SourceView.SourceTagTable,
  module Graphics.UI.Gtk.SourceView.SourceTagStyle,
  module Graphics.UI.Gtk.SourceView.SourceStyleScheme,
  module Graphics.UI.Gtk.SourceView.SourceIter
) where

import Graphics.UI.Gtk.SourceView.SourceView
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Graphics.UI.Gtk.SourceView.SourceLanguage
import Graphics.UI.Gtk.SourceView.SourceLanguagesManager
import Graphics.UI.Gtk.SourceView.SourceStyleScheme
import Graphics.UI.Gtk.SourceView.SourceTag
import Graphics.UI.Gtk.SourceView.SourceTagTable
import Graphics.UI.Gtk.SourceView.SourceTagStyle
import Graphics.UI.Gtk.SourceView.SourceIter
