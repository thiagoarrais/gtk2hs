-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Button
--
--  Author : Axel Simon
--
--  Created: 15 May 2001
--
--  Copyright (C) 1999-2005 Axel Simon
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
-- A widget that creates a signal when clicked on
--
module Graphics.UI.Gtk.VteLite (

-- * Types
  VteTerminal,
--  VteLiteClass,
--  castToVteLite,
--  toVteLite,

-- * Constructors
  vteLiteNew,

-- * Methods
  vteLiteSetFontFromString,
  vteLiteSetMouseAutoHide,
  vteLiteBeginAppOutput,
  vteLiteFinishAppOutput,
--  vteLiteFeed,

-- * Signals
--  onLineReceived
  ) where

import Control.Monad	(liftM)
import Foreign.ForeignPtr
import Foreign.Ptr

import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
import System.Glib.FFI
import System.Glib.UTFString

{#pointer *VteTerminal foreign newtype #}

class WidgetClass o => VteLiteClass o
toVteLite :: VteLiteClass o => o -> VteTerminal
toVteLite = unsafeCastGObject . toGObject

mkVteLite = VteTerminal
unVteLite (VteTerminal o) = o

instance VteLiteClass VteTerminal
instance ObjectClass VteTerminal
instance WidgetClass VteTerminal
instance GObjectClass VteTerminal where
  toGObject = mkGObject . castForeignPtr . unVteLite
  unsafeCastGObject = mkVteLite . castForeignPtr . unGObject

vteLiteNew :: IO VteTerminal
vteLiteNew =
  makeNewObject mkVteLite $
  liftM (castPtr :: Ptr Widget -> Ptr VteTerminal) $
  {# call unsafe vte_terminal_new #}

vteLiteSetFontFromString terminal font =
  withUTFString font $ \fontPtr ->
  {# call vte_terminal_set_font_from_string #}
    (toVteLite terminal)
    fontPtr

vteLiteSetMouseAutoHide terminal status =
  {# call vte_terminal_set_mouse_autohide #}
    (toVteLite terminal)
    (fromBool status)

vteLiteBeginAppOutput terminal =
  {# call vte_terminal_begin_app_output #}
    (toVteLite terminal)

vteLiteFinishAppOutput terminal =
  {# call vte_terminal_finish_app_output #}
    (toVteLite terminal)

