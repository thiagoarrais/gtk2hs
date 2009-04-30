-- -*-haskell-*-
--  GIMP Toolkit (GTK) Console Widget
--
--  Author: Thiago Arrais
--
--  Created: 16 Feb 2009
--
--  Copyright (C) 2009 Thiago Arrais
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
-- Stability   : experimental
-- Portability : portable (depends on GHC)
--
-- A terminal-like widget
--
module Graphics.UI.Gtk.Console (

-- * Types
  Console,
  ConsoleClass,

-- * Constructors
  consoleNew,

-- * Methods
  consoleSetFontFromString,
  consoleSetMouseAutoHide,
  consoleBeginAppOutput,
  consoleFinishAppOutput,
  consoleSetCommandPrompt,
  consoleFeed,

-- * Signals
  onLineReceived
  ) where

import Control.Monad	(liftM)
import Foreign.ForeignPtr
import Foreign.Ptr

import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import System.Glib.FFI
import System.Glib.UTFString

{#pointer *Console foreign newtype #}

class WidgetClass o => ConsoleClass o
toConsole :: ConsoleClass o => o -> Console
toConsole = unsafeCastGObject . toGObject

mkConsole = Console
unConsole (Console o) = o

instance ConsoleClass Console
instance ObjectClass Console
instance WidgetClass Console
instance GObjectClass Console where
  toGObject = mkGObject . castForeignPtr . unConsole
  unsafeCastGObject = mkConsole . castForeignPtr . unGObject

consoleNew :: IO Console
consoleNew =
  makeNewObject mkConsole $
  liftM (castPtr :: Ptr Widget -> Ptr Console) $
  {# call unsafe console_console_new #}

consoleSetFontFromString terminal font =
  withUTFString font $ \fontPtr ->
  {# call console_console_set_font_from_string #}
    (toConsole terminal)
    fontPtr

consoleSetMouseAutoHide terminal status =
  {# call console_console_set_mouse_autohide #}
    (toConsole terminal)
    (fromBool status)

consoleBeginAppOutput terminal =
  {# call console_console_begin_app_output #}
    (toConsole terminal)

consoleFinishAppOutput terminal =
  {# call console_console_finish_app_output #}
    (toConsole terminal)

consoleSetCommandPrompt terminal text =
  withUTFString text $ \textPtr ->
  {# call console_console_set_command_prompt #}
    (toConsole terminal)
    textPtr

consoleFeed terminal input =
  withUTFString input $ \inputPtr ->
  {# call console_console_feed #}
    (toConsole terminal)
    inputPtr
    (fromIntegral (length input))

onLineReceived :: ConsoleClass t => t
  -> (String -> IO ())
  -> IO (ConnectId t)
onLineReceived = connect_STRING__NONE "line-received" False

