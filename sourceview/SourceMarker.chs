-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry SourceMarker@
--
--  Author : Duncan Coutts
--  derived from GtkTextView bindings by Axel Simon
--          
--  Created: 26 October 2003
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
--
--
module SourceMarker (
  SourceMarker,
  sourceMarkerSetMarkerType,
  sourceMarkerGetMarkerType,
  sourceMarkerGetLine,
  sourceMarkerGetName,
  sourceMarkerGetBuffer,
  sourceMarkerNext,
  sourceMarkerPrev
) where

import Monad	(liftM)
import FFI
import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import SourceViewType#}

{# context lib="gtk" prefix="gtk" #}

-- methods

-- @method sourceMarkerSetMarkerType@
-- 
sourceMarkerSetMarkerType :: SourceMarker -> String -> IO ()
sourceMarkerSetMarkerType mark markType =
  withCString markType $ \strPtr1 ->
  {#call unsafe source_marker_set_marker_type#} mark strPtr1

-- @method sourceMarkerGetMarkerType@
-- 
sourceMarkerGetMarkerType :: SourceMarker -> IO String
sourceMarkerGetMarkerType mark = do
  strPtr <- {#call unsafe source_marker_get_marker_type#} mark
  markType <- peekUTFString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return markType

-- @method sourceMarkerGetLine@
-- 
sourceMarkerGetLine :: SourceMarker -> IO Int
sourceMarkerGetLine mark = liftM fromIntegral $
  {#call unsafe source_marker_get_line#} mark

-- @method sourceMarkerGetName@
-- 
sourceMarkerGetName :: SourceMarker -> IO String
sourceMarkerGetName mark =
  {#call unsafe source_marker_get_name#} mark >>= peekUTFString

-- @method sourceMarkerGetBuffer@
-- 
sourceMarkerGetBuffer :: SourceMarker -> IO SourceBuffer
sourceMarkerGetBuffer mark = makeNewGObject mkSourceBuffer $
  {#call unsafe source_marker_get_buffer#} mark

-- @method sourceMarkerNext@
-- 
sourceMarkerNext :: SourceMarker -> IO SourceMarker
sourceMarkerNext mark = makeNewGObject mkSourceMarker $
  {#call unsafe source_marker_next#} mark

-- @method sourceMarkerPrev@
-- 
sourceMarkerPrev :: SourceMarker -> IO SourceMarker
sourceMarkerPrev mark = makeNewGObject mkSourceMarker $
  {#call unsafe source_marker_prev#} mark
