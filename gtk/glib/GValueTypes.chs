-- -*-haskell-*-
--  GIMP Toolkit (GTK) @entry GValueTypes (record access)@
--
--  Author : Axel Simon
--          
--  Created: 1 June 2001
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
-- * This module implements only the necessities for the GTK binding.
--
--- DOCU ----------------------------------------------------------------------
--
-- * Everything here is only used by @TreeStore and friends.
--
--- TODO ----------------------------------------------------------------------
--
-- * Replace POINTER with Stable Dynamic or something safe and Haskell like.
--
module GValueTypes(
  valueSetUInt,
  valueGetUInt,
  valueSetInt,
  valueGetInt,
  valueSetUChar,
  valueGetUChar,
  valueSetChar,
  valueGetChar,
  valueSetBoolean,
  valueGetBoolean,
  valueSetPointer,
  valueGetPointer,
  valueSetFloat,
  valueGetFloat,
  valueSetDouble,
  valueGetDouble,
  valueSetString,
  valueGetString,
  valueSetObject,
  valueGetObject
  ) where

import Monad	(liftM)
import Foreign
import UTFCForeign
import GObject
{#import Hierarchy#}
import GType	(GType)
{#import GValue#}	(GValue, GenericValue(..))

{# context lib="glib" prefix="g" #}

-- Retrieve and set the data item in the @GenericValue.
--

valueSetUInt :: GValue -> {#type guint#} -> IO ()
valueSetUInt = {#call unsafe value_set_uint#}

valueGetUInt :: GValue -> IO {#type guint#}
valueGetUInt = {#call unsafe value_get_uint#}

valueSetInt :: GValue -> {#type gint#} -> IO ()
valueSetInt = {#call unsafe value_set_int#}

valueGetInt :: GValue -> IO {#type gint#}
valueGetInt = {#call unsafe value_get_int#}

valueSetUChar :: GValue -> {#type guchar#} -> IO ()
valueSetUChar = {#call unsafe value_set_uchar#}

valueGetUChar :: GValue -> IO {#type guchar#}
valueGetUChar = {#call unsafe value_get_uchar#}

valueSetChar :: GValue -> {#type gchar#} -> IO ()
valueSetChar = {#call unsafe value_set_char#}

valueGetChar :: GValue -> IO {#type gchar#}
valueGetChar = {#call unsafe value_get_char#}

valueSetBoolean :: GValue -> Bool -> IO ()
valueSetBoolean gv b = {#call unsafe value_set_boolean#} gv (fromBool b)

valueGetBoolean :: GValue -> IO Bool
valueGetBoolean gv = liftM toBool $ {#call unsafe value_get_boolean#} gv

-- These functions should probably never be used as they are dangerous.
--
valueSetPointer :: GValue -> (Ptr ()) -> IO ()
valueSetPointer = {#call unsafe value_set_pointer#}

valueGetPointer :: GValue -> IO (Ptr ())
valueGetPointer = {#call unsafe value_get_pointer#}

valueSetFloat :: GValue -> Float -> IO ()
valueSetFloat gv f = {#call unsafe value_set_float#} gv (realToFrac f)

valueGetFloat :: GValue -> IO Float
valueGetFloat gv = liftM realToFrac $ {#call unsafe value_get_float#} gv

valueSetDouble :: GValue -> Double -> IO ()
valueSetDouble gv d= {#call unsafe value_set_double#} gv (realToFrac d)

valueGetDouble :: GValue -> IO Double
valueGetDouble gv = liftM realToFrac $ {#call unsafe value_get_double#} gv

valueSetString :: GValue -> String -> IO ()
valueSetString gv str = do
  strPtr <- newCString str
  {#call unsafe value_set_static_string#} gv strPtr

valueGetString :: GValue -> IO String
valueGetString gv = do
  strPtr <- {#call unsafe value_get_string#} gv
  if strPtr==nullPtr then return "" else peekCString strPtr

-- * for some weird reason the API sais that @gv is a gpointer, not a GObject
valueSetObject :: GValue -> GObject -> IO ()
valueSetObject gv obj = g_value_set_object gv obj

foreign import ccall "g_value_set_object" unsafe
  g_value_set_object :: GValue -> GObject -> IO ()

valueGetObject :: GValue -> IO GObject
valueGetObject gv = makeNewGObject mkGObject $
  throwIfNull "GType.valueGetObject: extracting invalid object" $
  liftM castPtr $ {#call unsafe value_get_object#} gv

