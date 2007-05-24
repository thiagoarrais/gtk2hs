-- -*-haskell-*-
--  GIMP Toolkit (GTK) GValueTypes
--
--  Author : Axel Simon
--
--  Created: 1 June 2001
--
--  Version $Revision: 1.5 $ from $Date: 2005/07/02 19:22:03 $
--
--  Copyright (c) 1999..2002 Axel Simon
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
-- This is used by the implementation of properties and by the 
-- 'Graphics.UI.Gtk.TreeList.TreeModel' and
-- related modules.
--
module System.Glib.GValueTypes (
  valueSetUInt,
  valueGetUInt,
  valueSetInt,
  valueGetInt,
--  valueSetUChar,
--  valueGetUChar,
--  valueSetChar,
--  valueGetChar,
  valueSetBool,
  valueGetBool,
  valueSetPointer,
  valueGetPointer,
  valueSetFloat,
  valueGetFloat,
  valueSetDouble,
  valueGetDouble,
  valueSetEnum,
  valueGetEnum,
  valueSetFlags,
  valueGetFlags,
  valueSetString,
  valueGetString,
  valueSetMaybeString,
  valueGetMaybeString,
  valueSetBoxed,
  valueGetBoxed,
  valueSetGObject,
  valueGetGObject,
  ) where

import Control.Monad	(liftM)

import System.Glib.FFI
import System.Glib.Flags
import System.Glib.UTFString
{#import System.Glib.GValue#}		(GValue(GValue))
import System.Glib.GObject

{# context lib="glib" prefix="g" #}

valueSetUInt :: GValue -> Word -> IO ()
valueSetUInt gvalue value =
  {# call unsafe value_set_uint #} gvalue (fromIntegral value)

valueGetUInt :: GValue -> IO Word
valueGetUInt gvalue =
  liftM fromIntegral $
  {# call unsafe value_get_uint #} gvalue

valueSetInt :: GValue -> Int -> IO ()
valueSetInt gvalue value =
  {# call unsafe value_set_int #} gvalue (fromIntegral value)

valueGetInt :: GValue -> IO Int
valueGetInt gvalue =
  liftM fromIntegral $
  {# call unsafe value_get_int #} gvalue

{-
valueSetUChar :: GValue -> Word8 -> IO ()
valueSetUChar gvalue value =
  {# call unsafe value_set_uchar #} gvalue value

valueGetUChar :: GValue -> IO Word8
valueGetUChar gvalue =
  {# call unsafe value_get_uchar #} gvalue

valueSetChar :: GValue -> {#type gchar#} -> IO ()
valueSetChar gvalue value =
  {# call unsafe value_set_char #} gvalue value

valueGetChar :: GValue -> IO {#type gchar#}
valueGetChar gvalue =
  {# call unsafe value_get_char #} gvalue
-}

valueSetBool :: GValue -> Bool -> IO ()
valueSetBool gvalue value =
  {# call unsafe value_set_boolean #} gvalue (fromBool value)

valueGetBool :: GValue -> IO Bool
valueGetBool gvalue =
  liftM toBool $
  {# call  unsafe value_get_boolean #} gvalue

-- These functions should probably never be used as they are dangerous.
--
valueSetPointer :: GValue -> (Ptr ()) -> IO ()
valueSetPointer gvalue value =
  {# call unsafe value_set_pointer #} gvalue value

valueGetPointer :: GValue -> IO (Ptr ())
valueGetPointer gvalue =
  {# call unsafe value_get_pointer #} gvalue

valueSetFloat :: GValue -> Float -> IO ()
valueSetFloat gvalue value =
  {# call unsafe value_set_float #} gvalue (realToFrac value)

valueGetFloat :: GValue -> IO Float
valueGetFloat gvalue =
  liftM realToFrac $
  {# call unsafe value_get_float #} gvalue

valueSetDouble :: GValue -> Double -> IO ()
valueSetDouble gvalue value =
  {# call unsafe value_set_double #} gvalue (realToFrac value)

valueGetDouble :: GValue -> IO Double
valueGetDouble gvalue =
  liftM realToFrac $
  {# call unsafe value_get_double #} gvalue

valueSetEnum :: Enum enum => GValue -> enum -> IO ()
valueSetEnum gvalue value =
  {# call unsafe value_set_enum #} gvalue (fromIntegral $ fromEnum value)

valueGetEnum :: Enum enum => GValue -> IO enum
valueGetEnum gvalue =
  liftM (toEnum . fromIntegral) $
  {# call unsafe value_get_enum #} gvalue

valueSetFlags :: Flags flag => GValue -> [flag] -> IO ()
valueSetFlags gvalue value =
  {# call unsafe value_set_flags #} gvalue (fromIntegral $ fromFlags value)

valueGetFlags :: Flags flag => GValue -> IO [flag]
valueGetFlags gvalue =
  liftM (toFlags . fromIntegral) $
  {# call unsafe value_get_flags #} gvalue

valueSetString :: GValue -> String -> IO ()
valueSetString gvalue str =
  withUTFString str $ \strPtr ->
  {# call unsafe value_set_string #} gvalue strPtr

valueGetString :: GValue -> IO String
valueGetString gvalue = do
  strPtr <- {# call unsafe value_get_string #} gvalue
  if strPtr == nullPtr
    then return ""
    else peekUTFString strPtr

valueSetMaybeString :: GValue -> Maybe String -> IO ()
valueSetMaybeString gvalue (Just str) =
  withUTFString str $ \strPtr ->
  {# call unsafe value_set_string #} gvalue strPtr

valueSetMaybeString gvalue Nothing =
  {# call unsafe value_set_static_string #} gvalue nullPtr

valueGetMaybeString :: GValue -> IO (Maybe String)
valueGetMaybeString gvalue =
  {# call unsafe value_get_string #} gvalue
  >>= maybePeek peekUTFString

valueSetBoxed :: (boxed -> (Ptr boxed -> IO ()) -> IO ()) -> GValue -> boxed -> IO ()
valueSetBoxed with gvalue boxed =
  with boxed $ \boxedPtr -> do
  {# call unsafe g_value_set_boxed #} gvalue (castPtr boxedPtr)

valueGetBoxed :: (Ptr boxed -> IO boxed) -> GValue -> IO boxed
valueGetBoxed peek gvalue =
  {# call unsafe g_value_get_boxed #} gvalue >>= peek . castPtr

-- for some weird reason the API says that gv is a gpointer, not a GObject
--
valueSetGObject :: GObjectClass gobj => GValue -> gobj -> IO ()
valueSetGObject gvalue obj =
  withForeignPtr ((unGObject.toGObject) obj) $ \objPtr ->
    {# call unsafe g_value_set_object #} gvalue (castPtr objPtr)

-- Unsafe because it performs an unchecked downcast. Only for internal use.
--
valueGetGObject :: GObjectClass gobj => GValue -> IO gobj
valueGetGObject gvalue =
  liftM fromGObject $
  makeNewGObject mkGObject $
  throwIfNull "GValue.valueGetObject: extracting invalid object" $
  liftM castPtr $
  {# call unsafe value_get_object #} gvalue
