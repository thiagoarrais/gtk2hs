--  GIMP Toolkit (GTK) FFI extras and version dependencies
--
--  Author : Axel Simon
--
--  Created: 22 June 2001
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
-- #hide

-- |
--
-- This module serves as an impedance matcher for different compiler
-- versions. It also adds a few FFI utility functions.
--

module System.Glib.FFI (
  with,
  nullForeignPtr,
  maybeNull,
  newForeignPtr,
  withForeignPtrs,
  module Foreign,
  module Foreign.C
  ) where

import System.IO.Unsafe (unsafePerformIO)

-- We should almost certainly not be using the standard free function anywhere
-- in the glib or gtk bindings, so we do not re-export it from this module.

import Foreign.C
import qualified Foreign hiding (free)
import Foreign  hiding	(with, newForeignPtr, free)

with :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
with = Foreign.with

newForeignPtr = flip Foreign.newForeignPtr

nullForeignPtr :: ForeignPtr a
nullForeignPtr = unsafePerformIO $ newForeignPtr_ nullPtr

-- This is useful when it comes to marshaling lists of GObjects
--
withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs fptrs body = do
  result <- body (map unsafeForeignPtrToPtr fptrs)
  mapM_ touchForeignPtr fptrs
  return result

-- A marshaling utility function that is used by the code produced by the code
-- generator to marshal return values that can be null
maybeNull :: (IO (Ptr a) -> IO a) -> IO (Ptr a) -> IO (Maybe a)
maybeNull marshal genPtr = do
  ptr <- genPtr
  if ptr == nullPtr
    then return Nothing
    else do result <- marshal (return ptr)
            return (Just result)
