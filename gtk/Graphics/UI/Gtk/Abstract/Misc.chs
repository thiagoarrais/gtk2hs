-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Misc
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--
--  Created: 2 May 2001
--
--  Version $Revision: 1.2 $ from $Date: 2005/02/12 17:19:21 $
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
-- a base class for widgets with alignments and padding.
--
module Graphics.UI.Gtk.Abstract.Misc (
  Misc,
  MiscClass,
  castToMisc,
  miscSetAlignment,
  miscGetAlignment,
  miscSetPadding,
  miscGetPadding
  ) where

import Monad	(liftM)
import System.Glib.FFI


{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}

{# context lib="gtk" prefix="gtk" #}

-- Misc type declaration

-- methods

-- | Set the alignment of the widget.
--
miscSetAlignment :: MiscClass m => m -> Double -> Double -> IO ()
miscSetAlignment misc xalign yalign =  {#call misc_set_alignment#} 
  (toMisc misc) (realToFrac xalign) (realToFrac yalign) 
    
-- | Get the alignment of the widget.
--
miscGetAlignment :: MiscClass m => m -> IO (Double, Double)
miscGetAlignment misc = 
  alloca $ \xalignPtr -> alloca $ \yalignPtr -> do
  {#call unsafe misc_get_alignment#} (toMisc misc) xalignPtr yalignPtr
  xalign <- peek xalignPtr
  yalign <- peek yalignPtr
  return (realToFrac xalign, realToFrac yalign)

-- | Set the amount of space to add around the widget.
--
miscSetPadding :: MiscClass m => m -> Int -> Int -> IO ()
miscSetPadding misc xpad ypad = {#call misc_set_padding#} 
  (toMisc misc) (fromIntegral xpad) (fromIntegral ypad) 

-- | Get the amount of space added around the widget.
--
miscGetPadding :: MiscClass m => m -> IO (Int, Int)
miscGetPadding misc =
  alloca $ \xpadPtr -> alloca $ \ypadPtr -> do
  {#call unsafe misc_get_padding#} (toMisc misc) xpadPtr ypadPtr
  xpad <- peek xpadPtr
  ypad <- peek ypadPtr
  return (fromIntegral xpad, fromIntegral ypad)

