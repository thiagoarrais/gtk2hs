-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Box
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.8 $ from $Date: 2004/05/23 15:46:02 $
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
-- |
--
-- This abstract container class is instatiated by using HBox or VBox. It 
-- supplies all methods to add and remove children.
--

module Box(
  Box,
  BoxClass,
  castToBox,
  Packing(..),
  boxPackStart,
  boxPackEnd,
  boxPackStartDefaults,
  boxPackEndDefaults,
  boxSetHomogeneous,
  boxGetSpacing,
  boxSetSpacing,
  boxReorderChild,
  boxQueryChildPacking,
  boxSetChildPacking
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(PackType(..), Packing(..))

{# context lib="gtk" prefix="gtk" #}

-- methods


-- | Insert a widget at the beginning of the box
-- container.
--
-- * The 'Packing' parameter determines how the child behaves in the
--   horizontal or vertical way in an HBox or VBox, respectively.
--   'PackNatural' means the child is as big as it reqests. It will
--   move to the left in an 'HBox' or to the top in an
--   'VBox' if there is more space availble.
--   All children
--   that have choosen 'PackRepel' for @p@ will be padded 
--   on both sides with
--   additional space. 'PackGrow' will increase the size of the
--   so that is covers the available space.
--
boxPackStart :: (BoxClass b, WidgetClass w) => b -> w -> Packing -> Int ->
                IO ()
boxPackStart b w p pad = {#call box_pack_start#} (toBox b) (toWidget w)
  (fromBool $ p/=PackNatural) (fromBool $ p==PackGrow) (fromIntegral pad)

-- | Insert a widget at the end of the box container.
--
-- * See 'boxPackStart'. The option 'Natural' will
--   move a child to the right in an 'HBox' or to the bottom in an
--   'VBox' if there is more space availble.
--
boxPackEnd :: (BoxClass b, WidgetClass w) => b -> w -> Packing -> Int -> IO ()
boxPackEnd b w p pad = {#call box_pack_end#} (toBox b) (toWidget w)
  (fromBool $ p/=PackNatural) (fromBool $ p==PackGrow) (fromIntegral pad)


-- | Like 'boxPackStart' but uses the
-- default parameters 'PackRepel' and 0 for padding.
--
boxPackStartDefaults :: (BoxClass b, WidgetClass w) => b -> w -> IO ()
boxPackStartDefaults b w = 
  {#call box_pack_start_defaults#} (toBox b) (toWidget w)

-- | Like 'boxPackEnd' but uses the
-- default parameters 'PackRepel' and 0 for padding.
--
boxPackEndDefaults :: (BoxClass b, WidgetClass w) => b -> w -> IO ()
boxPackEndDefaults b w = 
  {#call box_pack_end_defaults#} (toBox b) (toWidget w)

-- | Set if all children should be spread homogeneous
-- withing the box.
--
boxSetHomogeneous :: BoxClass b => b -> Bool -> IO ()
boxSetHomogeneous b homo = 
  {#call box_set_homogeneous#} (toBox b) (fromBool homo)

-- | Set the standard spacing between two children.
--
-- * This space is in addition to the padding parameter that is given for each
--   child.
--
boxSetSpacing :: BoxClass b => b -> Int -> IO ()
boxSetSpacing b spacing =
  {#call box_set_spacing#} (toBox b) (fromIntegral spacing)

-- | Move @child@ to a new @position@
-- (counted from 0) in the box.
--
boxReorderChild :: (BoxClass b, WidgetClass w) => b -> w -> Int -> IO ()
boxReorderChild b w position = 
  {#call box_reorder_child#} (toBox b) (toWidget w) (fromIntegral position)

-- | Query the packing parameter of a child.
--
-- * Returns information on the behaviour if free space is available 
-- (in 'Packing'), the additional padding for this widget and
-- if the widget
-- was inserted at the start or end of the container ('PackType').
--
boxQueryChildPacking :: (BoxClass b, WidgetClass w) => b -> w ->
                        IO (Packing,Int,PackType)
boxQueryChildPacking b w = alloca $ \expandPtr -> alloca $ \fillPtr ->
  alloca $ \paddingPtr -> alloca $ \packPtr -> do
    {#call unsafe box_query_child_packing#} (toBox b) (toWidget w)
      expandPtr fillPtr paddingPtr packPtr
    expand  <- liftM toBool $ peek expandPtr
    fill    <- liftM toBool $ peek fillPtr
    padding <- liftM fromIntegral $ peek paddingPtr
    pack    <- liftM (toEnum.fromIntegral) $ peek packPtr
    return (if fill then PackGrow else 
            (if expand then PackRepel else PackNatural),
	    padding,pack)

-- | Set the packing parameter of a child.
--
boxSetChildPacking :: (BoxClass b, WidgetClass w) => b -> w -> Packing ->
                      Int -> PackType -> IO ()
boxSetChildPacking b w pack pad pt = {#call box_set_child_packing#} (toBox b) 
  (toWidget w) (fromBool $ pack/=PackNatural) (fromBool $ pack==PackGrow) 
  (fromIntegral pad) ((fromIntegral.fromEnum) pt)


-- | Retrieves the standard spacing between widgets.
--
boxGetSpacing :: BoxClass b => b -> IO Int
boxGetSpacing b = 
  liftM fromIntegral $ {#call unsafe box_get_spacing#} (toBox b)




