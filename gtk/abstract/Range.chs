-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Range
--
--  Author : Axel Simon
--          
--  Created: 23 May 2001
--
--  Version $Revision: 1.6 $ from $Date: 2004/08/01 16:08:14 $
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
-- An abstract base class to handle widgets that represent some value range.
--

module Range(
  Range,
  RangeClass,
  castToRange,
  rangeGetAdjustment,
  UpdateType(..),
  rangeSetUpdatePolicy,
  rangeGetUpdatePolicy,
  rangeSetAdjustment,
  rangeGetInverted,
  rangeSetInverted,
  ScrollType(..),
  rangeSetIncrements,
  rangeSetRange,
  rangeSetValue,
  rangeGetValue,
  onMoveSlider,
  afterMoveSlider
  ) where

import Monad	(liftM)
import FFI

import Object	(makeNewObject)
{#import Hierarchy#}
{#import Signal#}
import Enums	(UpdateType(..), ScrollType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Extract the 'Adjustment' object.
--
rangeGetAdjustment :: RangeClass r => r -> IO Adjustment
rangeGetAdjustment r = makeNewObject mkAdjustment $
  {#call unsafe range_get_adjustment#} (toRange r)

-- | Set how the internal 'Adjustment' object is updated.
--
rangeSetUpdatePolicy :: RangeClass r => r -> UpdateType -> IO ()
rangeSetUpdatePolicy r up = {#call range_set_update_policy#}
  (toRange r) ((fromIntegral.fromEnum) up)

-- | Get the update policy for the range widget.
--
rangeGetUpdatePolicy :: RangeClass r => r -> IO UpdateType
rangeGetUpdatePolicy r = liftM (toEnum.fromIntegral) $
  {#call unsafe range_get_update_policy#} (toRange r)

-- | Insert a new 'Adjustment' object.
--
rangeSetAdjustment :: RangeClass r => r -> Adjustment -> IO ()
rangeSetAdjustment r adj = {#call range_set_adjustment#} (toRange r) adj

-- | Get the inverted flag (determines if the range is reversed).
--
rangeGetInverted :: RangeClass r => r -> IO Bool
rangeGetInverted r = 
  liftM toBool $ {#call unsafe range_get_inverted#} (toRange r)

-- | Set the inverted flag.
--
rangeSetInverted :: RangeClass r => r -> Bool -> IO ()
rangeSetInverted r inv = {#call range_set_inverted#} (toRange r) (fromBool inv)

-- | Sets the step and page sizes for the range. The step size is used when the
-- user clicks the "Scrollbar" arrows or moves "Scale" via arrow keys. The
-- page size is used for example when moving via Page Up or Page Down keys.
--
rangeSetIncrements :: RangeClass r => r
                   -> Double  -- ^ step size
                   -> Double  -- ^ page size
                   -> IO ()
rangeSetIncrements r step page =
 {#call range_set_increments#} (toRange r) (realToFrac step) (realToFrac page)

-- | Sets the allowable values in the 'Range', and clamps the range value to be
-- between min and max.
--
rangeSetRange :: RangeClass r => r
              -> Double  -- ^ min
              -> Double  -- ^ max
              -> IO ()
rangeSetRange r min max =
 {#call range_set_range#} (toRange r) (realToFrac min) (realToFrac max)

-- | Sets the current value of the range. The range emits the \"value_changed\"
-- signal if the value changes.
--
-- * If the value is outside the minimum or maximum range values, it will be
-- clamped to fit inside them.
--
rangeSetValue :: RangeClass r => r -> Double -> IO ()
rangeSetValue r value =
  {#call range_set_value#} (toRange r) (realToFrac value)

-- | Gets the current value of the range.
--
rangeGetValue :: RangeClass r => r -> IO Double
rangeGetValue r = liftM realToFrac $
  {#call unsafe range_get_value#} (toRange r)

-- signals

-- | The slide has moved. The arguments give
-- detailed information what happend.
--
onMoveSlider, afterMoveSlider :: RangeClass r => r -> (ScrollType -> IO ()) ->
                                 IO (ConnectId r)
onMoveSlider = connect_ENUM__NONE "move-slider" False
afterMoveSlider = connect_ENUM__NONE "move-slider" True
