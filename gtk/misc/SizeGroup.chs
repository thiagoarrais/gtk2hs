-- -*-haskell-*-
-- GIMP Toolkit (GTK) Widget SizeGroup
--
--  Author : Duncan Coutts
--  Created: 2 August 2004
--
--  Copyright (c) 2004 Duncan Coutts
--  documentation Copyright (c) 1995..2000 the GTK+ Team
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- |
--
-- SizeGroup provides a mechanism for grouping a number of widgets together so
-- they all request the same amount of space. This is typically useful when you
-- want a column of widgets to have the same size, but you can't use a "Table"
-- widget.
--

module SizeGroup (
  sizeGroupNew,
  SizeGroupMode(..),
  sizeGroupSetMode,
  sizeGroupGetMode,
  sizeGroupAddWidget,
  sizeGroupRemoveWidget
  ) where

import Monad	(liftM)
import FFI

import GObject	(makeNewGObject)
{#import Hierarchy#}
{#import Signal#}

{# context lib="gtk" prefix="gtk" #}

{#enum SizeGroupMode {underscoreToCase}#}

-- | Create a new SizeGroup.
--
sizeGroupNew :: SizeGroupMode -> IO SizeGroup
sizeGroupNew mode = makeNewGObject mkSizeGroup $
  {#call unsafe size_group_new#} ((fromIntegral.fromEnum) mode)

-- | Adds a widget to a SizeGroup. In the future, the requisition of the widget
-- will be determined as the maximum of its requisition and the requisition of
-- the other widgets in the size group. Whether this applies horizontally,
-- vertically, or in both directions depends on the mode of the size group. See
-- 'sizeGroupSetMode'.
--
sizeGroupAddWidget :: (SizeGroupClass obj, WidgetClass widget) => obj
                   -> widget -> IO ()
sizeGroupAddWidget obj widget =
  {#call size_group_add_widget#} (toSizeGroup obj) (toWidget widget)

-- | Gets the current mode of the size group.
--
sizeGroupGetMode :: SizeGroupClass obj => obj -> IO SizeGroupMode
sizeGroupGetMode obj = liftM (toEnum.fromIntegral) $
  {#call unsafe size_group_get_mode#} (toSizeGroup obj)

-- | Removes the widget from the SizeGroup.
--
sizeGroupRemoveWidget :: (SizeGroupClass obj, WidgetClass widget) => obj -> widget -> IO ()
sizeGroupRemoveWidget obj widget =
  {#call size_group_remove_widget#} (toSizeGroup obj) (toWidget widget)

-- | Sets the 'SizeGroupMode' of the size group. The mode of the size group
-- determines whether the widgets in the size group should all have the same
-- horizontal requisition 'sizeGroupModeHorizontal' all have the same vertical
-- requisition 'sizeGroupModeVertical', or should all have the same requisition
-- in both directions 'sizeGroupModeBoth'.
--
sizeGroupSetMode :: SizeGroupClass obj => obj -> SizeGroupMode -> IO ()
sizeGroupSetMode obj mode =
  {#call size_group_set_mode#} (toSizeGroup obj) ((fromIntegral.fromEnum) mode)
