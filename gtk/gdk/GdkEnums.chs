--  -*-haskell-*-
--  GIMP Toolkit (GTK) Enumerations
--
--  Author : Manuel M. T. Chakravarty, Axel Simon
--  Created: 13 Januar 1999
--
--  Version $Revision: 1.7 $ from $Date: 2004/05/23 15:55:36 $
--
--  Copyright (c) [1999..2001] Manuel M. T. Chakravarty
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
-- General enumeration types.
--
-- TODO
--
--  * Documentation
--
module GdkEnums(
  CapStyle(..),
  CrossingMode(..),
  Dither(..),
  EventMask(..),
  ExtensionMode(..),
  Fill(..),
  FillRule(..),
  Function(..),
  InputCondition(..),
  JoinStyle(..),
  LineStyle(..),
  NotifyType(..),
  OverlapType(..),
  ScrollDirection(..),
  SubwindowMode(..),
  VisibilityState(..),
  WindowState(..),
  Flags(fromFlags,toFlags)
  ) where

import LocalData((.|.))

class  (Enum a, Bounded a) => Flags a where
  fromFlags ::  [a] -> Int
  toFlags   ::  Int -> [a]

  fromFlags is = orNum 0 is
    where
      orNum n []     = n
      orNum n (i:is) = orNum (n .|. fromEnum i) is
  toFlags n = andNum n minBound
    where
      andNum n (m::a) = (if (n .|. fromEnum m) == n then (m:) else id)
        (if fromEnum m==fromEnum (maxBound::a) then [] else andNum n (succ m))

{#context lib="libgdk" prefix ="gdk"#}

-- | Specify the how the ends of a line is drawn.
--
{#enum CapStyle {underscoreToCase}#}

-- | provide additionl information if cursor crosses a
-- window
--
{#enum CrossingMode {underscoreToCase}#}

-- | Specify how to dither colors onto the screen.
--
{#enum RgbDither as Dither {underscoreToCase}#}

-- | specify which events a widget will emit signals on
--
{#enum EventMask {underscoreToCase} deriving (Bounded)#}

instance Flags EventMask

-- | specify which input extension a widget desires
--
{#enum ExtensionMode {underscoreToCase} deriving(Bounded)#}

instance Flags ExtensionMode

-- | How objects are filled.
--
{#enum Fill {underscoreToCase}#}

-- | Determine how bitmap operations are carried out.
--
{#enum Function {underscoreToCase}#}

-- | Specify how to interpret a polygon.
--
-- * The flag determines what happens if a polygon has overlapping areas.
--
{#enum FillRule {underscoreToCase}#}

-- | Specify on what file condition a callback should be
-- done.
--
{#enum InputCondition {underscoreToCase} deriving(Bounded) #}

instance Flags InputCondition

-- | Determines how adjacent line ends are drawn.
--
{#enum JoinStyle {underscoreToCase}#}

-- | Determines if a line is solid or dashed.
--
{#enum LineStyle {underscoreToCase}#}

-- dunno
--
{#enum NotifyType {underscoreToCase}#}

-- | How a rectangle is contained in a 'Region'.
--
{#enum OverlapType {underscoreToCase}#}

-- | in which direction was scrolled?
--
{#enum ScrollDirection {underscoreToCase}#}

-- | Determine if child widget may be overdrawn.
--
{#enum SubwindowMode {underscoreToCase}#}

-- | visibility of a window
--
{#enum VisibilityState {underscoreToCase,
			VISIBILITY_PARTIAL as VisibilityPartialObscured}#}

-- | the state a GDK window is in
--
{#enum WindowState {underscoreToCase} deriving (Bounded)#}

instance Flags WindowState
