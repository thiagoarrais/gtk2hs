-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget Frame
--
--  Author : Axel Simon
--          
--  Created: 15 May 2001
--
--  Version $Revision: 1.1 $ from $Date: 2005/01/08 15:30:19 $
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
-- This container adds a frame around its contents. This is useful to
-- logically separate items in a dialog box.
--

module Graphics.UI.Gtk.Ornaments.Frame (
  Frame,
  FrameClass,
  castToFrame,
  frameNew,
  frameSetLabel,
  frameGetLabel,
  frameSetLabelWidget,
  frameGetLabelWidget,
  frameSetLabelAlign,
  frameGetLabelAlign,
  ShadowType(..),
  frameSetShadowType,
  frameGetShadowType
  ) where

import Monad	(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.General.Enums	(ShadowType(..))

{# context lib="gtk" prefix="gtk" #}

-- methods

-- | Create a new frame without a label.
--
-- * A label can later be set by calling 'frameSetLabel'.
--
frameNew :: IO Frame
frameNew  = makeNewObject mkFrame $
  liftM castPtr $ {#call unsafe frame_new#} nullPtr

-- | Replace the label of the frame.
--
frameSetLabel :: FrameClass f => f -> String -> IO ()
frameSetLabel f label = withUTFString label $ \strPtr ->
  {#call frame_set_label#} (toFrame f) strPtr

-- | Replace the label with a (label) widget.
--
frameSetLabelWidget :: (FrameClass f, WidgetClass w) => f -> w -> IO ()
frameSetLabelWidget f w = 
  {#call frame_set_label_widget#} (toFrame f) (toWidget w)

-- | Get the label widget for the frame.
--
frameGetLabelWidget :: FrameClass f => f -> IO (Maybe Widget)
frameGetLabelWidget f = do
  widgetPtr <- {#call frame_get_label_widget#} (toFrame f)
  if widgetPtr == nullPtr
    then return Nothing
    else liftM Just $ makeNewObject mkWidget (return widgetPtr)

-- | Specify where the label should be placed.
--
-- * A value of 0.0 means left justified (the default), a value of 1.0 means
--   right justified.
--
frameSetLabelAlign :: FrameClass f => f -> Float -> IO ()
frameSetLabelAlign f align =
  {#call frame_set_label_align#} (toFrame f) (realToFrac align) 0.0

-- | Get the label's horazontal alignment.
--
frameGetLabelAlign :: FrameClass f => f -> IO Float
frameGetLabelAlign f =
  alloca $ \alignPtr -> do
  {#call unsafe frame_get_label_align#} (toFrame f) alignPtr nullPtr
  align <- peek alignPtr
  return (realToFrac align)

-- | Set the shadow type of the frame.
--
frameSetShadowType :: FrameClass f => f -> ShadowType -> IO ()
frameSetShadowType f shadow = 
  {#call frame_set_shadow_type#} (toFrame f) ((fromIntegral.fromEnum) shadow)

-- | Set the shadow type of the frame.
--
frameGetShadowType :: FrameClass f => f -> IO ShadowType
frameGetShadowType f = liftM (toEnum.fromIntegral) $
  {#call unsafe frame_get_shadow_type#} (toFrame f)

-- | Retrieve the label of the frame.
--
-- * An exception is thrown if a non-Label widget was set.
--
frameGetLabel :: FrameClass f => f -> IO String
frameGetLabel f = do
  strPtr <- throwIfNull 
    "frameGetLabel: the title of the frame was not a Label widget." $
    {#call unsafe frame_get_label#} (toFrame f)
  res <- peekUTFString strPtr
  {#call unsafe g_free#} (castPtr strPtr)
  return res


   
